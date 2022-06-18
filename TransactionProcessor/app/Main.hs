{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Database.Redis
    ( lpop,
      connect,
      defaultConnectInfo,
      disconnect,
      runRedis,
      ConnectInfo(connectPort),
      Connection,
      PortID(PortNumber) )
import Database.HDBC
    ( toSql, IConnection(run, commit, disconnect) )
import Database.HDBC.ODBC ( connectODBC, Connection )
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy ( fromStrict )
import Data.Aeson ( decode, FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Prelude hiding (id)


-- data types
data Transaction = Transaction
  { id :: Int,
    sender_card_id :: Int,
    receiver_card_id :: Int,
    amount :: Double
  }
  deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON Transaction


-- redit config
myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {connectPort = PortNumber 6379}


-- main pipe
main :: IO ()
main = do
    -- connect to Redis
    connRedis <- connect myConnectInfo
    -- connect to Db
    connDb <- connectODBC "Driver={MySQL};Server=localhost;Port=3089;Database=bank;User=root;Password=admin;Option=3;"

    -- process all pending transactions from redis queue
    processTransactions connDb connRedis

    -- close connections
    Database.HDBC.disconnect connDb
    Database.Redis.disconnect connRedis

    print "transaction processing done"


-- process transactions until redis queue is empty
processTransactions :: Database.HDBC.ODBC.Connection -> Database.Redis.Connection -> IO ()
processTransactions connDb connRedis = do
  -- retrieve oldest pending transaction from redis queue
  transactionString <- fetchTransactionRedis connRedis
  let maybeTransaction = decode (fromStrict transactionString) :: Maybe Transaction

  case maybeTransaction of
    Nothing -> print "no pending transactions"
    Just transaction -> do
      -- process transaction
      processTransaction connDb transaction
      -- recurse
      processTransactions connDb connRedis


-- process transaction
processTransaction :: Database.HDBC.ODBC.Connection -> Transaction -> IO ()
processTransaction connDb transaction = do
  -- update balances
  decrementBalanceDb connDb (sender_card_id transaction) (amount transaction)
  incrementBalanceDb connDb (receiver_card_id transaction) (amount transaction)
  -- update transaction status to complete
  setTransactionStatus connDb (id transaction) 1
  -- commit transaction
  commit connDb
  -- print success message
  print $ "transaction complete: " ++ show (sender_card_id transaction) ++ " -> " ++ show (receiver_card_id transaction) ++ " amount: " ++ show (amount transaction)


-- get transaction bytestring from Redis
fetchTransactionRedis :: Database.Redis.Connection -> IO Data.ByteString.ByteString
fetchTransactionRedis conn = runRedis conn $ do
  result <- lpop "list:transactions"
  case result of
    Right (Just transactionString) -> return transactionString
    _ -> return $ Data.ByteString.Char8.pack "no transactions"


-- update card in database
incrementBalanceDb :: Database.HDBC.ODBC.Connection -> Int -> Double -> IO Integer
incrementBalanceDb conn card_id amount =
  run conn "UPDATE cards SET balance = balance + ? where id=?;" [toSql amount,toSql card_id]

decrementBalanceDb :: Database.HDBC.ODBC.Connection -> Int -> Double -> IO Integer
decrementBalanceDb conn card_id amount =
  run conn "UPDATE cards SET balance = balance - ? where id=?;" [toSql amount,toSql card_id]


-- set transaction status
setTransactionStatus :: Database.HDBC.ODBC.Connection -> Int -> Int -> IO Integer
setTransactionStatus conn transaction_id status =
  run conn "UPDATE transactions SET status = ? where id=?;" [toSql status,toSql transaction_id]
