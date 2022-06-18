{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Database.Redis hiding (decode)
import Database.HDBC
import Database.HDBC.ODBC
import Data.ByteString
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy
import Data.Text.Encoding
import System.Exit
import Data.Either
import Data.Aeson
import Control.Monad.Trans
import Data.Maybe
import GHC.Generics
import Data.Void


-- data types
data Transaction = Transaction
  { sender_card_id :: Int,
    receiver_card_id :: Int,
    amount :: Double
  }
  deriving (Show, Generic)


-- retrieval functions
getSender :: Transaction -> Int
getSender (Transaction {sender_card_id = sender}) = sender

getReceiver :: Transaction -> Int
getReceiver (Transaction {receiver_card_id = receiver}) = receiver

getAmount :: Transaction -> Double
getAmount (Transaction {amount = amount}) = amount

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
    -- connDb <- connectODBC "DSN=mysql://root:admin@localhost:3089/bank"

    -- get oldest pending transaction
    transactionString <- fetchTransactionRedis connRedis
    let maybeTransaction = decode (fromStrict transactionString) :: Maybe Transaction
  
    -- process transaction
    case maybeTransaction of
      Just transaction -> do
        -- manipulate balances
        decrementBalanceDb connDb (getSender transaction) (getAmount transaction)
        incrementBalanceDb connDb (getReceiver transaction) (getAmount transaction)
        -- commit transaction
        commit connDb
      _ -> print "no transactions"
    
    -- close connections
    Database.HDBC.disconnect connDb
    Database.Redis.disconnect connRedis

    print "transaction processing done"


-- get transaction bytestring from Redis
fetchTransactionRedis :: Database.Redis.Connection -> IO (Data.ByteString.ByteString)
fetchTransactionRedis conn = runRedis conn $ do
  result <- lpop "list:transactions"
  case result of
    Right (Just transactionString) -> return transactionString
    _ -> return $ Data.ByteString.Char8.pack "no transactions"


-- update card in database
incrementBalanceDb :: Database.HDBC.ODBC.Connection -> Int -> Double -> IO Integer
incrementBalanceDb conn card_id amount = do
  run conn "UPDATE cards SET balance = balance + ? where id=?;" [toSql amount,toSql card_id]

decrementBalanceDb :: Database.HDBC.ODBC.Connection -> Int -> Double -> IO Integer
decrementBalanceDb conn card_id amount = do
  run conn "UPDATE cards SET balance = balance - ? where id=?;" [toSql amount,toSql card_id]
  

