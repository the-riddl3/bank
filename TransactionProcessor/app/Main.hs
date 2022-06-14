{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import Database.Redis hiding (decode)
import Data.ByteString
import Data.ByteString.Lazy
import Data.Text.Encoding
import System.Exit
import Data.Either
import Data.Aeson
import Control.Monad.Trans
import Data.Maybe (Maybe(Nothing))
import GHC.Generics

data Transaction = Transaction
  { senderCardId :: Int,
    receiverCardId :: Int,
    amount :: Double
  }
  deriving (Show, Generic)

instance FromJSON Transaction

instance ToJSON Transaction


myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {connectPort = PortNumber 6379}


main :: IO ()
main = do
    conn <- connect myConnectInfo
    Prelude.putStrLn $ fetchTransactionRedis conn

fetchTransactionRedis :: Connection -> IO (Maybe Transaction)
fetchTransactionRedis conn = runRedis conn $ do
  result <- lpop "list:transactions"
  case result of
    Right (Just transactionString) -> return $ Just $ decode $ fromStrict transactionString :: Transaction
    _ -> return Nothing