
{-# LANGUAGE OverloadedStrings #-}

import Database.Redis
import Data.ByteString
import Control.Monad.Trans

import Data.Maybe (Maybe(Nothing))

import Lib

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {connectPort = PortNumber 6380}

main :: IO ()
main = do
    conn <- connect myConnectInfo
    runRedis conn $ do
        currentTransaction <- lpop "list:transactions"
        liftIO $ print currentTransaction