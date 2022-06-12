{-# LANGUAGE DeriveGeneric #-}
module Lib where

import Data.Aeson
import GHC.Generics

data Transaction = Transaction
  { senderCardId :: Int,
    receiverCardId :: Int,
    amount :: Float
  }
  deriving (Show, Generic)

instance FromJSON Transaction

instance ToJSON Transaction
