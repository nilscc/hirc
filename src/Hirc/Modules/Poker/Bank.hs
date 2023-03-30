{-# LANGUAGE DeriveGeneric #-}

module Hirc.Modules.Poker.Bank where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import Data.Aeson
import Data.Time (UTCTime)
import Data.Maybe (fromMaybe)

import Hirc

type Money = Integer

data Bank = Bank
  { bankBalance :: Map UserName Money
  , bankLoans :: Map UserName [Loan]
  }
  deriving (Generic, Eq, Show)

instance ToJSON Bank where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Bank

emptyBank :: Bank
emptyBank = Bank M.empty M.empty

totalLoans :: UserName -> Bank -> Money
totalLoans u b = maybe 0 (sum . map loanAmount) (M.lookup u $ bankLoans b)

data Loan = Loan
  { loanAmount :: Money
  , loanUTC :: UTCTime
  }
  deriving (Generic, Eq, Show)

instance ToJSON Loan where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Loan

balance :: UserName -> Bank -> Money
balance u = fromMaybe 0 . M.lookup u . bankBalance

deposit :: UserName -> Money -> Bank -> Bank
deposit user money bank = bank
  { bankBalance = M.alter f user $ bankBalance bank
  }
 where
  f = Just . maybe money (money +)

withdraw :: UserName -> Money -> Bank -> Bank
withdraw user money bank = bank
  { bankBalance = M.adjust (\m -> m - money) user $ bankBalance bank
  }

newLoan :: UserName -> Loan -> Bank -> Bank
newLoan user loan bank = bank
  { bankLoans = M.alter f user $ bankLoans bank
  , bankBalance = M.alter g user $ bankBalance bank
  }
 where
  f = Just . maybe [loan] (loan:)
  g = Just . maybe m (m +)
  m = loanAmount loan

saveToJson :: Bank -> FilePath -> IO ()
saveToJson b fp = BS.writeFile fp $ encode b

loadFromJson :: FilePath -> IO (Maybe Bank)
loadFromJson fp = decode <$> BS.readFile fp