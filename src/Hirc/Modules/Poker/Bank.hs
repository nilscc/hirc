module Hirc.Modules.Poker.Bank where

import Data.Map (Map)
import qualified Data.Map as M

import Hirc
import Data.Time (UTCTime)
import Data.Maybe (fromMaybe)

type Money = Integer

data Bank = Bank
  { bankBalance :: Map UserName Money
  , bankLoans :: Map UserName [Loan]
  }
  deriving (Eq, Show)

emptyBank :: Bank
emptyBank = Bank M.empty M.empty

data Loan = Loan
  { loanAmount :: Money
  , loanUTC :: UTCTime
  }
  deriving (Eq, Show)

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