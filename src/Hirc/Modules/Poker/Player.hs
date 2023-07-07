module Hirc.Modules.Poker.Player (Player(..)) where

import Hirc.Modules.Poker.Bank (Money)
import Hirc.Modules.Poker.Cards ( Hand )
import Hirc (NickName, UserName)

data Player = Player
  { playerUsername :: UserName
  , playerNickname :: NickName
  , playerStack :: Money
  , playerPot   :: Money
  , playerHand  :: Maybe Hand
  }
  deriving (Eq, Show)