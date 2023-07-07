module Hirc.Modules.Poker.Player (Player(..)) where

import Hirc.Modules.Poker.Bank (Money)
import Hirc.Modules.Poker.Cards ( Hand )
import Hirc (NickName, UserName)

data Player = Player
  { playerNickname :: NickName
  , playerUsername :: UserName
  , playerStack :: Money
  , playerPot   :: Money
  , playerHand  :: Maybe Hand
  }
  deriving (Eq, Show)