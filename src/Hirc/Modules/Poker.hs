{-# LANGUAGE ScopedTypeVariables, TypeFamilies, NamedFieldPuns, LambdaCase, DeriveAnyClass, TupleSections, RankNTypes #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Hirc.Modules.Poker
  ( pokerModule
  ) where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad (mzero, join, forM_, unless, when, guard)
import Control.Monad.State
import Control.Exception.Peel (handle, Exception, throw, SomeException (SomeException), catch)
import qualified Control.Monad.Reader as R
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.List hiding (delete)
import System.Random
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar, throwSTM, catchSTM)
import qualified Control.Concurrent.STM as STM
import GHC.Conc (TVar(TVar), STM (STM))
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.Except (runExceptT, ExceptT)
import Data.Acid.TemplateHaskell (tyVarBndrName)
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Data.Data (DataRep(FloatRep))

import Hirc
import Hirc.Modules.Poker.Cards
import Hirc.Modules.Poker.Game
import Hirc.Modules.Poker.Exception
import Hirc.Modules.Poker.Module
import Hirc.Modules.Poker.STM
import Hirc.Modules.Poker.Exception (PokerException(GameUpdateFailed))
import Hirc.Modules.Poker.Bank (Loan(loanAmount, Loan, loanUTC), newLoan, withdraw, balance, Money, deposit)
import Data.List.Extra (groupOn)
import Data.Ord (Down(Down))

--------------------------------------------------------------------------------
-- The main module

instance IsModule PokerModule where
  type ModuleState PokerModule = PokerState
  moduleName _ = "Poker"
  initModule _ = initPokerState
  shutdownModule _ = Nothing
  onStartup _ = Nothing
  onMessage _ = Just runPokerModule

pokerModule :: Module
pokerModule = newModule PokerModule

runPokerModule :: PokerM ()
runPokerModule = handle pokerExceptions $ do

  -- handle nick changes of players
  onCommand "NICK" $ doneAfter updatePlayerNicknames

  -- TODO: handle part/quit of players

  runBankCommands
  runPokerCommands
 where
  pokerExceptions :: SomeException -> PokerM ()
  pokerExceptions e = do
    msg <- getMessage
    logM 1 $ "Exception on message: " ++ show e ++ "\n" ++ show msg

runBankCommands :: PokerM ()
runBankCommands = do
  -- debug only:
  userCommand $ \"bank" -> do
    b <- runSTM askBank
    logM 2 $ show b

  userCommand $ \"bank" "balance"         -> doneAfter bankBalance
  userCommand $ \"bank" "help"            -> doneAfter bankHelp

  -- check if game is running currently
  inGame <- runSTM userInGame
  unless inGame $ do
    userCommand $ \"bank" "loan"          -> doneAfter $ bankLoan defaultLoan

  -- userCommand $ \"bank" "withdraw" amount -> doneAfter $
  --   bankWithdraw $ Just (read amount)
  -- userCommand $ \"bank" "withdraw"        -> doneAfter $ bankWithdraw Nothing

  -- userCommand $ \"bank" "deposit" amount  -> doneAfter $
  --   bankDeposit (Just $ read amount)
  -- userCommand $ \"bank" "deposit"         -> doneAfter $ bankDeposit Nothing

 
defaultLoan :: Money
defaultLoan = 10000

minimumBalanceForLoan :: Money
minimumBalanceForLoan = 1000

runPokerCommands :: PokerM ()
runPokerCommands = do

  --
  -- START DEBUG
  --

  userCommand $ \"game" -> doneAfter $ do
    mg <- runSTM askMaybeGame
    logM 2 $ show mg

  --
  -- END DEBUG
  --

  userCommand $ \"poker" "help"     -> doneAfter showHelp

  userCommand $ \"players"          -> doneAfter showPlayers
  userCommand $ \"stack"            -> doneAfter showStack

  mg <- runSTM $ ignoreConst Nothing [NotInChannel] askMaybeGame

  -- starting a new game
  when (maybe True isNewGame mg) $ do

    userCommand $ \"poker" "join"   -> doneAfter playerJoin
    userCommand $ \"poker" "leave"  -> doneAfter $ do
      playerInGameGuard
      playerQuit

    let runDeal = doneAfter $ do
          playerInGameGuard
          deal

    userCommand $ \"deal" "cards"   -> runDeal
    userCommand $ \"deal"           -> runDeal

  -- while in game
  when (maybe False isActiveGame mg) $ do

    userCommand $ \"pot"            -> doneAfter showPot
    userCommand $ \"turn"           -> doneAfter showCurrentPlayer
    userCommand $ \"order"          -> doneAfter showCurrentOrder
    userCommand $ \"hand"           -> doneAfter showHand

    userCommand $ \"check"          -> doneAfter $ do
      currentPlayerOnlyGuard
      check'

    userCommand $ \"call"           -> doneAfter $ do
      currentPlayerOnlyGuard
      call

    userCommand $ \"raise" amount   -> doneAfter $ do
      currentPlayerOnlyGuard
      raise (read amount)

    userCommand $ \"fold"          -> doneAfter $ do
      currentPlayerOnlyGuard
      fold'

  {-
  userCommand $ \"cards"          -> doneAfter showCurrentCards
  userCommand $ \"ca"             -> doneAfter showCurrentCards

  userCommand $ \"bet"   amount   -> doneAfter $ raise amount
  --userCommand $ \"fold" "now"     -> doneAfter fold'
  userCommand $ \"fold"           -> doneAfter fold
  userCommand $ \"all" "in"       -> doneAfter allIn
  -}


-- Help
showHelp :: PokerM ()
showHelp = do
  whisper "Playing Texas Hold'em, available commands are:"
  whisper "    poker help                --   show this help"
  whisper "    poker join                --   join a new game"
  whisper "    poker leave               --   leave the current game"
  whisper "    players                   --   show who is playing in the next game"
  whisper "    deal                      --   deal cards, start a new game"
  whisper "While playing:"
  whisper "    check/call/fold           --   check, call the current bet or fold your cards"
  whisper "    raise <num>               --   bet a new sum (at least big blind or the amount of the last raise)"
  whisper "    all in                    --   go all in"
  whisper "    pot                       --   show current pot"
  whisper "    order                     --   show current order"
  whisper "    cards                     --   show your own and all community cards"
  whisper "    turn                      --   show whose turn it currently is"


--------------------------------------------------------------------------------
-- Random

askStdGen :: PokerM StdGen
askStdGen = do
  g <- runSTM askGame
  case stdGen g of
    Just sg -> return sg
    Nothing -> do
      sg <- liftIO initStdGen
      runSTM $ updateGame $ \g -> g { stdGen = Just sg }
      return sg

shuffle :: StdGen -> [a] -> [a]
shuffle sg ls = do
  execState `flip` ls $
    forM_ [1..length ls-1] $ \i -> do
      let j = randomRs (0,i) sg !! i
      modify $ swap j i
 where
  swap j i l =
    let lj = l !! j
        li = l !! i
     in replace i lj (replace j li l)
  replace n y xs = take n xs ++ [y] ++ drop (n+1) xs


--------------------------------------------------------------------------------
-- Helper

runSTM :: PokerSTM a -> PokerM a
runSTM rstm = do
  mchan <- getCurrentChannel
  withNickAndUser $ \n u -> do
    tvar <- R.ask
    liftIO $ atomically $ runReaderT rstm (tvar, n, u, mchan)


--------------------------------------------------------------------------------
-- Information

showColors :: PokerM ()
showColors = do
  let c1 = head fullDeck
      c2 = fullDeck !! 13
      c3 = fullDeck !! 26
      c4 = fullDeck !! 39
  answer $ unwords (map colorCard [c1,c2,c3,c4])

showPlayers :: PokerM ()
showPlayers = do
  g <- runSTM askGame
  if null (players g) then
    say "No players yet."
   else
    say $ "Current players: " ++ intercalate ", " (map playerNickname (players g))

showPot :: PokerM ()
showPot = join . runSTM $ do
  g <- askGame
  p <- askPlayer
  t <- askToCall p
  return $ answer $
    "You currently have " ++ show (playerPot p) ++ " in the pot"
    ++ (if t > 0 then " (" ++ show t ++ " to call)" else "")
    ++ ". (total pot size: " ++ show (totalPotSize g) ++ ")"

showStack :: PokerM ()
showStack = do
  p <- runSTM askPlayer
  answer $ "You currently have " ++ show (playerStack p) ++ " in your stack"
    ++ (if playerPot p > 0 then " and " ++ show (playerPot p) ++ " in the current pot" else "")
    ++ "."

--------------------------------------------------------------------------------
-- Bank commands
--

bankBalance :: PokerM ()
bankBalance = withUsername $ \u -> 
  join . runSTM $ do
    b <- askBank
    return $ answer $ "Your bank account balance is: " ++ show (balance u b)

bankLoan :: Money -> PokerM ()
bankLoan amount = withUsername $ \u -> do
  now <- liftIO getCurrentTime
  join $ runSTM $ do
    -- check if balance is small enough
    b <- askBank
    let bal = balance u b
    if bal <= minimumBalanceForLoan then do
      let l = Loan { loanUTC = now, loanAmount = amount - bal }
      updateBank $ newLoan u l
      return $ answer $
        "You loaned " ++ show (loanAmount l) ++ " from the bank."
     else
      return $ answer "You still have enough funds."

-- bankWithdraw :: Maybe Money -> PokerM ()
-- bankWithdraw mm = do
--   u <- getUsername
--   join . runSTM $ do
--     b <- askBank
--     case (mm, balance u b) of
--       -- check if account balance is positive
--       (_, bal) | bal <= 0 -> return $ answer $
--         "You have nothing in your bank account. " ++
--         "You may take out another loan if you have to."
-- 
--       -- check if balance is high enough to withdraw amount
--       (Just m, bal)
--         | m <= bal -> do
--           putBank $ withdraw u m b
--           return $ answer $
--             "You withdrew " ++ show m ++ "from your bank account " ++
--             "(new balance: " ++ show (bal - m) ++ ")."
--         | otherwise -> do
--           return $ answer "You don't have enough money in your bank account to do that."
-- 
--       -- withdraw everything
--       (Nothing, bal) -> do
--         putBank $ withdraw u bal b
--         return $ answer $ "You withdrew " ++ show bal ++ " from your bank account."
-- 
-- bankDeposit :: Maybe Money -> PokerM ()
-- bankDeposit _ = answer "Not implemented."

bankHelp :: PokerM ()
bankHelp = return ()


--------------------------------------------------------------------------------
-- Start game
--

playerJoin :: PokerM ()
playerJoin = join . runSTM $ do
  ig <- userInGame
  if ig then
    return $ answer "You're already in the game!"
   else do
    n <- askNick
    u <- askUser
    -- check bank balance
    b <- askBank
    let bal = balance u b
    if bal <= bigBlind then
      return $ do
        logM 2 $ show bal ++ " <= " ++ show bigBlind
        answer $
          "You don't have enough money to join the game. You need at least " ++ show bigBlind ++ "."
     else do
      -- withdraw all money from the bank and put it into the current game
      putBank $ withdraw u bal b
      -- add player to game
      updateGame $ addPlayer' Player
        { playerNickname = n
        , playerUsername = u
        , playerStack = bal
        , playerPot = 0
        , playerHand = Nothing
        }
      return $ say $ "Player \"" ++ n ++ "\" joins the game."
 where
  addPlayer' p g = g { players = players g ++ [p] }


playerQuit :: PokerM ()
playerQuit = handle playerNotFound $ do
  join . runSTM $ do
    p <- askPlayer
    if isNothing (playerHand p) then do
      updateGame $ \g -> g
        { players = L.filter
            ((playerUsername p /=) . playerUsername)
            (players g)
        }
      return $ say ("\"" ++ playerNickname p ++ "\" left the game.")
     else
      return $ answer "You have to fold first."
 where
  playerNotFound PlayerNotFound = done

updatePlayerNicknames :: PokerM ()
updatePlayerNicknames = withParams $ \[newNick] -> runSTM $ do

  u <- askUser
  let changeNick p@Player{ playerUsername }
        | u == playerUsername = p { playerNickname = newNick }
        | otherwise = p
      updateNick g = g { players = map changeNick (players g) }

  -- change all players in all games
  updatePokerState $ \pokerState -> 
    pokerState { games = M.map updateNick (games pokerState) }

showCurrentPlayer :: PokerM ()
showCurrentPlayer = join . runSTM $ do
  cp <- askCurrentPlayer
  tc <- askToCall cp
  return $ say $
    "Current player: " ++ playerNickname cp
    ++ (if tc > 0 then " (" ++ show tc ++ " to call)" else "")

showCurrentOrder :: PokerM ()
showCurrentOrder = do
  (order, fpos) <- runSTM $ (,) <$> askCurrentOrder <*> askFirstPosition

  let formatNames p =
         -- first player indication
        (if p == fpos then "*" else "")
         -- nick
        ++ playerNickname p
         -- (current pot/total wealth)
        ++ " (" ++ show (playerPot p) ++ "/" ++ show (playerStack p) ++ ")"

  say $ "Currently playing: " ++ intercalate ", " (map formatNames order)

showHand :: PokerM ()
showHand = do
  p <- runSTM askPlayer
  showHand' p

showHand' :: Player -> PokerM ()
showHand' p
  | Just (Hand hand) <- playerHand p =
    sendNoticeTo (playerNickname p) $ "Your hand: " ++ unwords (map colorCard hand)
  | otherwise =
    withNickAndUser $ \n u ->
      logM 1 $ "No hand found: " ++ show p ++ " (" ++ n ++ " / " ++ u ++ ")"


--------------------------------------------------------------------------------
-- Play

deal :: PokerM ()
deal = do
  sg <- askStdGen
  join . runSTM . (`orElse` return (answer "Waiting for more players to join.")) $ do

    -- Shuffle player order and card deck
    updateGame $ \g -> g
      { players = shuffle sg (players g)
      , deck = shuffle sg fullDeck
      }

    pls <- askPlayers
    check $ length pls >= 2

    updateGame dealCards
    blnds <- payBlinds'

    return $ do
      say $ "Starting a new round! The players are: " ++ intercalate ", " (map playerNickname pls)
      say "Dealing hands…"
      notifyHands
      blnds

-- Send NOTICE to all players with their hands
notifyHands :: PokerM ()
notifyHands = do
  g <- runSTM askGame
  forM_ (players g) showHand'

payBlinds' :: PokerSTM (PokerM ())
payBlinds' = do
  g <- askGame
  let (sb,bb) = blinds g
      (p1:p2:_) = players g

  -- lookup small and big blinds
  updateGame payBlinds

  -- done
  return $ do
    say $ playerNickname p1 ++ " pays " ++ show sb ++ " (small blind)."
    say $ playerNickname p2 ++ " pays " ++ show bb ++ " (big blind)."
    showCurrentPlayer

playerInGameGuard :: PokerM ()
playerInGameGuard = do
  inGame <- runSTM userInGame
  unless inGame done

currentPlayerOnlyGuard :: PokerM ()
currentPlayerOnlyGuard = do
  isCur <- runSTM isCurrentPlayer
  unless isCur $ do
    answer "It's not your turn!"
    done

check' :: PokerM ()
check' = do
  join . runSTM $ do
    pl <- askCurrentPlayer
    tc <- askToCall pl
    if tc == 0 then do
      nxt <- nextPlayer
      return $ do
        say $ playerNickname pl ++ " checks."
        nxt
     else
      return $ answer "You can't do that!"

call :: PokerM ()
call = join . runSTM $ do
  pl <- askCurrentPlayer
  tc <- askToCall pl

  -- check if there is a positive amount to call and if player money is sufficient
  if tc > 0 && playerStack pl >= tc then do
    bet pl tc
    nxt <- nextPlayer
    return $ do
      say $ playerNickname pl ++ " calls " ++ show tc ++ "."
      nxt

  -- player does not have enough money
   else if playerStack pl < tc then
    return $ answer $ "You don't have enough money to do that. " ++
      "To call: " ++ show tc ++ ", your wealth: " ++ show (playerStack pl)

  -- there is nothing to call
   else
    return $ answer "You can't do that."

raise :: Money -> PokerM ()
raise r = join . runSTM $ do
  pl <- askCurrentPlayer
  tc <- askToCall pl

  -- subtract both amount to call and the amount to raise from player money
  let tot = r + tc

  -- raise must be at least equal to last raise
  mlr <- askLastRaise
  case mlr of
    -- check if raise is at least as high as the last raise
    Just (_,lr)
      | r < lr -> return $ say $ "You must raise at least " ++ show lr
    -- check for sufficient funds
    _ | playerStack pl < tot ->
          return $ say "You don't have enough money to do that."
    -- all good => execute raise
      | otherwise -> do
          bet pl tot
          newPot <- askCurrentPot
          updateGame $ \g -> g { lastRaise = Just (currentPosition g, r) }
          nxt <- nextPlayer
          return $ do
            say $ playerNickname pl ++ " raises the pot by " ++ show r ++ "."
            nxt

fold' :: PokerM ()
fold' = join . runSTM $ do

  -- remove current player from game
  p <- askCurrentPlayer
  updateGame $ \g -> g
    { players = L.delete p (players g)
    , currentPosition = currentPosition g `mod` (length (players g) - 1)
    , pot = pot g + playerPot p
    }

  -- put stack back into bank account
  updateBank $ deposit (playerUsername p) (playerStack p)

  -- check how to continue game
  g <- askGame
  e <- if length (players g) <= 1 then
    endGame
   else
    return showCurrentPlayer
  return $ do
    say $ playerNickname p ++ " folds."
    e
 where
  deleteAt i l = let (a,_:b) = splitAt i l in a ++ b


--------------------------------------------------------------------------------
-- Organizing players & cards

incPosition :: PokerSTM ()
incPosition = updateGame $ \g -> g
  { currentPosition = (currentPosition g + 1) `mod` length (players g)
  }

nextPlayer :: PokerSTM (PokerM ())
nextPlayer = do
  g <- askGame
  if length (players g) <= 1 then
    -- last player wins the game
    endGame
   else do
    -- advance current position to next player
    incPosition
    -- check if phase has ended
    endPhase `orElse` return showCurrentPlayer
 where
  endPhase = do
    g <- askGame
    let mlr = lastRaise g
        cp = currentPosition g
        np = length $ players g -- number of players

    -- end phase only if:
    lift $ STM.check $ maybe
      -- there was no raise yet and current position is first player after big blind
      (cp == 0)
      -- or current position is last raise
      ((cp ==) . fst)
      mlr

    -- perform next phase
    case g of
      _ | Nothing <- flop g -> do
          updateGame showFlop
          g' <- askGame
          case flop g' of
            Just (a,b,c) -> return $ do
              say $ "Flop: " ++ unwords (map colorCard [a,b,c])
              showCurrentPlayer
            _ -> return $ do
              logM 1 $ "Flop failed: " ++ show g'
              throw GameUpdateFailed
        | Nothing <- turn g -> do
          updateGame showTurn
          g' <- askGame
          case (flop g', turn g') of
            (Just (a,b,c), Just d) -> return $ do
              say $ "Turn: " ++ unwords (map colorCard [a,b,c]) ++ "  " ++ colorCard d
              showCurrentPlayer
            _ -> return $ do
              logM 1 $ "Turn failed: " ++ show g'
              throw GameUpdateFailed
        | Nothing <- river g -> do
          updateGame showRiver
          g' <- askGame
          case (flop g', turn g', river g') of
            (Just (a,b,c), Just d, Just e) -> return $ do
              say $ "Turn: " ++ unwords (map colorCard [a,b,c,d]) ++ "  " ++ colorCard e
              showCurrentPlayer
            _ -> return $ do
              logM 1 $ "River failed: " ++ show g'
              throw GameUpdateFailed

        | otherwise -> endGame

endGame :: PokerSTM (PokerM ())
endGame = do
  -- figure out player hand ranks
  g <- askGame

  -- TODO: handle side pots
  let pot = totalPotSize g

  forM_ (players g) $ \p -> updateBank $ deposit (playerUsername p) (playerStack p)
  -- reset game
  putGame newGame

  case players g of

    -- last player wins the game
    [p] -> do
      updateBank $ deposit (playerUsername p) pot
      return $ do
        say $
          playerNickname (head $ players g) ++ " wins the game. " ++
          "Pot size: " ++ show (totalPotSize g)
   
    _ | Just (f1,f2,f3) <- flop g
      , Just t <- turn g
      , Just r <- river g -> do

        let -- community cards
            cc = [f1,f2,f3,t,r]
            -- figure out best hand
            ranks = map (\p -> (p, findBestHand . (cc ++) . hCards =<< playerHand p)) (players g)
            -- get winner(s) and winning hand(s)
            (winners:_) = groupOn snd $ sortOn (Down . snd) ranks

        result <- case winners of

          -- single winner takes it all
          [(p,Just h)] | Just r <- rank h -> do
            updateBank $ deposit (playerUsername p) pot
            return $ say $ playerNickname p ++ " wins the game with: " ++ show r

          -- multiple winners share the pot
          ((p, Just h):_) | Just r <- rank h -> do
            let n = length winners
            forM_ winners $ \(p,_) -> do
              updateBank $ deposit (playerUsername p) (pot `div` fromIntegral n)
            return $ say $
              "Split pot! The amount of " ++ show pot ++ " is split between: " ++
              intercalate ", " (map (playerNickname . fst) winners) ++ ". " ++
              "Winning hand: " ++ show r

          -- catch errors
          _ -> return $ do
            logM 1 $ "No winner?! " ++ show g
            throw GameUpdateFailed

        return $ do
          say "Showdown! These are the players hands:"
          forM_ (players g) $ \p@Player{ playerHand = Just h }->
            say $ "  " ++ playerNickname p ++ ": " ++ unwords (map colorCard (hCards h))
          result

    _ -> return $ do
      logM 1 $ "End game failed: " ++ show g
      throw GameUpdateFailed