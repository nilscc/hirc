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
import Hirc.Modules.Poker.Bank (Loan(loanAmount, Loan, loanUTC), newLoan, withdraw, balance, Money)

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
  whisper "    money                     --   show your current wealth"
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


{-
showMoney :: MessageM ()
showMoney = withUsername $ \u -> do
  wealth <- loadMoney u
  answer $ "Your current wealth is " ++ show wealth ++ "."

loadMoney :: UserName -> MessageM Integer
loadMoney u = do
  mmo <- loadGlobal "money"
  case mmo of
       Just mo | Just wealth <- lookupMap u mo ->
         return wealth
       _ -> do
         -- add the starting money if the current player has no money at all
         updateGlobal "money" $ \mmo' ->
           case mmo' of
               Just mo
                 | memberMap u mo -> mo
                 | otherwise ->
                   insertMap u startingMoney mo
               Nothing ->
                   singletonMap u startingMoney
         return startingMoney

getCurrentOrder :: MessageM (Maybe [(NickName, UserName, Integer, Integer)])
getCurrentOrder = do
  mo <- load "order"
  case mo of
       Just o | Just order <- fromList o -> do
         nicks <- fmap (map fromJust)                  $ mapM getPlayerNickname (order)
         mons  <- fmap (map $ fromMaybe startingMoney) $ mapM getPlayerWealth   (order)
         pot   <- fmap (map $ fromMaybe 0)             $ mapM getPlayerPot      (order)
         return $ Just $ zip4 nicks order mons pot
       _ -> return Nothing

getPlayerNickname :: UserName -> MessageM (Maybe NickName)
getPlayerNickname u = do
  mps <- load "players"
  return $
    case mps of
         Just ps -> lookupMap u ps
         _       -> Nothing

getPlayerWealth :: UserName -> MessageM (Maybe Integer)
getPlayerWealth u = do
  loadGlobal "money" ~> u

getPlayerPot :: UserName -> MessageM (Maybe Integer)
getPlayerPot u = do
  load "pot" ~> u

getPotMax :: MessageM (Maybe Integer)
getPotMax = do
  fmap getMax `fmap` load "pot"
 where
  getMax = maximum . elemsMap

getAmountToCall :: UserName -> MessageM Integer
getAmountToCall u = do
  mp <- getPlayerPot u
  mx <- getPotMax
  return $ fromMaybe 0 mx - fromMaybe 0 mp

showCurrentOrder :: MessageM ()
showCurrentOrder = do
  mo <- getCurrentOrder
  mfp <- load "first position"
  let formatNames (n,u,w,p) =
        (if Just u == mfp then "*" else "")  -- first player indication
        ++ n  -- nick
        ++ " (" ++ show p ++ "/" ++ show w ++ ")"  -- (current pot/total wealth)

  case mo of
       Just nicks -> say $ "Currently playing: " ++ intercalate ", " (map formatNames nicks)
       Nothing    -> say "Cannot figure out current order – sorry!"
 where

showCurrentPlayer :: MessageM ()
showCurrentPlayer = do
  cur <- getCurrentPlayer
  s   <- load "state"
  case cur of
       Just (n,u) -> do
         mp <- load "pot"
         let p  = fromMaybe (singletonMap "" (0 :: Integer)) mp
             mx, up, tc :: Integer
             mx = maximum (elemsMap p)
             up = fromMaybe mx (lookupMap u p)
             tc = mx - up
         say $ "It's " ++ n ++ "s turn" ++ if tc > 0 then " (" ++ show tc ++ " to call)." else "."
       Nothing | s == Nothing || s == Just "end" ->
         say "No one is playing at the moment."
       Nothing ->
         endGame
-}

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

{-
showCurrentCards :: MessageM ()
showCurrentCards = withUsername $ \u -> do
  showCommunityCards
  showPlayerCards u

showPlayerCards :: UserName -> MessageM ()
showPlayerCards u = do
  mn <- getPlayerNickname u
  mh <- getPlayerCards u
  case (mn, mh) of
       (Just n, Just hand) -> sendNotice n $ "Your hand: " ++ intercalate " " (map colorCard hand)
       _                   -> return ()

showCommunityCards :: MessageM ()
showCommunityCards = do
  ccs <- getCommunityCards
  case ccs of
       (Just (c1,c2,c3), Nothing, Nothing) ->
         say $ "Flop cards: " ++ intercalate " " (map colorCard [c1,c2,c3])
       (Just (c1,c2,c3), Just tc, Nothing) ->
         say $ "Turn cards: " ++ intercalate " " (map colorCard [c1,c2,c3]) ++ "  " ++ colorCard tc
       (Just (c1,c2,c3), Just tc, Just rc) ->
         say $ "River cards: " ++ intercalate " " (map colorCard [c1,c2,c3]) ++ "  " ++ colorCard tc ++ "  " ++ colorCard rc
       _ ->
         say "There are no community cards yet."

getPlayerCards :: UserName -> MessageM (Maybe [Card])
getPlayerCards u = do
  mr <- load "hands" ~> u
  return $
    case mr of
         Just (fromList -> Just l) -> fmap sort $ sequence $ map toCard l
         _ -> Nothing

getCommunityCards :: MessageM (Maybe (Card,Card,Card), Maybe Card, Maybe Card)
getCommunityCards = do
  -- load flop cards
  mfres <- load "flop"
  --Just (Just fl) <- load "flop"
  --let Just fcs = fromList fl
  let fcs = do fres       <- mfres
               fl         <- fres
               [s1,s2,s3] <- fromList fl
               c1         <- toCard s1
               c2         <- toCard s2
               c3         <- toCard s3
               return (c1,c2,c3)
  -- load turn card
  mtres <- load "turn"
  let tc = do tres <- mtres
              ts   <- tres
              toCard ts
  -- load river card
  mrres <- load "river"
  let rc = do rres <- mrres
              rs   <- rres
              toCard rs
  return (fcs, tc, rc)
  

--------------------------------------------------------------------------------
-- Setting up the game environment

{ -
startGame :: MessageM ()
startGame = withNickAndUser $ \n u -> do
  state <- load "state"
  if (state == Nothing || state == Just "end") then do
    answerTo $ \who ->
      case who of
           Left  _ -> answer "Sorry, but this game needs to be run in a public channel."
           Right _ -> do
             store "state" "new game"
             store "game started by" u
             hirc <- getNickname
             say $ "Poker game started by " ++ n ++ ". Say \"" ++ hirc ++ ": join poker\" to join this game."
             addPlayer n u
   else do
    answer "Poker game already in progress. You can play only one game per channel."
-}



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
  p <- askCurrentPlayer
  updateGame $ \g -> g
    { players = L.delete p (players g)
    , currentPosition = currentPosition g `mod` length (players g) - 1
    , pot = pot g + playerPot p
    }
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

{-

fold :: MessageM ()
fold = requireCurrentPlayer $ do
  fold'
  nextPlayer

fold' :: MessageM ()
fold' = do
  withNickAndUser $ \n u -> do
    say $ n ++ " folds."
    update "order" $
      maybe emptyMap (deleteMap u)
    showCurrentPlayer

allIn :: MessageM ()
allIn = undefined

endGame :: MessageM ()
endGame = do
  say "Ending game."
  store "state" "end"
  delete "flop"
  delete "turn"
  delete "river"
  delete "hands"
  delete "cards"
  delete "last raise"
  delete "pot"
  delete "order"
  done

requireCurrentPlayer :: MessageM () -> MessageM ()
requireCurrentPlayer m = withUsername $ \u -> do
  mc <- getCurrentPlayer
  case mc of
       Just (_,u') | u == u' -> m
       _                     -> answer "It's not your turn."


--------------------------------------------------------------------------------
-- Organizing players & cards

getCurrentPlayer :: MessageM (Maybe (NickName, UserName))
getCurrentPlayer = do
  mo <- load "order"
  mp <- load "players"
  return $
    case (mo, mp) of
         (Just o, Just p)
           | Just u <- headList o
           , Just n <- lookupMap u p ->
             Just (n,u)
         _ -> Nothing
-}

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
      _
        | Nothing <- flop g -> do
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
  let ranks = map (\p -> (p, rank <$> playerHand p)) (players g)

  -- TODO: deposit stacks into bank accounts

  -- reset game
  putGame newGame

  if length (players g) > 1 then do
    return $ do
      say "Showdown! These are the players hands:"
      forM_ (players g) $ \p@Player{ playerHand = Just h }->
        say $ "  " ++ playerNickname p ++ ": " ++ unwords (map colorCard (hCards h))
   else do
    return $ do
      say $
        playerNickname (head $ players g) ++ " wins the game. " ++
        "Pot size: " ++ show (totalPotSize g)
    

{-
nextPlayer :: MessageM ()
nextPlayer = do
  mo' <- load "order"
  if fmap lengthList mo' <= Just 1 then
     endGame
   else do
     update "order" $ \mo ->
       case mo of
           Just l | Just (f :: UserName) <- headList l ->
                appendList (tailList l) (singletonList f)
           _ -> emptyList -- shouldn't happen anyway
     mc  <- getCurrentPlayer
     mlr <- load "last raise" :: MessageM (Maybe (Maybe UserName, Money))
     mgs <- load "round started by"
     let playerIsLastRaise    = fmap fst mlr == Just (fmap snd mc)
         playerHasStartedGame = Nothing == join (fmap fst mlr) && mgs == fmap snd mc
     when (playerIsLastRaise || playerHasStartedGame) nextPhase

nextPhase :: MessageM ()
nextPhase = do
  s <- load "state"
  case s of
       Just "preflop" -> flop
       Just "flop"    -> turn
       Just "turn"    -> river
       Just "river"   -> showdown


--------------------------------------------------------------------------------
-- Phase transitions

burnCard :: MessageM ()
burnCard = update "cards" $ maybe emptyList tailList

takeCard :: MessageM Card
takeCard = do
  Just (Just (Just c)) <- fmap (fmap toCard . headList) `fmap` load "cards"
  update "cards" $ maybe emptyList tailList
  return c

-- start round with first position player
startFromFirstPosition :: MessageM ()
startFromFirstPosition = do
  Just (fp :: UserName) <- load "first position"
  store "round started by" fp
  let skip = do Just (_,cp) <- getCurrentPlayer
                unless (cp == fp) (nextPlayer >> skip)
  skip
  update "last raise" $ \(Just (_,lr)) ->
    ((Nothing :: Maybe UserName), (lr :: Integer))

flop :: MessageM ()
flop = require "state" "preflop" $ do
  say "First betting round ends."
  store "state" "flop"
  burnCard
  c1 <- takeCard
  c2 <- takeCard
  c3 <- takeCard
  let fcs = map show [c1,c2,c3]
  store "flop" $ Just (toList fcs)
  showCommunityCards
  startFromFirstPosition

turn :: MessageM ()
turn = require "state" "flop" $ do
  say "Second betting round ends."
  store "state" "turn"
  -- pick turn card
  burnCard
  tc <- takeCard
  let tcs = show tc
  store "turn" $ (Just tcs)
  -- show cards
  showCommunityCards
  startFromFirstPosition

river :: MessageM ()
river = require "state" "turn" $ do
  say "Third betting round ends. This is the last betting round!"
  store "state" "river"
  -- pick river card
  burnCard
  rc <- takeCard
  let rcs = show rc
  store "river" $ (Just rcs)
  -- show cards
  showCommunityCards
  startFromFirstPosition

showdown :: MessageM ()
showdown = require "state" "river" $ do
  say "Showdown!"
  startFromFirstPosition
  endGame

-}