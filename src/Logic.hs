module Logic where

import Graphics.Gloss (loadBMP)
import Graphics.Gloss.Interface.IO.Interact
import Data.Array (inRange)
import System.Exit (exitSuccess)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy
import Control.Concurrent.MVar
import Network.Socket hiding (recv, sendAll)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Data.Maybe (fromMaybe, fromJust)

import Game
import Cards
import BetProcessing (processNumber, isKeyPad)

makeIOFromList :: [(String, IO Picture)] -> IO [(String, Picture)]
makeIOFromList xs = sequenceA $ fmap helper xs
    where
        helper = (\(f, s) -> s >>= return . ((,) f) )


-- -- validateBet :: Bet -> Bet
-- validateBet bets =


buildDealerHand :: Hand -> Deck -> Hand
buildDealerHand hand deck' =
    case countHandScore hand of
        Moscow       -> hand
        ScoreValue v -> if v <= 17
                        then let newCard = head deck'
                             in buildDealerHand (newCard : hand) (tail deck')
                        else hand
        Overflow _   -> hand


checkOverflow :: Hand -> GameState
checkOverflow hand =
    case countHandScore hand of
        Overflow _ -> Finished
        _          -> Running


giveCard :: Game -> Game
giveCard game =
    case gamePlayer game of
        GamePlayer -> game { deck = tail oldDeck
                           , playerHand = newCard : oldHand
                           , gameState = checkOverflow (newCard : oldHand)
                           , currentBankAmount = max 0 finMoney
                           }
                        where
                            finMoney = if checkOverflow (newCard : oldHand) == Finished
                                          then (curMoney - curBet)
                                          else curMoney
                            oldDeck  = deck game
                            oldHand  = playerHand game
                            newCard  = head oldDeck
                            curMoney = currentBankAmount game
                            curBet   = betListToInt . currentBet $ game
        GameDealer -> game { dealerHand = dHand
                           , gameState  = Finished
                           , currentBankAmount = dHand `seq` (max 0 finMoney)
                           }
                      where
                          finMoney =
                              case pScore > dScore of
                                       True  -> if pScore == 22
                                                  then (curMoney + (round $ fromIntegral curBet * (1.5 :: Double)))
                                                  else (curMoney + curBet)
                                       False -> curMoney - curBet
                          dHand    = buildDealerHand (dealerHand game) (deck game)
                          pHand    = playerHand game
                          curBet   = betListToInt . currentBet $ game
                          curMoney = currentBankAmount game
                          pScore   = scoreToInt . countHandScore $ pHand
                          dScore   = scoreToInt . countHandScore $ dHand



standEvent :: Game -> Game
standEvent game = game {gamePlayer = GameDealer}

hitEvent :: Game -> Game
hitEvent game = game {gamePlayer = GamePlayer}


isCoordCorrect :: (Int, Int) -> Bool
isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))


mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

processNext :: Game -> IO Game
processNext game = do
    shuffledDeck <- shuffleDeck
    -- let (playerHand', remainingDeck) = dealCards 2 shuffledDeck
    return $ game { gameState = Betting
                  , deck = shuffledDeck
                  , playerHand = []
                  , dealerHand = []
                  , currentBet = []
                  }


processExit :: Game -> IO Game
processExit game =
    let sock = gameSocket game
    in return menuScreen {gameSocket = sock}


startGame :: Maybe Socket -> IO Game
startGame (Just sock) = do
    let names = map transformCardToPath fullDeck
    let pairs = ("images/card_back.bmp", loadBMP "images/card_back.bmp")
                : (zip names (map loadBMP names))
    p <- makeIOFromList pairs
    shuffledDeck <- shuffleDeck
    -- let (playerHand', remainingDeck) = dealCards 2 shuffledDeck
    return initialGame { deck = shuffledDeck
                       , cardImages = p
                       , playerHand = []
                       , gameState  = Betting
                       , gameSocket = Just sock
                       }


processMouseClick :: Game -> (Int, Int) -> IO Game
processMouseClick game mousePos
    | isCoordCorrect mousePos =
        case mousePos of
            (2, 1)    -> processNext game
            (2, 2)    -> processNext game
            (2, 4)    -> processExit game
            (2, 5)    -> processExit game
            _         -> return game
    | otherwise = return game

processMenuMouseClick :: Game -> (Int, Int) -> IO Game
processMenuMouseClick game mousePos@(x, y)
    | isCoordCorrect mousePos =
        let middle = fromIntegral $ div n 2
        in case (abs $ middle - y) < 2 of
                True -> case middle - x of
                             1    -> exitSuccess
                             (-1) -> startGame $ gameSocket game
                             0    -> return game {gameState = Settings}
                             _    -> return game
                _    -> return game
    | otherwise = return game


transformGame :: Event -> Game -> IO Game
transformGame (EventKey (SpecialKey KeySpace) Up _ _) game =
    return
    $ case gameState game of
          Running -> giveCard $ hitEvent game
          _       -> game


transformGame (EventKey (SpecialKey KeyEnter) Up _ _) game =
    case gameState game of
          Running -> return $ giveCard $ standEvent game
          Betting -> do
                        shuffledDeck <- shuffleDeck
                        let (playerHand', remainingDeck) = dealCards 1 shuffledDeck
                        let bet = betListToInt . currentBet $ game
                        if bet > currentBankAmount game
                            then return game { gameState  = Running
                                      , currentBet = [currentBankAmount game]
                                      , playerHand = playerHand'
                                      , deck       = remainingDeck
                                      }
                            else return game { gameState = Running
                                      , playerHand = playerHand'
                                      , deck       = remainingDeck
                                      }
          _       -> return game


transformGame (EventKey (SpecialKey KeyEsc) Up _ _) game =
    let sock = gameSocket game
    in return menuScreen {gameSocket = sock}


transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
          Finished -> processMouseClick game $ mousePosAsCellCoord mousePos
          Menu     -> processMenuMouseClick game $ mousePosAsCellCoord mousePos
          _        -> return game


transformGame (EventKey key Up _ _) game
    | isKeyPad key && (gameState game == Betting) = processNumber key game
    | otherwise = return game

transformGame _ game = return game


takeGameFromServer :: MVar Game -> Float -> Game -> IO Game
takeGameFromServer mvarGame _ game = do
    let sock = gameSocket game
    tmp <- (recv (fromJust sock) 20000)
    if (not $ Data.ByteString.Lazy.null tmp) then do
        recievedGame <- fmap decodeGame (return tmp)
        putMVar mvarGame recievedGame
        return recievedGame
    else return game


sendEventToServer :: Maybe Socket -> Event -> IO ()
sendEventToServer Nothing _ = undefined
sendEventToServer (Just sock) event = undefined {- sendAll sock (encode . event)-}


transformGame' :: MVar Game -> Event -> Game -> IO Game
transformGame' mvarGame event@(EventKey (SpecialKey KeySpace) Up _ _) game = do
    mgame <- fmap (fromMaybe game) $ tryTakeMVar mvarGame
    sendEventToServer (gameSocket mgame) event
    return game

transformGame' _ _ game = return game

chooseTransformer :: Maybe Socket -> MVar Game -> Event -> Game -> IO Game
chooseTransformer Nothing _ = transformGame
chooseTransformer (Just _) mgame = transformGame' mgame
