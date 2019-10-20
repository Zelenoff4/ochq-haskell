module Game where

import Data.Array (array, Array, range)
import Graphics.Gloss (Picture, Display(..), Color, makeColorI)
import Data.Binary (encode, decode)
import Data.ByteString.Lazy (ByteString)
import Network.Socket

import Cards (Card, Deck, Hand)

type Board      = Array (Int, Int) String
type Bet        = [Int]
data Cell       = Empty | Cards [Card] deriving (Eq, Show, Read)
data GamePlayer = GamePlayer | GameDealer deriving (Eq, Show, Read)
data GameState  = Menu | Settings | Running | Finished | Betting deriving (Eq, Show, Read)
data GameMode   = Solo | Duo deriving (Eq, Show, Read)


data Game = Game { gameBoard :: Board
                 , gamePlayer :: GamePlayer
                 , gameState :: GameState
                 , playerHand :: Hand
                 , dealerHand :: Hand
                 , deck :: Deck
                 , cardImages :: [(String, Picture)]
                 , currentBet :: Bet
                 , currentBankAmount :: Int
                 , gameMode :: GameMode
                 , gameSocket :: Maybe Socket
                 } deriving (Eq)


encodeGame :: Game -> ByteString
encodeGame game =
    encode
    $ show $
    [ show . gameBoard $ game
    , show . gamePlayer $ game
    , show . gameState $ game
    , show . playerHand $ game
    , show . dealerHand $ game
    , show . deck $ game
    , show . currentBet $ game
    , show . currentBankAmount $ game
    , show . gameMode $ game
    ]

decodeGame :: ByteString -> Game
decodeGame bs =
    let gameAsArr = read (decode bs) :: [String]
    in Game { gameBoard  = read (gameAsArr !! 0) :: Array (Int, Int) String
            , gamePlayer = read (gameAsArr !! 1) :: GamePlayer
            , gameState  = read (gameAsArr !! 2) :: GameState
            , playerHand = read (gameAsArr !! 3) :: Hand
            , dealerHand = read (gameAsArr !! 4) :: Hand
            , deck       = read (gameAsArr !! 5) :: Deck
            , currentBet = read (gameAsArr !! 6) :: Bet
            , currentBankAmount = read (gameAsArr !! 7) :: Int
            , gameMode   = read (gameAsArr !! 8) :: GameMode
            }


n :: Int
n = 7

window :: Display
window = InWindow "Test" (screenWidth, screenHeight) (0, 0)

backgroundColor :: Color
backgroundColor = makeColorI 7 99 36 0

screenWidth :: Int
screenWidth = 720

screenHeight :: Int
screenHeight = 720

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

betListToInt :: Bet -> Int
betListToInt bets = helper 0 (reverse bets)
    where
        helper ans []            = ans
        helper ans (bet : bets') = helper (ans + bet * (10 ^ length bets')) bets'


initialGame :: Game
initialGame = Game { gameBoard  = array indexRange $ zip (range indexRange) (cycle ["Empty"])
                   , gameState  = Running
                   , playerHand = []
                   , dealerHand = []
                   , deck       = []
                   , cardImages = []
                   , gamePlayer = GamePlayer
                   , currentBet = []
                   , currentBankAmount = 500
                   , gameMode = Solo
                   , gameSocket = Nothing
                   }
                where indexRange = ((0, 0), (n - 1, n - 1))


menuScreen :: Game
menuScreen = Game { gameBoard  = array indexRange $ zip (range indexRange) (cycle ["Empty"])
                  , gameState  = Menu
                  , playerHand = []
                  , dealerHand = []
                  , deck       = []
                  , cardImages = []
                  , gamePlayer = GamePlayer
                  , currentBet = []
                  , currentBankAmount = 500
                  , gameMode = Solo
                  , gameSocket = Nothing
                  }
                where indexRange = ((0, 0), (n - 1, n - 1))

bettingScreen :: Game
bettingScreen = Game { gameBoard  = array indexRange $ zip (range indexRange) (cycle ["Empty"])
                     , gameState  = Betting
                     , playerHand = []
                     , dealerHand = []
                     , deck       = []
                     , cardImages = []
                     , gamePlayer = GamePlayer
                     , currentBet = []
                     , currentBankAmount = 500
                     , gameMode = Solo
                     , gameSocket = Nothing
                     }
                where indexRange = ((0, 0), (n - 1, n - 1))
