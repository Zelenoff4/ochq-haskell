module Cards where

import Data.Random (runRVar, StdRandom(..), MonadRandom)
import Data.Random.Extras (shuffle)
import Data.Char (toLower)

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Show, Eq, Enum, Read)

data CardValue = Six | Seven | Eight | Nine |
                 Ten | Jack  | Queen | King | Ace deriving (Eq, Enum, Read, Show)

-- instance Show CardValue where
--     show Six   = "6"
--     show Seven = "7"
--     show Eight = "8"
--     show Nine  = "9"
--     show Ten   = "10"
--     show Jack  = "Jack"
--     show Queen = "Queen"
--     show King  = "King"
--     show Ace   = "Ace"
--
-- instance Read CardValue where
--     read "6" = Six
--     read "7" = Seven
--     read "8" = Eight
--     read "9" = Nine
--     read "10" = Ten
--     read "Jack" = Jack
--     read "Queen" = Queen
--     read "King" = King
--     read "Ace" = Ace


data Card = Card Suit CardValue deriving (Eq, Show, Read)

cardValues :: Card -> Int
cardValues (Card _ Jack)      = 2
cardValues (Card _ Queen)     = 3
cardValues (Card _ King)      = 4
cardValues (Card _ Ace)       = 11
cardValues (Card _ cardValue) = k
    where k = 6 + fromEnum cardValue

transformCardToPath :: Card -> String
transformCardToPath (Card suit value) =
    case value of
        Ten -> pt ++ show k ++ [sFirst suit] ++ ext
        v   -> pt ++ [head $ fakeShowCard $ v] ++ [sFirst suit] ++ ext
    where
        pt       = "images/"
        ext      = ".bmp"
        k        = 6 + fromEnum value
        sFirst = toLower . head . show


fakeShowCard :: CardValue -> String
fakeShowCard Six   = "6"
fakeShowCard Seven = "7"
fakeShowCard Eight = "8"
fakeShowCard Nine  = "9"
fakeShowCard Ten   = "10"
fakeShowCard x     = show x

-- instance Show Card where
    -- show (Card suit cardValue) = show cardValue ++ " of " ++ show suit

type Deck = [Card]
type Hand = [Card]


fullDeck :: Deck
fullDeck = [Card suit cardVal | suit <- [Diamonds ..], cardVal <- [Six .. ]]

shuffleDeck :: MonadRandom m => m Deck
shuffleDeck = runRVar (shuffle fullDeck) StdRandom


data Score = ScoreValue Int | Moscow | Overflow Int deriving (Show, Eq, Ord)

scoreToInt :: Score -> Int
scoreToInt score =
    case score of
        Moscow       -> 22
        ScoreValue v -> v
        Overflow _   -> -1

countHandScore :: Hand -> Score
countHandScore [Card _ Ace, Card _ Ace] = Moscow
countHandScore hand = let score = foldr (+) 0 $ fmap cardValues hand
                      in case score > 21 of
                          True  -> Overflow score
                          False -> ScoreValue score

dealCards :: Int -> Deck -> (Hand, Deck)
dealCards number deck = (take number deck, drop number deck)
