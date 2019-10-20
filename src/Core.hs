module Core where

import Data.List
import Text.Printf
import qualified Data.Map as Map

import Cards

playersMap :: Map.Map String Money
playersMap = Map.fromList [(,) "Maxim" 100, (,) "Test" 1000]


getBankMoney :: String -> Money
getBankMoney playerName =
    case Map.lookup playerName playersMap of
        Nothing -> 100 :: Int
        Just value -> value

dealCards :: Int -> Deck -> (Hand, Deck)
dealCards number deck = (take number deck, drop number deck)

type Money = Int
data Outcome = Loss | Push | Win deriving (Show, Eq)
data Score = Value Int | Moscow | Overflow Int deriving (Show, Eq, Ord)

countHandScore :: Hand -> Score
countHandScore [Card _ Ace, Card _ Ace] = Moscow
countHandScore hand = Value $ foldr (+) 0 $ fmap cardValues hand

showHandAndScore :: String -> Hand -> Maybe (String, Score)
showHandAndScore player hand = do
    let score = countHandScore hand
    case score of
        Moscow -> Just (player ++ " hand: [Ace, Ace], Score: Moscow", Moscow)
        Value v -> if v >= 22
                      then Nothing
                      else Just $ (player ++ " hand: " ++ show hand ++ ", Score: " ++ (show v),
                                  Value v)

playerTurn :: Hand -> Deck -> IO (Hand, Deck)
playerTurn [] _ = return ([], [])
playerTurn hand deck = do
    result <- return $ showHandAndScore "Player" hand
    case result of
        Nothing               -> return ([], deck)
        Just (message, score) -> do
            putStrLn message
            putStrLn "Do you want to continue? (y/n)"
            responce <- getChar
            putStrLn ""
            case responce of
                'n'       -> do
                    putStrLn $ "Your final hand: " ++ show hand ++
                                ", Score: " ++ (show score)
                    return (hand, deck)
                'y'       -> do
                                let (newCard, remainingDeck) = dealCards 1 deck
                                playerTurn (hand ++ newCard) remainingDeck
                otherwise -> playerTurn hand deck

fetchScore :: Score -> Int
fetchScore (Value x) = x
fetchScore Moscow = 22


dealerTurn :: Hand -> Deck -> IO Hand
dealerTurn [] _ = return []
dealerTurn dHand deck = do
    result <- return $ showHandAndScore "Dealer" dHand
    case result of
        Nothing               -> return []
        Just (message, score) -> do
            let dScore = fetchScore $ countHandScore dHand
            case 17 >= dScore of
                True      -> do
                                let (newCard, remainingDeck) = dealCards 1 deck
                                dealerTurn (dHand ++ newCard) remainingDeck
                otherwise -> do
                                putStrLn message
                                return dHand


data Winner = Player (Score, Int) | Dealer Score deriving (Show)

decideWinner :: [Score] -> Score -> Winner
decideWinner pScores dScore =
    let winner@(pWinner, pNumber) = maxin $ map fetchScore pScores
        in case pWinner > fetchScore dScore of
            True -> Player (Value pWinner, pNumber)
            otherwise -> Dealer dScore
    where
        maxin xs =
            foldr (\ (x,y) acc -> if (x == maximum xs) then (x,y) else acc)
            (0,head xs) (zip xs [0..])


countMoneyMade :: Money -> Winner -> Money
countMoneyMade bet (Dealer _) = -1 * bet
countMoneyMade bet (Player (Value _, _)) = bet
countMoneyMade bet (Player (Moscow, _)) = ceiling $ (1.5 :: Double) * fromIntegral bet



play :: Money -> IO Money
play bet = do
    shuffledDeck <- shuffleDeck
    let (playerHand, remainingDeck) = dealCards 2 shuffledDeck
        (dealerHand, remainingDeck') = dealCards 2 remainingDeck
    (finalPlayerHand, remainingDeck'') <- playerTurn playerHand remainingDeck'
    if finalPlayerHand == []
        then do
            putStrLn "Player overflown"
            return $ countMoneyMade bet (Dealer $ Value 0)
        else do
            finalDealerHand <- dealerTurn dealerHand remainingDeck''
            let (finalPlayerScore, finalDealerScore) =
                    (countHandScore finalPlayerHand, countHandScore finalDealerHand)
            if finalDealerHand == []
                then do
                    putStrLn "Dealer overflown"
                    return $ countMoneyMade bet (Player (finalPlayerScore, 0))
                else do
                    let winner = decideWinner [finalPlayerScore] finalDealerScore
                    return $ countMoneyMade bet winner


main = do
    putStrLn "Hello! Welcome to Russian BlackJack (aka 21). Pls, enter your name to begin"
    playerName <- getLine
    case playerName of
        "" -> do
            putStrLn "Pls try again, your name cannot be empty suka"
            main
        otherwise -> do
                        let availableMoney = getBankMoney playerName
                        putStrLn $ "Hai zyabl, your bank account is: " ++ show availableMoney
                        putStrLn "Available commands : play {amount} / leave"
                        gotLine <- getLine
                        let responce = words gotLine
                        case length responce of
                            2 -> play $ read $ responce !! 1
                            otherwise -> do
                                            putStrLn "Bye-bye gamer"
                                            return $ getBankMoney playerName
