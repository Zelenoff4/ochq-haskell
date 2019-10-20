module BetProcessing where

import Graphics.Gloss.Interface.IO.Interact

import Game

listOfKeyNumbers :: [Key]
listOfKeyNumbers =
    [ Char '0'
    , Char '1'
    , Char '2'
    , Char '3'
    , Char '4'
    , Char '5'
    , Char '6'
    , Char '7'
    , Char '8'
    , Char '9'
    , SpecialKey KeyDelete
    , SpecialKey KeyBackspace
    ]


isKeyPad :: Key -> Bool
-- isKeyPad key = (/= 0) . length . filter (==key) $ listOfKeyNumbers
isKeyPad key = elem key listOfKeyNumbers


keyPadToNumber :: Key -> Int
keyPadToNumber key =
    head
        [x | x <- [0 .. length listOfKeyNumbers], key == listOfKeyNumbers !! x]


deleteDigitFromBet :: Bet -> Bet
deleteDigitFromBet bet =
    if null $ bet
        then [0]
        else tail bet


processNumber :: Key -> Game -> IO Game
processNumber key game =
    let curBet = currentBet game
        newBet = deleteDigitFromBet curBet
    in case key of
        SpecialKey KeyDelete    -> return game {currentBet = newBet }
        SpecialKey KeyBackspace -> return game {currentBet = newBet }
        _                       -> return game {currentBet = newBet'}
                                   where
                                       element = keyPadToNumber key
                                       newBet' = if (length curBet) >= 5
                                                 then replicate 5 9
                                                 else (element : curBet)
