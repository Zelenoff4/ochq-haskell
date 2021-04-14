module Rendering where

import Graphics.Gloss ( Color
                      , makeColorI
                      , color
                      , scale
                      , Picture(..)
                      , pictures
                      , rectangleSolid
                      , rectangleWire
                      , line
                      , translate
                      , white
                      , red
                      , cyan
                      , orange
                      , green
                      , magenta
                      )

import Cards
import Game


winColor :: Color
winColor = makeColorI 16 195 44 0

loseColor :: Color
loseColor = makeColorI 145 3 3 1

snapPictureToCell :: Picture -> (Float, Float) -> Float -> Picture
snapPictureToCell picture (row, column) pos = translate x y picture
    where
        x = column * cellWidth + cellWidth * pos
        y = row * cellHeight + cellHeight * pos

drawCard :: Picture -> (Int, Int) -> Float -> Int -> Picture
drawCard pic (row, column) pos sep = translate x y pic
    where
        x = fromIntegral column * cellWidth + cellWidth * pos + (cellWidth * fromIntegral sep)
        y = fromIntegral row * cellHeight + cellHeight * pos

sepLines :: Picture
sepLines =
    pictures
    [ snapPictureToCell rect (2, fromIntegral n / 2) 0
    , snapPictureToCell rect (5, fromIntegral n / 2) 0
    ]
    where
        rHeight = 10
        rWidth  = fromIntegral screenWidth
        rect    = rectangleSolid rWidth rHeight

deckPile :: [(String, Picture)] -> Picture
deckPile picPairs =
    snapPictureToCell cardBack (fromIntegral $ div n 2, 0) 0.5
    where
        cardBack = case lookup "images/card_back.bmp" picPairs of
                        Just pic -> pic
                        Nothing  -> Blank

playerCards :: Hand -> [(String, Picture)] -> Picture
playerCards hand images = pictures $ helpDrawCards [] names images
    where
        names = map transformCardToPath hand
        
        helpDrawCards :: [Picture] -> [String] -> [(String, Picture)] -> [Picture]
        helpDrawCards ans [] _                   = ans
        helpDrawCards ans (value : hand') images' =
            case lookup value images of
                Just pic -> helpDrawCards
                                ((drawCard pic (1, 0) 0.5 (length ans)) : ans)
                                hand'
                                images'
                Nothing -> [Blank]

dealerCards :: Hand -> [(String, Picture)] -> Picture
dealerCards hand images = pictures $ helpDrawCards [] names images
    where
        names = map transformCardToPath hand
        helpDrawCards :: [Picture] -> [String] -> [(String, Picture)] -> [Picture]
        helpDrawCards ans [] _                    = ans
        helpDrawCards ans (value : hand') images' =
            case lookup value images of
                Just pic -> helpDrawCards
                                ((drawCard pic (n - 2, 0) 0.5 (length ans)) : ans)
                                 hand'
                                 images'
                Nothing  -> [Blank]

playerScore :: Hand -> Picture
playerScore hand =
    pictures [ snapPictureToCell backWhiteRect (1, fromIntegral $ n - 1) 0
             , snapPictureToCell pic (0, fromIntegral $ n - 2) 0.5
             ]
    where
        pic = case countHandScore hand of
                    Moscow       -> Text "AA"
                    ScoreValue 0 -> Blank
                    ScoreValue v -> Text $ show v
                    Overflow v   -> Text $ show v
        backWhiteRect = if countHandScore hand == (ScoreValue 0)
                        then Blank
                        else color white $ rectangleSolid (2 * cellWidth) (2 * cellHeight)

dealerScore :: Hand -> Picture
dealerScore hand =
    pictures [ snapPictureToCell backWhiteRect (fromIntegral $ n - 1, fromIntegral $ n - 1) 0
             , snapPictureToCell pic (fromIntegral $ n - 2, fromIntegral $ n - 2) 0.5
             ]
    where
        pic = case countHandScore hand of
                    Moscow       -> Text "AA"
                    ScoreValue 0 -> Blank
                    ScoreValue v -> Text $ show v
                    Overflow v   -> Text $ show v

        backWhiteRect = color white $ rectangleSolid (2 * cellWidth) (2 * cellHeight)

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

boardAsPicture :: Game -> Picture
boardAsPicture game =
    pictures [ deckPile cImages
             , playerCards pHand cImages
             , playerScore pHand
             -- , boardGrid
             , sepLines
             ]
    where
        pHand   = playerHand game
        cImages = cardImages game

resultColors :: Hand -> Hand -> (Color, Color)
resultColors playerHand' dealerHand' =
    if playerScore' > dealerScore'
        then (winColor, loseColor)
        else (loseColor, winColor)
    where
        playerScore' = scoreToInt . countHandScore $ playerHand'
        dealerScore' = scoreToInt . countHandScore $ dealerHand'

currentBankPicture :: Game -> Picture
currentBankPicture game =
    pictures [ snapPictureToCell bankText (middle, fromIntegral n - 2.75) 0.3
             , snapPictureToCell accountText (fromIntegral $ div n 2, 1) 0.25
             ]
    where
        bankText = betAsPicture (currentBankAmount game)
        middle = fromIntegral $ div n 2
        accountText = scale 0.5 0.5 $ Text "Your bank:"

finishedBoardAsPicture :: Game -> Picture
finishedBoardAsPicture game =
    pictures [ deckPile $ cImages
             , color (snd $ (resultColors pHand dHand)) $ dealerScore dHand
             , dealerCards dHand cImages
             , playerCards pHand cImages
             , color (fst $ (resultColors pHand dHand)) $ playerScore pHand
             , bigWhiteRect pHand dHand
             -- , boardGrid
             , nextAndFinishButtons
             , currentBankPicture game
             , sepLines
             ]
    where
        pHand   = playerHand game
        dHand   = dealerHand game
        cImages = cardImages game

nextAndFinishButtons :: Picture
nextAndFinishButtons =
    pictures [ snapPictureToCell nextButton (2, 1) 0.5
             , snapPictureToCell nextButton (2, 2) 0.5
             , snapPictureToCell finishButton (2, 4) 0.5
             , snapPictureToCell finishButton (2, 5) 0.5
             , snapPictureToCell retryText (2, 1) 0.3
             , snapPictureToCell finishText (2, 4) 0.3
             ]
    where
        nextButton   = color green $ rectangleSolid (1 * cellWidth) cellHeight
        finishButton = color red $ rectangleSolid (1 * cellWidth) cellHeight
        retryText    = scale 0.25 0.4 $ Text "New turn"
        finishText   = scale 0.4 0.4 $ Text "Leave"

bigWhiteRect :: Hand -> Hand -> Picture
bigWhiteRect playerHand' dealerHand' =
    pictures [ snapPictureToCell pic (middleCoords, middleCoords) 0.5
             , snapPictureToCell textSample (middleCoords + 1, middleCoords - 1) 0.5
             ]
    where
        pic          = color white $ rectangleSolid (5 * cellWidth) (2.95 * cellHeight)
        textSample   = scale 0.3 0.3
                       $ case playerScore' > dealerScore' of
                           True  -> Text "Player won"
                           False -> Text "Dealer won"
        middleCoords = fromIntegral $ div n 2
        playerScore'  = scoreToInt . countHandScore $ playerHand'
        dealerScore'  = scoreToInt . countHandScore $ dealerHand'

startButton :: Picture
startButton =
    pictures [ snapPictureToCell startBlock (coord, coord - 1) 0.5
             , snapPictureToCell startText (coord, coord - 2) 0.3
             ]
    where
        startBlock = color cyan $ rectangleSolid (3 * cellWidth) cellHeight
        startText  = scale 0.5 0.5 $ Text "Start"
        coord      = (+1) $ fromIntegral $ div n 2

exitButton :: Picture
exitButton =
    pictures [ snapPictureToCell exitBlock (coord, coord + 1) 0.5
             , snapPictureToCell exitText (coord, coord) 0.3
             ]
    where
        exitBlock = color orange $ rectangleSolid (3 * cellWidth) cellHeight
        exitText  = scale 0.5 0.5 $ Text "Exit"
        coord     = (fromIntegral $ div n 2) - 1

htpButton :: Picture
htpButton =
    pictures [ snapPictureToCell htpBlock (coord, coord) 0.5
             , snapPictureToCell htpText (coord, coord - 1) 0.3
             ]
    where
        htpBlock = color magenta $ rectangleSolid (3 * cellWidth) cellHeight
        htpText  = scale 0.4 0.4 $ Text "Help"
        coord    = fromIntegral $ div n 2

menuAsPicture :: Game -> Picture
menuAsPicture _ =
    pictures [ snapPictureToCell backgroundRect (middle, middle) 1.5
             -- , boardGrid
             , startButton
             , exitButton
             , htpButton
             ]
    where
        backgroundRect = color white $ rectangleSolid
                                        (fromIntegral screenWidth)
                                        (fromIntegral screenHeight)
        middle         = fromIntegral $ div n 2

betAsPicture :: Int -> Picture
betAsPicture bet = pic
    where
        pic = scale 0.4 0.4 $ Text . show $ bet

betScreenAsPicture :: Game -> Picture
betScreenAsPicture game =
    pictures [ boardAsPicture game
             , snapPictureToCell
                rectBorders
                (middle, middle)
                0
             , snapPictureToCell enterBetText (fromIntegral . (+1) $ div n 2, 1) 0.25
             , snapPictureToCell fieldBorder (fromIntegral $ div n 2, fromIntegral $ div n 2) 0.5
             , snapPictureToCell accountText (fromIntegral . (subtract 1) $ div n 2, 1) 0.25
             , snapPictureToCell bankText (fromIntegral . (subtract 1) $ div n 2, fromIntegral n - 2.75) 0.25
             , snapPictureToCell betPicture (fromIntegral $ div n 2, 2) 0.5
             ]
    where
        rectBorders = color white $ rectangleSolid
                        (fromIntegral (n - 2) * cellWidth)
                        (cellHeight * 3)
        fieldBorder = rectangleWire (4 * cellWidth) cellHeight
        enterBetText = scale 0.5 0.5 $ Text "Enter your bet"
        accountText = scale 0.5 0.5 $ Text "Your bank:"
        bankText = betAsPicture (currentBankAmount game)
        middle = fromIntegral n / 2
        betPicture = betAsPicture . betListToInt . currentBet $ game

helpScreenAsPicture :: Picture
helpScreenAsPicture =
    pictures [ snapPictureToCell backgroundRect (middle, middle) 0.5
             , snapPictureToCell howToPlayText (fn - 1, 0) 0.3
             , snapPictureToCell hitText (fn - 2, 0) 0.3
             , snapPictureToCell standText (fn - 3, 0) 0.3
             , snapPictureToCell escText (fn - 4, 0) 0.3
             , snapPictureToCell betText (2, 0) 0.3
             ]
    where
        howToPlayText  = scale 0.5 0.5 $ Text "How to play: "
        hitText        = scale 0.5 0.5 $ Text "Spacebar: Hit"
        standText      = scale 0.5 0.5 $ Text "Enter: Stand"
        escText        = scale 0.5 0.5 $ Text "ESC: exit to menu"
        betText        = scale 0.3 0.3 $ Text "Press enter to confirm bet"
        fn             = fromIntegral n
        middle         = fromIntegral $ div n 2
        backgroundRect = color white $ rectangleSolid
                                        (fromIntegral screenWidth)
                                        (fromIntegral screenHeight)

gameAsPicture :: Game -> IO Picture
gameAsPicture game = return $ translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Menu     -> menuAsPicture game
                    Settings -> helpScreenAsPicture
                    Betting  -> betScreenAsPicture game
                    Running  -> boardAsPicture game
                    Finished -> finishedBoardAsPicture game
