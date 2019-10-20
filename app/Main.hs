module Main where

import Graphics.Gloss.Interface.IO.Game (playIO)

import Game (menuScreen, window, backgroundColor)
import Rendering (gameAsPicture)
import Logic (transformGame)

main :: IO ()
main = do
    playIO
        window
        backgroundColor
        30
        menuScreen
        gameAsPicture
        transformGame
        (\_ y -> return y)
