module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv)
import Control.Exception
import Control.Concurrent.MVar
-- import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Game (backgroundColor, window, gameSocket, decodeGame)
-- import Cards
import Logic
import Rendering

type Port   = String
type Host = String


main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  (ip, port) <- case args of ip:port:[] -> return (ip, port)
                             _          -> do
                                             putStrLn "IncorrectArguments"
                                             putStrLn "Expected <ip> <port>"
                                             exitFailure

  addr <- resolve ip port
  -- putStrLn $ show addr
  bracket (open addr) close runClient


resolve :: Host -> Port -> IO AddrInfo
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  (addrInfo : _) <- getAddrInfo (Just hints) (Just host) (Just port)
  return addrInfo

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  return sock

runClient :: Socket -> IO ()
runClient sock = do
    game <- fmap decodeGame (recv sock 20000)
    recvMVar <- newMVar (game {gameSocket = Just sock})
    -- let game = game {gameSocket = sock}
    -- threadId <- forkIO $ recvLoop recvMVar sock
    playIO
        window
        backgroundColor
        30
        game {gameSocket = Just sock}
        gameAsPicture
        transformGame
        -- (\_ y -> return y)
        (takeGameFromServer recvMVar)
