module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll)
import Control.Exception
-- import Control.Concurrent.MVar
import System.Environment
import System.Exit

-- import Cards
import Game


main :: IO ()
main = withSocketsDo $ do
  args            <- getArgs
  (port, players) <- case args of port:players:[] -> return (port, read players :: Int)
                                  _               -> do
                                                       putStrLn "Incorrect Arguments"
                                                       putStrLn "Expected <port>"
                                                       exitFailure
  addr <- resolve port
  -- putStrLn $ show players
  -- putStrLn $ show addr
  bracket (open players addr) close (runServer players menuScreen)

resolve :: String -> IO AddrInfo
resolve port = do
 let hints = defaultHints { addrFlags = [AI_PASSIVE]
                          , addrSocketType = Stream
                          }
 addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
 return addr

open :: Int -> AddrInfo -> IO Socket
open players addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr players
    bind sock (addrAddress addr)
    listen sock players
    return sock


-- runServer :: Int -> Game -> Socket -> IO ()
-- runServer players game socket =
--     case players of
--         1 -> do
--             (player1Sock, _) <- accept socket
--             loopSolo player1Sock game
--      -- 2 -> do
--      --   (player1Sock, _) <- accept socket
--      --   (player2Sock, _) <- accept socket
--      --   loop player1Sock player2Sock (nullScore $ changeMode state)
--      _ -> putStrLn "You and your argument invaid!"

runServer :: Int -> Game -> Socket -> IO ()
runServer players game sock = do
  case players of
    1 -> do
      (player1Sock, _) <- accept sock
      loopSolo player1Sock game
    2 -> do
      (player1Sock, _) <- accept sock
      (player2Sock, _) <- accept sock
      loop player1Sock player2Sock game
      putStrLn "asd"
    _ -> putStrLn "You and your argument invaid!"

loopSolo :: Socket -> Game -> IO ()
loopSolo sock game = do
    sendAll sock (encodeGame game)
    -- game' <- fmap decodeGame $ recv sock 20000
    putStrLn "zxc"

loop :: Socket -> Socket -> Game -> IO ()
loop = undefined
