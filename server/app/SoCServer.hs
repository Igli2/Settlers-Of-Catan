{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Monad (when)
import Control.Exception (handle, SomeException (SomeException))

type Packet = String

serverPort :: PortNumber
serverPort = 50140

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    broadcastChan <- newChan
    incomingChan <- newChan
    
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet serverPort 0)
    listen sock 4

    packetSender <- forkIO . fix $  \loop -> do
        msg <- readChan incomingChan
        writeChan broadcastChan msg
        putStrLn $ "Received packet: " ++ msg
        loop

    broadcastClearer <- forkIO . fix $ \loop -> do
        _ <- readChan  broadcastChan
        loop

    serverLoop sock broadcastChan incomingChan

    killThread packetSender
    killThread broadcastClearer

serverLoop :: Socket -> Chan Packet -> Chan Packet -> IO ()
serverLoop sock broadcastChan incomingChan = do
    connection <- accept sock
    forkIO (handleConnection connection broadcastChan incomingChan)

    serverLoop sock broadcastChan incomingChan

handleConnection :: (Socket, SockAddr) -> Chan Packet -> Chan Packet -> IO ()
handleConnection (sock, addr) broadcastChan incomingChan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    putStrLn $ "Client connected: " ++ show addr

    listenerChan <- dupChan broadcastChan
    listener <- forkIO . fix $ \loop -> do
        msg <- readChan listenerChan
        hPutStrLn hdl $ "Received: " ++ msg
        loop

    handle (\(SomeException _) -> return ()) . fix $ \loop -> do
        line <- fmap init . hGetLine $ hdl
        case line of
            "quit"  -> return ()
            _       -> broadcast line >> loop

    killThread listener
    hClose hdl
    
    putStrLn $ "Client with ip " ++ show addr ++ " disconnected"

    where broadcast msg = writeChan incomingChan msg