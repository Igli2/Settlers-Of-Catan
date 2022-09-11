{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import System.IO
import Control.Concurrent

serverPort :: PortNumber
serverPort = 50140

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet serverPort 0)
    listen sock 4
    serverLoop sock

serverLoop :: Socket -> IO ()
serverLoop sock = do
    connection <- accept sock
    forkIO (handleConnection connection)
    serverLoop sock

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (sock, addr) = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering

    hPutStrLn handle "Hello from server!"
    putStrLn $ "Client connected: " ++ show addr

    hClose handle