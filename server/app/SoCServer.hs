{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception (handle, SomeException (SomeException))
import System.IO (IOMode(ReadWriteMode), hSetBuffering, BufferMode (NoBuffering), hClose, hPutStr)
import Data.Binary.Put (runPut)

import ConfigLoader

import PacketHandler (Packet(packetType), parsePacket, sendPacket)
import SOCMap (createMapFromConfig)

serverPort :: PortNumber
serverPort = 50140

main :: IO ()
main = do
    socCfg <- parseConfig "config.yml" :: IO GameConfig
    mapCfg <- parseConfig (mapFile  socCfg) :: IO MapConfig
    let socMap = createMapFromConfig mapCfg

    print socCfg
    print mapCfg
    print socMap

    sock <- socket AF_INET Stream 0
    broadcastChan <- newChan :: IO (Chan Packet)
    incomingChan <- newChan :: IO (Chan Packet)

    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet serverPort 0)
    listen sock 4

    packetSender <- forkIO . fix $  \loop -> do
        toSend <- readChan incomingChan
        writeChan broadcastChan toSend
        putStrLn $ "Received packet: " ++ show toSend
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
        toSend <- readChan listenerChan
        sendPacket toSend hdl
        loop

    handle (\(SomeException _) -> return ()) . fix $ \loop -> do
        packet <- parsePacket hdl 
        case packetType packet of
            0  -> return ()
            _   -> broadcast packet >> loop

    killThread listener

    putStrLn $ "Client with ip " ++ show addr ++ " disconnected"

    where broadcast msg = writeChan incomingChan msg