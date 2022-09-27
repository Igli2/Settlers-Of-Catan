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
import SOCMap (createMapFromConfig, SOCMap)
import PacketResponses (sendTileMapPackets)

serverPort :: PortNumber
serverPort = 50140

main :: IO ()
main = do
    socCfg <- parseConfig "config.yml" :: IO GameConfig
    mapCfg <- parseConfig (mapFile  socCfg) :: IO MapConfig
    let socM = createMapFromConfig mapCfg

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

    serverLoop sock broadcastChan incomingChan socM

    killThread packetSender
    killThread broadcastClearer

serverLoop :: Socket -> Chan Packet -> Chan Packet -> SOCMap -> IO ()
serverLoop sock broadcastChan incomingChan socM = do
    connection <- accept sock
    forkIO (handleConnection connection broadcastChan incomingChan socM)

    serverLoop sock broadcastChan incomingChan socM

handleConnection :: (Socket, SockAddr) -> Chan Packet -> Chan Packet -> SOCMap -> IO ()
handleConnection (sock, addr) broadcastChan incomingChan socM = do
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
            2 -> sendTileMapPackets socM hdl >> loop
            _   -> broadcast packet >> loop

    killThread listener

    putStrLn $ "Client with ip " ++ show addr ++ " disconnected"

    where broadcast msg = writeChan incomingChan msg