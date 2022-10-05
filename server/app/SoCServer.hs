module Main where

import Network.Socket
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception (handle, SomeException (SomeException))
import System.IO (IOMode(ReadWriteMode), hSetBuffering, BufferMode (NoBuffering), hClose, hPutStr)
import Data.Binary.Put (runPut)

import ConfigLoader
import PacketHandler (Packet(packetType), parsePacket, sendPacket, IPAddress (IPAddress, BroadcastIP), InPacketChan, OutPacketChan, PacketChan)
import SOCMap (createMapFromConfig, SOCMap)
import PacketResponses (sendTileMapPackets)
import GHC.Base (IO(IO), when)

main :: IO ()
main = do
    socCfg <- parseConfig "config.yml" :: IO GameConfig
    mapCfg <- parseConfig (mapFile  socCfg) :: IO MapConfig
    let socM = createMapFromConfig mapCfg

    sock <- socket AF_INET Stream 0
    broadcastChan <- newChan :: IO PacketChan
    incomingChan <- newChan :: IO PacketChan

    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromInteger . serverPort  $ socCfg) 0)
    listen sock 4

    packetSender <- forkIO . fix $  \loop -> do
        toProcess <- readChan incomingChan
        putStrLn $ "Received packet: " ++ show toProcess

        case packetType . fst $ toProcess of
            2 -> sendTileMapPackets socM broadcastChan (snd toProcess)
            _ -> return () -- err packet

        loop

    broadcastClearer <- forkIO . fix $ \loop -> do
        _ <- readChan  broadcastChan
        loop

    serverLoop (playerCount socCfg) sock broadcastChan incomingChan

    killThread packetSender
    killThread broadcastClearer

serverLoop :: Int -> Socket -> OutPacketChan -> InPacketChan -> IO ()
serverLoop sockCount sock recvChan sendChan = do
    connection <- accept sock
    forkIO (handleConnection connection recvChan sendChan)

    serverLoop (sockCount - 1) sock recvChan sendChan

handleConnection :: (Socket, SockAddr) -> OutPacketChan -> InPacketChan -> IO ()
handleConnection (sock, addr) recvChan sendChan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    putStrLn $ "Client connected: " ++ show addr

    listenerChan <- dupChan recvChan
    listener <- forkIO . fix $ \loop -> do
        toSend <- readChan listenerChan

        case snd toSend of
            BroadcastIP        -> sendPacket (fst toSend) hdl
            IPAddress recvAddr -> when (recvAddr == addr) $ sendPacket (fst toSend) hdl

        loop

    handle (\(SomeException _) -> return ()) . fix $ \loop -> do
        packet <- parsePacket hdl 
        case packetType packet of
            0  -> return ()
            _   -> sendQuery packet >> loop

    killThread listener

    putStrLn $ "Client with ip " ++ show addr ++ " disconnected"

    where sendQuery msg = writeChan sendChan (msg, IPAddress addr)