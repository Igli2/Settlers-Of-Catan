module Main where

import Network.Socket
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Exception (handle, SomeException (SomeException))
import System.IO (IOMode(ReadWriteMode), hSetBuffering, BufferMode (NoBuffering), hClose, hPutStr)
import Data.Binary.Put (runPut)

import ConfigLoader
import PacketTransfer (Packet(packetType), parsePacket, sendPacket, IPAddress (IPAddress, BroadcastIP), InPacketChan, OutPacketChan, PacketChan)
import GameData(TileMap, createCornerMap, Player (Player))
import PacketResponses (sendErrorPacket, sendPacketResponse)
import GHC.Base (IO(IO), when)
import GameState (GameState (tileMap, GameState), applyModifier, placeSettlement)
import qualified Data.Map as Map
import Data.Either (fromLeft, isLeft)

main :: IO ()
main = do
    socCfg <- parseConfig "config.yml" :: IO GameConfig
    mapCfg <- parseConfig (mapFile  socCfg) :: IO MapConfig

    let socM = createMapFromConfig mapCfg
    let corM = createCornerMap socM
    let gs = GameState (Map.singleton "Test" (Player Map.empty Map.empty 0 (10, 10, 10))) socM corM

    sock <- socket AF_INET Stream 0
    broadcastChan <- newChan :: IO PacketChan
    incomingChan <- newChan :: IO PacketChan

    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromInteger . serverPort  $ socCfg) 0)
    listen sock 4

    packetHandler <- forkIO $ packetHandlerLoop gs incomingChan broadcastChan

    broadcastClearer <- forkIO . fix $ \loop -> do
        _ <- readChan  broadcastChan
        loop

    serverLoop (playerCount socCfg) sock broadcastChan incomingChan

    killThread packetHandler
    killThread broadcastClearer

packetHandlerLoop :: GameState -> InPacketChan -> OutPacketChan -> IO ()
packetHandlerLoop gs recvChan broadcastChan = do 
    toProcess <- readChan recvChan
    putStrLn $ "Received packet: " ++ show toProcess

    gs' <- handlePacket gs toProcess broadcastChan

    packetHandlerLoop gs' recvChan broadcastChan

handlePacket :: GameState -> (Packet, IPAddress) -> OutPacketChan -> IO GameState
handlePacket gs (p, ipAddr) broadcastChan = do
    unwrapModifiedGameState $ case packetType p of
        2 -> Right gs
        4 -> applyModifier (placeSettlement "Test" (0, 0)) gs
        _ -> Left "Unknown packet type!"

    where unwrapModifiedGameState = either onError onSuccess
          onSuccess gs' = sendPacketResponse p ipAddr gs' broadcastChan >> return gs'
          onError err = sendErrorPacket broadcastChan ipAddr err >> return gs

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