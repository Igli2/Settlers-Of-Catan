module PacketResponses (
    sendErrorPacket,
    sendPacketResponse
) where

import GameData (TileMap)
import GHC.IO.Handle (Handle)
import PacketTransfer (Packet (Packet), OutPacketChan, IPAddress)
import qualified Data.Map as Map
import Control.Monad (sequence)
import Control.Concurrent (writeChan)
import GameState (GameState (tileMap))

sendTileMapPackets :: TileMap -> OutPacketChan -> IPAddress -> IO ()
sendTileMapPackets socM sendChan receiver = mapM_ sendTile (Map.toList socM)
    where sendTile ((x, y), tileType) = sendPacket (Packet 3 (show x ++ " " ++ show y ++ " " ++ tileType), receiver)
          sendPacket = writeChan sendChan

sendErrorPacket :: OutPacketChan -> IPAddress -> String -> IO ()
sendErrorPacket sendChan receiver errMsg = writeChan sendChan (Packet 1 errMsg, receiver) 

sendPacketResponse :: Packet -> IPAddress -> GameState -> OutPacketChan -> IO ()
sendPacketResponse (Packet 2 _) ipAddr gs sendChan = sendTileMapPackets (tileMap gs) sendChan ipAddr
sendPacketResponse _ _ _ _ = return ()