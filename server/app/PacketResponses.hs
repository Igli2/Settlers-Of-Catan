module PacketResponses (
    sendTileMapPackets
) where

import System.IO
import SOCMap (SOCMap)
import GHC.IO.Handle (Handle)
import PacketHandler (Packet (Packet), OutPacketChan, IPAddress)
import qualified Data.Map as Map
import Control.Monad (sequence)
import Control.Concurrent (writeChan)

sendTileMapPackets :: SOCMap -> OutPacketChan -> IPAddress -> IO ()
sendTileMapPackets socM sendChan receiver = mapM_ sendTile (Map.toList socM)
    where sendTile ((x, y), tileType) = sendPacket (Packet 3 (show x ++ " " ++ show y ++ " " ++ tileType), receiver)
          sendPacket = writeChan sendChan