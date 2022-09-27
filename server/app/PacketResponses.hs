module PacketResponses (
    sendTileMapPackets
) where

import System.IO
import SOCMap (SOCMap)
import GHC.IO.Handle (Handle)
import PacketHandler (sendPacket, Packet (Packet))
import qualified Data.Map as Map
import Control.Monad (sequence)

sendTileMapPackets :: SOCMap -> Handle -> IO ()
sendTileMapPackets socM hdl = mapM_ sendTile (Map.toList socM)
    where sendTile ((x, y), tileType) = sendPacket (Packet 3 (show x ++ " " ++ show y ++ " " ++ tileType)) hdl
