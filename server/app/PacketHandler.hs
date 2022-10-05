module PacketHandler (
    Packet(..),
    IPAddress(..),
    PacketChan,
    OutPacketChan,
    InPacketChan,
    parsePacket,
    sendPacket,
) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS (ByteString, length, empty)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Int (Int8, Int64)
import Data.Binary.Get (getInt64be, getInt8, runGet, getByteString, Get)
import System.IO (Handle, hPutStr)
import Data.Binary.Put (runPut, putInt8, putInt64be, putByteString, Put)
import Network.Socket (SockAddr)
import Control.Concurrent (Chan)

data Packet = Packet {
    packetType :: !Int8,
    packetData :: !String
} deriving (Show)

data IPAddress = BroadcastIP | IPAddress SockAddr deriving (Show)

type PacketChan = Chan (Packet, IPAddress)
type OutPacketChan = PacketChan
type InPacketChan = PacketChan


parsePacket :: Handle -> IO Packet
parsePacket hdl = do
    packetIDBS <- BS8.hGet hdl 1
    packetLengthBS <- BS8.hGet hdl 8
    let packetLength = fromIntegral $ runGet getInt64be . BSL.fromStrict $ packetLengthBS
    packetDataBS <- BS8.hGet hdl packetLength

    return $ runGet getPacket $ BSL.fromStrict (packetIDBS <> packetLengthBS <> packetDataBS)

sendPacket :: Packet -> Handle -> IO ()
sendPacket toSend hdl = hPutStr hdl (BSL8.unpack . runPut $  putPacket toSend)

getPacket :: Get Packet
getPacket = do
    packetID <- getInt8
    packet_length <- getInt64be
    packetData <- BS8.unpack <$> getByteString (fromIntegral packet_length)

    return $! Packet packetID packetData

putPacket :: Packet -> Put
putPacket (Packet packetID packetData) = do
    putInt8 packetID
    putInt64be (fromIntegral . length $ packetData :: Int64)
    putByteString $ BS8.pack packetData