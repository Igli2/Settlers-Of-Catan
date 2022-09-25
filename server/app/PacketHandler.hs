module PacketHandler (
    Packet,
    parsePacket,
    putPacket,
    packetData,
    packetType
) where

import Data.Int (Int8, Int64)
import Data.Binary.Get (getInt64be, getInt8, runGet, getByteString, Get)
import Data.Binary (encode, Put)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS (ByteString, length, empty)
import Network.Socket.ByteString (recv)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import Control.Exception (Exception, throw)
import Data.Data (Typeable)
import System.IO (Handle)
import Data.Binary.Put (runPut, putInt8, putInt64le, putByteString)

data Packet = Packet {
    packetType :: !Int8,
    packetData :: !String
} deriving (Show)

data SocketException = SocketUnavailable deriving (Show, Typeable)
instance Exception SocketException

getPacket :: Get Packet
getPacket = do
    packetID <- getInt8
    packet_length <- getInt64be
    packetData <- BS8.unpack <$> getByteString (fromIntegral packet_length)

    return $! Packet packetID packetData

parsePacket :: Handle -> IO Packet
parsePacket hdl = do
    packetIDBS <- BS8.hGet hdl 1
    packetLengthBS <- BS8.hGet hdl 8
    let packetLength = fromIntegral $ runGet getInt64be . BSL.fromStrict $ packetLengthBS
    packetDataBS <- BS8.hGet hdl packetLength

    return $ runGet getPacket $ BSL.fromStrict (packetIDBS <> packetLengthBS <> packetDataBS)

putPacket :: Packet -> Put
putPacket (Packet packetID packetData) = do
    putInt8 packetID
    putInt64le (fromIntegral . length $ packetData :: Int64)
    putByteString $ BS8.pack packetData