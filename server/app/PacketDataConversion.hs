module PacketDataConversion (
    unpackCornerPlaceablePacket
) where
import PacketTransfer (Packet (Packet))
import GameData (Position)

unpackCornerPlaceablePacket :: Packet -> (String, Position)
unpackCornerPlaceablePacket (Packet 4 pData) = let [pType, x, y] = words pData in (pType, (read x, read y)) 
unpackCornerPlaceablePacket _ = undefined
