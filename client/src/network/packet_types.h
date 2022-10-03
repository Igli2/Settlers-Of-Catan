#pragma once

namespace network {
    enum PacketType {
        DISCONNECT,
        REGISTER_PLAYER,
        GET_TILEMAP,
        TILE_DATA,
        PLACE_BUILDING
    };
}