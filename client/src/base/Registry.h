#pragma once

#include <array>
#include <string>

namespace client {
    enum HexTileType {
        DESERT,
        FIELD,
        HILL,
        FOREST,
        MOUNTAIN,
        PASTURE,
        HEX_TILE_MAX
    };

    const std::array<std::string, client::HexTileType::HEX_TILE_MAX> tile_texture_names = {
        "tile_desert",
        "tile_field",
        "tile_hill",
        "tile_forest",
        "tile_mountain",
        "tile_pasture"
    };

    enum ResourceType {
        GRAIN,
        BRICK,
        ORE,
        WOOL,
        LUMBER,
        RESOURCE_MAX
    };

    const std::array<std::string, client::ResourceType::RESOURCE_MAX> resource_texture_names = {
        "grain",
        "brick",
        "ore",
        "wool",
        "lumber"
    };
}