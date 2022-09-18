#include "HexMap.h"

#include "base/GameState.h"
#include "base/GameWindow.h"

const std::array<std::string, client::HexTileType::HEX_TILE_MAX> texture_names = {
    "tile_desert",
    "tile_desert",
    "tile_desert",
    "tile_desert",
    "tile_desert",
    "tile_desert"
};

client::HexMap::HexMap() : size{5, 5}, zoom{1.0f} {
    for (int i = 0; i < this->size.x; i++) {
        for (int j = 0; j < this->size.y - std::abs(i - this->size.y / 2); j++) {
            this->tilemap.push_back(HexTile{j, i, HexTileType::DESERT});
        }
    }
    this->hex_shape.setPointCount(6);
    this->hex_shape.setRadius(HEX_SIZE / 2.0f);
}

void client::HexMap::render(GameWindow& game_window, GameState& game_state) {
    for (const HexTile& tile : this->tilemap) {
        this->hex_shape.setTexture(&game_state.get_texture_manager().get_texture(texture_names[tile.type]));
        int tile_x = tile.y > this->size.y / 2 ? tile.x - this->size.y / 2 : tile.x - tile.y;
        float pixel_x = HEX_SIZE / 2.0f * (std::sqrt(3.0f) * tile_x + std::sqrt(3.0f) / 2.0f * tile.y);
        float pixel_y = HEX_SIZE / 2.0f * (3.0f / 2.0f * tile.y);
        this->hex_shape.setPosition(pixel_x, pixel_y);
        game_window.draw(this->hex_shape);
    }
}