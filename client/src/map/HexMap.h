#pragma once

#include <SFML/Graphics.hpp>
#include <vector>
#include <array>
#include <cmath>

namespace client {
    class GameState;
    class GameWindow;

    enum HexTileType {
        DESERT,
        FIELD,
        HILL,
        FOREST,
        MOUNTAIN,
        PASTURE,
        HEX_TILE_MAX
    };

    struct HexTile {
        int x;
        int y;
        HexTileType type;
    };

    class HexMap {
        const float HEX_SIZE = 200.0f;
        private:
            std::vector<HexTile> tilemap;
            sf::CircleShape hex_shape;
            sf::Vector2i size;
        public:
            sf::Vector2f pos;
            float zoom;

            HexMap();
            void render(GameWindow& game_window, GameState& game_state);
    };
}