#pragma once

#include <SFML/Graphics.hpp>
#include <vector>
#include <array>
#include <cmath>

#include "base/Registry.h"

namespace client {
    class GameState;
    class GameWindow;

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