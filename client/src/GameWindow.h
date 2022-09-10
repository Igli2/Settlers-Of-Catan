#pragma once

#include <SFML/Graphics.hpp>

#include "GameState.h"

namespace client {
    class GameWindow : public sf::RenderWindow {
        public:
            GameWindow(client::GameState& game_state);
            void game_loop(client::GameState& game_state);
    };
}