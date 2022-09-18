#pragma once

#include <SFML/Graphics.hpp>

namespace client {
    class GameState;

    class KeyboardHandler {
        const float MOVEMENT_SPEED = 10.0f;
        private:
            bool key_states[4];
            GameState& game_state;
        public:
            KeyboardHandler(GameState& game_state);
            void on_key_pressed(sf::Event::KeyEvent event_parameters);
            void on_key_released(sf::Event::KeyEvent event_parameters);
            void update_view();
    };
}