#pragma once

#include <SFML/Graphics.hpp>
#include <string>

namespace client {
    class GameState;

    class VictoryPoint {
        private:
            sf::Sprite image;
            sf::Text text;
            int points;
            int max_points;
        public:
            VictoryPoint(GameState& game_state);
            void set_points_required(int points);
            void add_points(int points);
            const sf::Sprite& get_sprite();
            const sf::Text& get_text();
    };
}