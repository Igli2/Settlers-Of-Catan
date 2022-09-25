#pragma once

#include <SFML/Graphics.hpp>

namespace client {
    class GameState;

    class Clickable {
        private:
            sf::Rect<int> click_area;
        public:
            Clickable(sf::Rect<int> area); // register manually
            Clickable(GameState& game_state, sf::Rect<int> area); // registers automatically
            ~Clickable() = default;
            virtual bool on_click(GameState& game_state, sf::Mouse::Button button) { return false; } // press and release both inside area
            virtual bool on_press(GameState& game_state, sf::Mouse::Button button) { return false; } // move button down
            virtual bool on_release(GameState& game_state, sf::Mouse::Button button) { return false; } // mouse button up
            virtual bool on_enter(GameState& game_state) { return false; } // mouse move event into area
            virtual bool on_exit(GameState& game_state) { return false; } // mouse move out of area
            virtual bool on_move(GameState& game_state) { return false; } // any move inside of area
            bool contains(const sf::Vector2i& point);
            void set_area(sf::Rect<int> rect);
    };
}