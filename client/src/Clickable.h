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
            virtual bool on_click(sf::Mouse::Button button) { return false; } // press and release both inside area
            virtual bool on_press(sf::Mouse::Button button) { return false; } // move button down
            virtual bool on_release(sf::Mouse::Button button) { return false; } // mouse button up
            virtual bool on_enter() { return false; } // mouse move event into area
            virtual bool on_exit() { return false; } // mouse move out of area
            bool contains(const sf::Vector2i& point);
            void set_area(sf::Rect<int> rect);
    };
}