#pragma once

#include <SFML/Graphics.hpp>

#include "Clickable.h"

namespace client {
    class GameState;

    class MouseHandler {
        private:
            std::vector<Clickable*> clickables; // unregister when deleted
            GameState& game_state;
            sf::Vector2i last_mouse_pressed;
            Clickable* last_hovered;

            void call_on_move(const sf::Vector2i& pos);
        public:
            MouseHandler(GameState& game_state);
            void call_mouse_press(sf::Mouse::Button button, const sf::Vector2i& pos); // calls clickable handlers
            void call_mouse_release(sf::Mouse::Button button, const sf::Vector2i& pos); // calls clickable handlers
            void call_mouse_move(const sf::Vector2i& pos); // calls clickable handlers
            void add_clickable_object(Clickable* c);
            void add_clickable_object(Clickable* c, unsigned int priority); // priority defines at which index the clickable is inserted, useful for clickable overlays
            void remove_clickable_object(Clickable* c);
    };
}