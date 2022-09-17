#pragma once

#include <SFML/Graphics.hpp>
#include <string>

#include "Clickable.h"
#include "OverlayAction.h"

namespace client {
    class GameState;
    class GameWindow;

    class Overlay : public Clickable {
        private:
            sf::Text title;
            sf::RectangleShape blocker;
            std::array<sf::RectangleShape, 4> background;
            bool is_active;
            OverlayAction accept;
            OverlayAction decline;
            sf::Rect<int> dimensions;
        public:
            Overlay(GameState& game_state, std::string title);
            void render(GameWindow& game_window, GameState& game_state);
            void set_active(GameState& game_state, bool state);
            void set_dimensions(GameState& game_state, sf::Rect<int> dimensions); // update inner widgets
            void on_accept(GameState& game_state);
            void on_decline(GameState& game_state);

            // block all mouse events for clickables not in overlay
            bool on_click(GameState& game_state, sf::Mouse::Button button) override { return true; }
            bool on_press(GameState& game_state, sf::Mouse::Button button) { return true; }
            bool on_release(GameState& game_state, sf::Mouse::Button button) { return true; }
            bool on_enter(GameState& game_state) { return true; }
            bool on_exit(GameState& game_state) { return true; }
    };
}