#pragma once

#include <SFML/Graphics.hpp>
#include <string>
#include <array>

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
            OverlayAction accept;
            OverlayAction decline;
        protected:
            sf::Rect<int> dimensions;
            bool is_active;
            GameState& game_state;
        public:
            Overlay(GameState& game_state, std::string title);
            void render(GameWindow& game_window);
            void set_active(bool state);
            void set_dimensions(sf::Rect<int> dimensions); // update inner widgets
            void on_accept();
            void on_decline();
            void set_action_names(std::string accept, std::string decline);

            // block all mouse events for clickables not in overlay
            bool on_click(sf::Mouse::Button button) override { return true; }
            bool on_press(sf::Mouse::Button button) override { return true; }
            bool on_release(sf::Mouse::Button button) override { return true; }
            bool on_enter() override { return true; }
            bool on_exit() override { return true; }
    };
}