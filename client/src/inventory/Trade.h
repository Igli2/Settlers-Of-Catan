#pragma once

#include <SFML/Graphics.hpp>

#include "base/Resizable.h"
#include "base/Clickable.h"
#include "base/Overlay.h"

namespace client {
    class GameState;
    class GameWindow;

    class Trade : public sf::Sprite, public Resizable, public Clickable {
        private:
            Overlay overlay;
        public:
            Trade(GameState& game_state);
            void on_resize(GameState& game_state) override;
            bool on_click(GameState& game_state, sf::Mouse::Button button) override;
            void render(GameWindow& game_window, GameState& game_state);
    };
}