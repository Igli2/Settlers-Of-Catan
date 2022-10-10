#pragma once

#include <SFML/Graphics.hpp>

#include "base/Resizable.h"
#include "base/Clickable.h"

#include "TradeOfferOverlay.h"

namespace client {
    class GameState;
    class GameWindow;

    class Trade : public sf::Sprite, public Resizable, public Clickable {
        private:
            TradeOfferOverlay overlay;
        public:
            Trade(GameState& game_state);
            void on_resize(GameState& game_state) override;
            bool on_click(sf::Mouse::Button button) override;
            void render(GameWindow& game_window, GameState& game_state);
    };
}