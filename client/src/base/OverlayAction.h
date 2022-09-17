#pragma once

#include <SFML/Graphics.hpp>

#include "Clickable.h"

namespace client {
    class Overlay;
    class GameState;

    enum OverlayActionType {
        ACCEPT,
        DECLINE
    };

    class OverlayAction : public Clickable {
        private:
            Overlay* overlay_p;
            OverlayActionType type;
            sf::Text text;
        public:
            OverlayAction(GameState& game_state, Overlay* overlay_p, OverlayActionType type);
            bool on_click(GameState& game_state, sf::Mouse::Button button) override;
            sf::Text& get_text();
            void set_action_name(std::string name);
    };
}