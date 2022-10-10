#pragma once

#include <SFML/Graphics.hpp>

#include "ClickableText.h"

namespace client {
    class Overlay;
    class GameState;

    enum OverlayActionType {
        ACCEPT,
        DECLINE
    };

    class OverlayAction : public ClickableText {
        private:
            Overlay* overlay_p;
            OverlayActionType type;
        public:
            OverlayAction(GameState& game_state, Overlay* overlay_p, OverlayActionType type);
            bool on_click(sf::Mouse::Button button) override;
            void set_action_name(std::string name);
    };
}