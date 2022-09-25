#pragma once

#include <SFML/Graphics.hpp>

#include "base/registry.h"
#include "base/Clickable.h"

namespace client {
    class GameState;

    class BuildButton : public sf::Sprite, public Clickable {
        private:
            BuildingType type;
        public:
            BuildButton();
            void init(GameState& game_state, BuildingType type);
            bool on_click(GameState& game_state, sf::Mouse::Button button) override;
            // TODO: display tooltip on hover
    };
}