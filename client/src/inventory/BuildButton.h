#pragma once

#include <SFML/Graphics.hpp>

#include "base/registry.h"
#include "base/Clickable.h"

namespace client {
    class GameState;

    class BuildButton : public sf::Sprite, public Clickable {
        private:
            BuildingType type;
            GameState& game_state;
        public:
            BuildButton(GameState& game_state);
            void init(BuildingType type);
            bool on_click(sf::Mouse::Button button) override;
            // TODO: display tooltip on hover
    };
}