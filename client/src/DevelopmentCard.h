#pragma once

#include <SFML/Graphics.hpp>
#include <array>

#include "Clickable.h"

namespace client {
    class GameState;

    enum DevelopmentCardType {
        KNIGHT,
        MONOPOLY,
        ROAD_BUILD,
        YEAR_OF_PLENTY,
        UNIVERSITY,
        MARKET,
        CHAPEL,
        LIBRARY,
        GREAT_HALL,
        DEVELOPMENT_CARD_MAX
    };

    class DevelopmentCard : public sf::Sprite, public Clickable {
        private:
            DevelopmentCardType type;
        public:
            DevelopmentCard(GameState& game_state, DevelopmentCardType type);
            bool on_click(sf::Mouse::Button button) override;
            bool on_enter() override;
    };
}