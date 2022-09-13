#pragma once

#include <SFML/Graphics.hpp>

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

    class DevelopmentCard : public sf::Sprite {
        private:
            DevelopmentCardType type;
        public:
            DevelopmentCard(GameState& game_state, DevelopmentCardType type);
    };
}