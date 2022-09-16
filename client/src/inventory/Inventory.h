#pragma once

#include <SFML/Graphics.hpp>
#include <array>
#include <vector>
#include <algorithm>

#include "Resource.h"
#include "DevelopmentCard.h"
#include "VictoryPoint.h"

#include "base/Resizable.h"

#define INV_BORDER_SIZE 5.0f
#define INV_TOP_HEIGHT 50.0f

namespace client {
    class GameState;
    class GameWindow;

    class Inventory : public Resizable {
        private:
            std::array<sf::RectangleShape, 5> background;
            std::array<Resource, ResourceType::RESOURCE_MAX> resources;
            std::vector<DevelopmentCard*> development_cards; // delete manually when removed
            VictoryPoint victory_point;
        public:
            Inventory(GameState& game_state);
            ~Inventory();
            void render(GameWindow& game_window, GameState& game_state);
            void on_resize(GameState& game_state) override;
            void remove_development_card(GameState& game_state, DevelopmentCard* dc);
    };
}