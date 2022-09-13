#pragma once

#include <SFML/Graphics.hpp>
#include <array>
#include <vector>

#include "Resource.h"
#include "Resizable.h"
#include "DevelopmentCard.h"

#define INV_BORDER_SIZE 5.0f
#define INV_TOP_HEIGHT 50.0f

namespace client {
    class GameState;
    class GameWindow;

    class Inventory : public Resizable {
        private:
            std::array<sf::RectangleShape, 5> background;
            std::array<client::Resource, client::ResourceType::RESOURCE_MAX> resources;
            std::vector<client::DevelopmentCard> development_cards;
        public:
            Inventory(GameState& game_state);
            void render(GameWindow& game_window, GameState& game_state);
            void on_resize(GameState& game_state) override;
    };
}