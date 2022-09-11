#pragma once

#include <SFML/Graphics.hpp>
#include <array>

#include "Resource.h"
#include "Resizable.h"

namespace client {
    class GameState;
    class GameWindow;

    class Inventory : public Resizable {
        private:
            sf::Sprite background;
            std::array<client::Resource, client::ResourceType::MAX> resources;
        public:
            Inventory(GameState& game_state);
            void render(GameWindow& game_window, GameState& game_state);
            void on_resize(GameState& game_state) override;
    };
}