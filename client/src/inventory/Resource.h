#pragma once

#include <SFML/Graphics.hpp>
#include <string>

#include "base/Resizable.h"
#include "base/registry.h"

namespace client {
    class GameState;

    class Resource : public sf::Sprite, public Resizable {
        private:
            unsigned int amount;
            sf::Text text;
            ResourceType type;
        public:
            Resource();
            void init(GameState& game_state, ResourceType type);
            void set_amount(unsigned int amount);
            unsigned int get_amount();
            sf::Text& get_render_text();
            void on_resize(GameState& game_state) override;
    };
}