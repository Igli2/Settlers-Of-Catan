#pragma once

#include <SFML/Graphics.hpp>

#include "TextureManager.h"
#include "Inventory.h"
#include "Resizable.h"

namespace client {
    class GameState {
        private:
            std::vector<Resizable*> resizables; // unregister when objects are deleted on runtime
            TextureManager texture_manager;
            sf::Vector2u window_size;
            Inventory inventory;
        public:
            GameState();
            const TextureManager& get_texture_manager();
            const sf::Vector2u& get_window_size();
            void set_window_size(sf::Vector2u size);
            Inventory& get_inventory();
            void resize();
            void add_resizable_object(Resizable* r);
    };
}