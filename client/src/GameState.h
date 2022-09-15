#pragma once

#include <SFML/Graphics.hpp>
#include <algorithm>

#include "TextureManager.h"
#include "LocalizationManager.h"
#include "Inventory.h"
#include "Resizable.h"
#include "Clickable.h"

namespace client {
    class GameState {
        private:
            std::vector<Resizable*> resizables; // unregister when objects are deleted on runtime
            std::vector<Clickable*> clickables; // unregister when deleted
            TextureManager texture_manager;
            LocalizationManager localization_manager;
            sf::Vector2u window_size;
            sf::Vector2i last_mouse_pressed;
            Clickable* last_hovered;
            Inventory inventory;
        public:
            GameState();
            const TextureManager& get_texture_manager();
            const sf::Vector2u& get_window_size();
            void set_window_size(sf::Vector2u size);
            Inventory& get_inventory();
            void resize(); // calls resizeable handlers
            void call_mouse_press(sf::Mouse::Button button, const sf::Vector2i& pos); // calls clickable handlers
            void call_mouse_release(sf::Mouse::Button button, const sf::Vector2i& pos); // calls clickable handlers
            void call_mouse_move(const sf::Vector2i& pos); // calls clickable handlers
            void add_resizable_object(Resizable* r);
            void add_clickable_object(Clickable* c);
            void remove_clickable_object(Clickable* c);
            const LocalizationManager& get_localization_manager();
    };
}