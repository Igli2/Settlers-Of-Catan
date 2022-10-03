#pragma once

#include <SFML/Graphics.hpp>
#include <algorithm>
#include <thread>

#include "TextureManager.h"
#include "LocalizationManager.h"
#include "Resizable.h"
#include "Clickable.h"
#include "MouseHandler.h"

#include "inventory/Inventory.h"

#include "map/HexMap.h"

#include "network/Socket.h"
#include "network/packet_types.h"

namespace client {
    class GameState {
        private:
            std::vector<Resizable*> resizables; // unregister when objects are deleted on runtime
            TextureManager texture_manager;
            LocalizationManager localization_manager;
            sf::Vector2u window_size;
            MouseHandler mouse_handler;
            Inventory inventory;
            HexMap hex_map;
            network::Socket& socket;
            std::thread receive_thread;
        public:
            sf::View inventory_view;
            sf::View map_view;
            bool is_open;

            GameState(network::Socket& socket);
            ~GameState();
            const sf::Vector2u& get_window_size();
            void set_window_size(sf::Vector2u size);
            const TextureManager& get_texture_manager();
            const LocalizationManager& get_localization_manager();
            Inventory& get_inventory();
            HexMap& get_hexmap();
            MouseHandler& get_mouse_handler();
            network::Socket& get_socket();

            void resize(); // calls resizeable handlers
            void add_resizable_object(Resizable* r);

            static void receive_packets(GameState* game_state, network::Socket* socket);
    };
}