#pragma once

#include <SFML/Graphics.hpp>
#include <vector>
#include <array>
#include <cmath>
#include <string>

#include "base/registry.h"
#include "base/Clickable.h"
#include "base/Resizable.h"

namespace client {
    class GameState;
    class GameWindow;

    struct HexTile {
        int x;
        int y;
        HexTileType type;
    };

    struct BuildingData {
        int x;
        int y;
        BuildingType type;
    };

    class HexMap : public Clickable, public Resizable {
        const float HEX_SIZE = 200.0f;
        private:
            std::vector<HexTile> tilemap;
            std::vector<BuildingData> buildings;
            sf::CircleShape hex_shape;
            sf::Vector2i size;
            BuildingType currently_building;
            GameWindow* game_window;
            GameState& game_state;
            sf::Sprite marker;
            sf::Sprite building;

            HexTile* pixel_to_hex(sf::Vector2f pos);
            sf::Vector2f hex_to_pixel(const HexTile& tile);
            sf::Vector2i cube_to_axial(sf::Vector3i cube);
            sf::Vector3f axial_to_cube(sf::Vector2f axial);
            sf::Vector3i cube_round(sf::Vector3f cube);
            sf::Vector2i get_closest_corner(sf::Vector2f mid, sf::Vector2f pos, const HexTile& tile);
            client::HexTileType tile_name_to_id(std::string name);
            sf::Vector2f corner_to_pixel(BuildingData building);
            bool can_place_settlement(sf::Vector2i corner);
            bool can_place_city(sf::Vector2i corner);
        public:
            sf::Vector2f pos;
            float zoom;

            HexMap(GameState& game_state);
            void render(GameWindow& game_window, GameState& game_state);
            bool on_click(GameState& game_state, sf::Mouse::Button button) override;
            void on_resize(GameState& game_state) override;
            bool on_move(GameState& game_state) override;
            void place(BuildingType type);
            void set_game_window(GameWindow* game_window);
            void add_tile(std::string tile_packet);
    };
}