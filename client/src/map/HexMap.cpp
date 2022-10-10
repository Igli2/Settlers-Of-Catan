#include "HexMap.h"

#include "base/GameState.h"
#include "base/GameWindow.h"

client::HexMap::HexMap(GameState& game_state) :
    size{5, 5},
    zoom{1.0f},
    Clickable{sf::Rect<int>{}},
    Resizable{game_state},
    currently_building{BuildingType::BUILDING_NONE},
    game_state{game_state} {
        this->hex_shape.setPointCount(6);
        this->hex_shape.setRadius(HEX_SIZE / 2.0f);
        this->marker.setPosition(sf::Vector2f{-100000.0f, -100000.0f});
        this->marker.setTexture(game_state.get_texture_manager().get_texture(building_texture_names[this->currently_building]));
}

void client::HexMap::add_tile(std::string tile_packet) {
    std::stringstream ss{tile_packet};
    int x, y;
    std::string tilename;
    ss >> x >> y >> tilename;
    if (tilename != "water") {
        this->tilemap.push_back(HexTile{x, y, tile_name_to_id(tilename)});
    }
}

client::HexTileType client::HexMap::tile_name_to_id(std::string name) {
    return tile_name_ids.at(name);
}

void client::HexMap::render(GameWindow& game_window, GameState& game_state) {
    for (const HexTile& tile : this->tilemap) {
        this->hex_shape.setTexture(&game_state.get_texture_manager().get_texture(tile_texture_names[tile.type]));
        this->hex_shape.setPosition(this->hex_to_pixel(tile) - sf::Vector2f{100.0f, 100.0f}); // offset as position is top left, not mid
        game_window.draw(this->hex_shape);
    }
    for (const BuildingData& bd : this->buildings) {
        this->building.setTexture(game_state.get_texture_manager().get_texture(building_texture_names[bd.type]));
        this->building.setPosition(this->corner_to_pixel(bd) - sf::Vector2f{25.0f, 25.0f});
        game_window.draw(this->building);
    }
    if (this->currently_building != BuildingType::BUILDING_NONE) {
        game_window.draw(this->marker);
    }
}

void client::HexMap::on_resize(GameState& game_state) {
    this->set_area(sf::Rect<int>{
        0,
        0,
        (int)game_state.get_window_size().x,
        (int)(game_state.get_window_size().y)
    });
}

bool client::HexMap::on_click(sf::Mouse::Button button) {
    if (this->currently_building == BuildingType::BUILDING_NONE) {
        return false;
    }

    sf::Vector2i pixel_pos = sf::Mouse::getPosition(*this->game_window);
    sf::Vector2f world_pos = this->game_window->mapPixelToCoords(pixel_pos, this->game_state.map_view);

    // while building, only clicks in upper part are allowed
    if (pixel_pos.y > this->game_state.get_window_size().y / 3.0 * 2.0) {
        return true;
    }
    if (button == sf::Mouse::Button::Right || button == sf::Mouse::Button::Middle) {
        this->currently_building = BuildingType::BUILDING_NONE;
        return true;
    }
    
    HexTile* tile = this->pixel_to_hex(world_pos);
    if (tile == nullptr) {
        this->marker.setPosition(sf::Vector2f{-100000.0f, -100000.0f});
        return true;
    }
    sf::Vector2i corner;
    switch (this->currently_building) {
        case BuildingType::SETTLEMENT:
            // validate
            corner = this->get_closest_corner(this->hex_to_pixel(*tile), world_pos, *tile);
            if (!this->can_place_settlement(corner)) {
                break;
            }
            // place
            this->game_state.get_socket().send(network::Packet{
                network::PacketType::PLACE_BUILDING,
                "SETTLEMENT " + std::to_string(corner.x) + " " + std::to_string(corner.y)
            });
            this->buildings.push_back(BuildingData{corner.x, corner.y, BuildingType::SETTLEMENT});
            this->currently_building = BuildingType::BUILDING_NONE;
            break;
        case BuildingType::CITY:
            // validate
            corner = this->get_closest_corner(this->hex_to_pixel(*tile), world_pos, *tile);
            if (!this->can_place_city(corner)) {
                break;
            }
            // place
            this->game_state.get_socket().send(network::Packet{
                network::PacketType::PLACE_BUILDING,
                "CITY " + std::to_string(corner.x) + " " + std::to_string(corner.y)
            });
            this->buildings.push_back(BuildingData{corner.x, corner.y, BuildingType::CITY});
            this->currently_building = BuildingType::BUILDING_NONE;
            break;
    }

    return true;
}

bool client::HexMap::on_move() {
    if (this->currently_building == BuildingType::BUILDING_NONE) {
        return false;
    }

    sf::Vector2i pixel_pos = sf::Mouse::getPosition(*this->game_window);
    sf::Vector2f world_pos = this->game_window->mapPixelToCoords(pixel_pos, this->game_state.map_view);

    HexTile* tile = this->pixel_to_hex(world_pos);
    if (tile == nullptr) {
        this->marker.setPosition(sf::Vector2f{-100000.0f, -100000.0f});
        return true;
    }
    sf::Vector2i corner;
    sf::Vector2f corner_pixel;
    switch (this->currently_building) {
        case BuildingType::SETTLEMENT:
            // validate
            corner = this->get_closest_corner(this->hex_to_pixel(*tile), world_pos, *tile);
            if (!this->can_place_settlement(corner)) {
                return true;
            }
            // set marker
            corner_pixel = corner_to_pixel(BuildingData{corner.x, corner.y, BuildingType::BUILDING_NONE});
            this->marker.setPosition(corner_pixel - sf::Vector2f{25.0f, 25.0f});
            return true;
        case BuildingType::CITY:
            // validate
            corner = this->get_closest_corner(this->hex_to_pixel(*tile), world_pos, *tile);
            if (!this->can_place_city(corner)) {
                return true;
            }
            // set marker
            corner_pixel = corner_to_pixel(BuildingData{corner.x, corner.y, BuildingType::BUILDING_NONE});
            this->marker.setPosition(corner_pixel - sf::Vector2f{25.0f, 25.0f});
            return true;
    }

    return false;
}

void client::HexMap::place(BuildingType type) {
    this->currently_building = type;
    this->marker.setTexture(this->game_state.get_texture_manager().get_texture(building_texture_names[type]));
    this->marker.setTextureRect(sf::Rect<int>{
        0,
        0,
        (int)this->marker.getTexture()->getSize().x,
        (int)this->marker.getTexture()->getSize().y
    });
}

void client::HexMap::set_game_window(GameWindow* game_window) {
    this->game_window = game_window;
}

client::HexTile* client::HexMap::pixel_to_hex(sf::Vector2f pos) {
    float xf = (std::sqrt(3.0f) / 3.0f * pos.x - 1.0f / 3.0f * pos.y) / (HEX_SIZE / 2.0f);
    float yf = (2.0f / 3.0f * pos.y) / (HEX_SIZE / 2.0f);
    // round coords
    sf::Vector2i hex_coords = this->cube_to_axial(this->cube_round(this->axial_to_cube(sf::Vector2f{xf, yf})));
    hex_coords.x = hex_coords.x + (hex_coords.y + (hex_coords.y & 1)) / 2; // convert from axial coords to offset coords

    for (HexTile& tile : this->tilemap) {
        if (tile.x == hex_coords.x && tile.y == hex_coords.y) {
            return &tile;
        }
    }
    return nullptr;
}

sf::Vector2f client::HexMap::hex_to_pixel(const HexTile& tile) {
    int tile_x = tile.x - (tile.y + (tile.y & 1)) / 2; // convert from offset coords to axial coords
    float pixel_x = (HEX_SIZE / 2.0f) * (std::sqrt(3.0f) * tile_x + std::sqrt(3.0f) / 2.0f * tile.y);
    float pixel_y = (HEX_SIZE / 2.0f) * (3.0f / 2.0f * tile.y);
    return sf::Vector2f{pixel_x, pixel_y};
}

sf::Vector2i client::HexMap::cube_to_axial(sf::Vector3i cube) {
    return sf::Vector2i{cube.x, cube.y};
}

sf::Vector3f client::HexMap::axial_to_cube(sf::Vector2f axial) {
    return sf::Vector3f{axial.x, axial.y, -axial.x - axial.y};
}

sf::Vector3i client::HexMap::cube_round(sf::Vector3f cube) {
    float x = std::round(cube.x);
    float y = std::round(cube.y);
    float z = std::round(cube.z);

    float x_diff = std::abs(x - cube.x);
    float y_diff = std::abs(y - cube.y);
    float z_diff = std::abs(z - cube.z);

    if (x_diff > y_diff && x_diff > z_diff) {
        x = -y-z;
    } else if (y_diff > z_diff) {
        y = -x-z;
    } else {
        z = -x-y;
    }

    return sf::Vector3i{(int)x, (int)y, (int)z};
}

sf::Vector2i client::HexMap::get_closest_corner(sf::Vector2f mid, sf::Vector2f pos, const HexTile& tile) {
    float min_dist = HEX_SIZE;
    sf::Vector2f result;
    int x, y; // corner coordinate system

    for (float angle = 30.0f; angle < 360.0f; angle += 60.0f) {
        float radians = angle * M_PI / 180.0f;
        sf::Vector2f off = pos - (mid + sf::Vector2f{std::cos(radians) * 100.0f, -std::sin(radians) * 100.0f});
        float dist = std::sqrt(off.x * off.x + off.y * off.y);
        if (dist < min_dist) {
            min_dist = dist;
            result = mid + sf::Vector2f{std::cos(radians) * 100.0f, -std::sin(radians) * 100.0f};
            // convert angle to corner coordinate system
            if (angle == 30.0f) {
                x = tile.x * 2 + 1;
                y = tile.y;
            } else if (angle == 90.0f) {
                x = tile.x * 2;
                y = tile.y;
            } else if (angle == 150.0f) {
                x = tile.x * 2 - 1;
                y = tile.y;
            } else if (angle == 210.0f) {
                x = tile.y & 1 ? (tile.x - 1) * 2 : tile.x * 2;
                y = tile.y + 1;
            } else if (angle == 270.0f) {
                x = tile.y & 1 ? tile.x * 2 - 1 : tile.x * 2 + 1;
                y = tile.y + 1;
            } else {
                x = tile.y & 1 ? tile.x * 2 : (tile.x + 1) * 2;
                y = tile.y + 1;
            }
        }
    }
    return sf::Vector2i{x, y};
}

sf::Vector2f client::HexMap::corner_to_pixel(BuildingData building) {
    int tile_x = building.x < 0 && building.x & 1 ? building.x / 2 - 1 : building.x / 2;
    int tile_y = building.y;
    sf::Vector2f tile_pos = this->hex_to_pixel(HexTile{tile_x, tile_y, HexTileType::DESERT});

    float angle = building.x & 1 ? 30.0f : 90.0f;
    float radians = angle * M_PI / 180.0f;
    return tile_pos + sf::Vector2f{std::cos(radians) * 100.0f, -std::sin(radians) * 100.0f};
}

bool client::HexMap::can_place_settlement(sf::Vector2i corner) {
    sf::Vector2i checks[4];
    checks[0] = sf::Vector2i{corner.x, corner.y};
    checks[1] = sf::Vector2i{corner.x + 1, corner.y};
    checks[2] = sf::Vector2i{corner.x - 1, corner.y};
    int check4x = corner.y & 1 ? corner.x - 1 : corner.x + 1;
    int check4y = corner.x & 1 ? corner.y + 1 : corner.y - 1;
    checks[3] = sf::Vector2i{check4x, check4y};

    for (const BuildingData& bd : this->buildings) {
        for (const sf::Vector2i& check : checks) {
            if (check.x == bd.x && check.y == bd.y) {
                return false;
            }
        }
    }

    return true;
}

bool client::HexMap::can_place_city(sf::Vector2i corner) {
    for (const BuildingData& bd : this->buildings) {
        if (bd.type == BuildingType::SETTLEMENT && corner.x == bd.x && corner.y == bd.y) {
            return true;
        }
    }

    return false;
}