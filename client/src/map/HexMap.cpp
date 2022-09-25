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
        for (int i = 0; i < this->size.x; i++) {
            for (int j = 0; j < this->size.y - std::abs(i - this->size.y / 2); j++) {
                this->tilemap.push_back(HexTile{j, i, (HexTileType)(i + 1)});
            }
        }
        this->hex_shape.setPointCount(6);
        this->hex_shape.setRadius(HEX_SIZE / 2.0f);
        this->marker.setPosition(sf::Vector2f{-100000.0f, -100000.0f});
        this->marker.setTexture(game_state.get_texture_manager().get_texture(building_texture_names[this->currently_building]));
}

void client::HexMap::render(GameWindow& game_window, GameState& game_state) {
    for (const HexTile& tile : this->tilemap) {
        this->hex_shape.setTexture(&game_state.get_texture_manager().get_texture(tile_texture_names[tile.type]));
        this->hex_shape.setPosition(this->hex_to_pixel(tile) - sf::Vector2f{100.0f, 100.0f}); // offset as position is top left, not mid
        game_window.draw(this->hex_shape);
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

bool client::HexMap::on_click(GameState& game_state, sf::Mouse::Button button) {
    if (this->currently_building != BuildingType::BUILDING_NONE) {
        sf::Vector2i pixel_pos = sf::Mouse::getPosition(*this->game_window);
        // while building, only clicks in upper part are allowed
        if (pixel_pos.y > game_state.get_window_size().y / 3.0 * 2.0) {
            return true;
        }
        if (button == sf::Mouse::Button::Right || button == sf::Mouse::Button::Middle) {
            this->currently_building = BuildingType::BUILDING_NONE;
            return true;
        }
        // TODO
        // get closest corner for settlement
        // send place packet
        return true;
    }
    return false;
}

bool client::HexMap::on_move(GameState& game_state) {
    if (this->currently_building == BuildingType::BUILDING_NONE) {
        return false;
    }

    sf::Vector2i pixel_pos = sf::Mouse::getPosition(*this->game_window);
    sf::Vector2f world_pos = this->game_window->mapPixelToCoords(pixel_pos, game_state.map_view);

    HexTile* tile;
    sf::Vector2f corner;
    switch (this->currently_building) {
        case BuildingType::SETTLEMENT:
            // get clicked tile
            tile = this->pixel_to_hex(world_pos);
            if (tile == nullptr) {
                this->marker.setPosition(sf::Vector2f{-100000.0f, -100000.0f});
                return true;
            }
            // mark closest corner
            corner = this->get_closest_corner(this->hex_to_pixel(*tile), world_pos);
            this->marker.setPosition(corner - sf::Vector2f{25.0f, 25.0f});
            return true;
        default:
            return false;
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
    // offset for settlers coord system
    hex_coords.x = hex_coords.y > this->size.y / 2 ? hex_coords.x + this->size.y / 2 : hex_coords.x + hex_coords.y;

    for (HexTile& tile : this->tilemap) {
        if (tile.x == hex_coords.x && tile.y == hex_coords.y) {
            return &tile;
        }
    }
    return nullptr;
}

sf::Vector2f client::HexMap::hex_to_pixel(const HexTile& tile) {
    int tile_x = tile.y > this->size.y / 2 ? tile.x - this->size.y / 2 : tile.x - tile.y;
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

sf::Vector2f client::HexMap::get_closest_corner(sf::Vector2f mid, sf::Vector2f pos) {
    float min_dist = HEX_SIZE;
    sf::Vector2f result;

    for (float angle = 30.0f; angle < 360.0f; angle += 60.0f) {
        float radians = angle * M_PI / 180.0f;
        sf::Vector2f off = pos - (mid + sf::Vector2f{std::cos(radians) * 100.0f, -std::sin(radians) * 100.0f});
        float dist = std::sqrt(off.x * off.x + off.y * off.y);
        if (dist < min_dist) {
            min_dist = dist;
            result = mid + sf::Vector2f{std::cos(radians) * 100.0f, -std::sin(radians) * 100.0f};
        }
    }

    return result;
}