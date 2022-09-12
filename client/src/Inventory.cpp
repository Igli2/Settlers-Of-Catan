#include "Inventory.h"
#include "GameState.h"
#include "GameWindow.h"

client::Inventory::Inventory(GameState& game_state) : Resizable{game_state} {
    for (int i = 0; i < ResourceType::MAX; i++) {
        this->resources[i].init(game_state, (ResourceType)i);
    }
    this->resources[0].setTexture(game_state.get_texture_manager().get_texture("grain"));
    this->resources[1].setTexture(game_state.get_texture_manager().get_texture("brick"));
    this->resources[2].setTexture(game_state.get_texture_manager().get_texture("ore"));
    this->resources[3].setTexture(game_state.get_texture_manager().get_texture("wool"));
    this->resources[4].setTexture(game_state.get_texture_manager().get_texture("lumber"));

    this->background[0].setFillColor(sf::Color{61, 33, 9});
    this->background[1].setFillColor(sf::Color{87, 44, 6});
    this->background[2].setFillColor(sf::Color{107, 51, 3});
    this->background[3].setFillColor(sf::Color{222, 182, 147});
    this->background[4].setFillColor(sf::Color{222, 182, 147});
}

void client::Inventory::render(GameWindow& game_window, GameState& game_state) {
    for (sf::RectangleShape& rs : this->background) {
        game_window.draw(rs);
    }

    for (int x = 0; x < ResourceType::MAX; x++) {
        game_window.draw(this->resources[x]);
        game_window.draw(this->resources[x].get_render_text());
    }
}

void client::Inventory::on_resize(GameState& game_state) {
    float inv_height = (float)game_state.get_window_size().y / 3.0;
    for (int x = 0; x < 3; x++) {
        this->background[x].setSize(sf::Vector2f{game_state.get_window_size().x - x * 10.0f, inv_height - x * 10.0f});
        this->background[x].setPosition(x * 5.0f, inv_height * 2.0f + x * 5.0f);
    }
    this->background[3].setSize(sf::Vector2f{game_state.get_window_size().x - 30.0f, 50});
    this->background[3].setPosition(15.0f, inv_height * 2.0f + 15.0f);
    this->background[4].setSize(sf::Vector2f{game_state.get_window_size().x - 30.0f, inv_height - 85.0f});
    this->background[4].setPosition(15.0f, inv_height * 2.0f + 70.0f);

    float resource_pos_x = 20.0f;
    float resource_pos_y = game_state.get_window_size().y / 3.0f * 2.0f + 24.0f;
    for (int x = 0; x < ResourceType::MAX; x++) {
        this->resources[x].setPosition(resource_pos_x + x * 60.0f, resource_pos_y);
        this->resources[x].get_render_text().setPosition(resource_pos_x + x * 60.0f + 30.0f, resource_pos_y);
    }
}