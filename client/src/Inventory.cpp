#include "Inventory.h"
#include "GameState.h"
#include "GameWindow.h"

client::Inventory::Inventory(GameState& game_state) : Resizable{game_state} {
    this->background.setTexture(game_state.get_texture_manager().get_texture("inventory_background"));

    for (int i = 0; i < ResourceType::MAX; i++) {
        this->resources[i].init(game_state, (ResourceType)i);
    }
    this->resources[0].setTexture(game_state.get_texture_manager().get_texture("grain"));
    this->resources[1].setTexture(game_state.get_texture_manager().get_texture("brick"));
}

void client::Inventory::render(GameWindow& game_window, GameState& game_state) {
    game_window.draw(this->background);

    for (int x = 0; x < ResourceType::MAX; x++) {
        game_window.draw(this->resources[x]);
        game_window.draw(this->resources[x].get_render_text());
    }
}

void client::Inventory::on_resize(GameState& game_state) {
    float x_scale = (float)game_state.get_window_size().x / this->background.getTexture()->getSize().x;
    float y_scale = (float)game_state.get_window_size().y / this->background.getTexture()->getSize().y;
    this->background.setScale(x_scale, y_scale / 3);
    this->background.setPosition(0, game_state.get_window_size().y / 3 * 2 + 1);

    int resource_pos_x = 10;
    int resource_pos_y = game_state.get_window_size().y / 3 * 2 + 11;
    for (int x = 0; x < ResourceType::MAX; x++) {
        this->resources[x].setPosition(resource_pos_x + x * 50, resource_pos_y);
        this->resources[x].get_render_text().setPosition(resource_pos_x + x * 50 + 30, resource_pos_y);
    }
}