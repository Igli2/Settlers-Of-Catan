#include "Inventory.h"
#include "GameState.h"
#include "GameWindow.h"

client::Inventory::Inventory(GameState& game_state) : Resizable{game_state} {
    for (int i = 0; i < ResourceType::RESOURCE_MAX; i++) {
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

    // test
    this->development_cards.push_back(DevelopmentCard{game_state, DevelopmentCardType::KNIGHT});
    this->development_cards.push_back(DevelopmentCard{game_state, DevelopmentCardType::KNIGHT});
    this->development_cards.push_back(DevelopmentCard{game_state, DevelopmentCardType::KNIGHT});
}

void client::Inventory::render(GameWindow& game_window, GameState& game_state) {
    for (sf::RectangleShape& rs : this->background) {
        game_window.draw(rs);
    }

    for (int x = 0; x < ResourceType::RESOURCE_MAX; x++) {
        game_window.draw(this->resources[x]);
        game_window.draw(this->resources[x].get_render_text());
    }

    for (client::DevelopmentCard& dc : this->development_cards) {
        game_window.draw(dc);
    }
}

void client::Inventory::on_resize(GameState& game_state) {
    float inv_height = (float)game_state.get_window_size().y / 3.0f;
    float inv_pos = inv_height * 2.0f;
    float window_width = (float)game_state.get_window_size().x;
    for (int x = 0; x < 3; x++) {
        this->background[x].setSize(sf::Vector2f{window_width - x * INV_BORDER_SIZE * 2.0f, inv_height - x * INV_BORDER_SIZE * 2.0f});
        this->background[x].setPosition(x * INV_BORDER_SIZE, inv_pos + x * INV_BORDER_SIZE);
    }
    float inner_rect_width = window_width - INV_BORDER_SIZE * 6.0f;
    this->background[3].setSize(sf::Vector2f{inner_rect_width, INV_TOP_HEIGHT});
    this->background[3].setPosition(INV_BORDER_SIZE * 3.0f, inv_pos + INV_BORDER_SIZE * 3.0f);
    this->background[4].setSize(sf::Vector2f{inner_rect_width, inv_height - INV_TOP_HEIGHT - INV_BORDER_SIZE * 7.0f});
    this->background[4].setPosition(INV_BORDER_SIZE * 3.0f, inv_pos + INV_TOP_HEIGHT + INV_BORDER_SIZE * 4.0f);

    float resource_pos_x = INV_BORDER_SIZE * 4.0f;
    float resource_pos_y = inv_pos + INV_BORDER_SIZE * 4.0f;
    for (int x = 0; x < ResourceType::RESOURCE_MAX; x++) {
        this->resources[x].setPosition(resource_pos_x + x * 100.0f, resource_pos_y);
        this->resources[x].get_render_text().setPosition(resource_pos_x + x * 100.0f + 45.0f, resource_pos_y + 6.0f);
    }

    int c = 0;
    for (DevelopmentCard& dc : this->development_cards) {
        float texture_scale = (inv_height - INV_TOP_HEIGHT - INV_BORDER_SIZE * 9.0f) / dc.getTexture()->getSize().y;
        dc.setScale(texture_scale, texture_scale);
        dc.setPosition(INV_BORDER_SIZE * 4.0f + c * (dc.getTexture()->getSize().x * texture_scale + INV_BORDER_SIZE), (float)game_state.get_window_size().y - dc.getTexture()->getSize().y * texture_scale - INV_BORDER_SIZE * 4.0f);
        c++;
    }
}