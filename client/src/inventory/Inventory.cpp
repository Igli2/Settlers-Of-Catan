#include "Inventory.h"

#include "base/GameState.h"
#include "base/GameWindow.h"

const std::array<std::string, client::ResourceType::RESOURCE_MAX> resource_texture_names = {
    "grain",
    "brick",
    "ore",
    "wool",
    "lumber"
};

client::Inventory::Inventory(GameState& game_state) : Resizable{game_state}, victory_point{game_state} {
    for (int i = 0; i < ResourceType::RESOURCE_MAX; i++) {
        this->resources[i].init(game_state, (ResourceType)i);
        this->resources[i].setTexture(game_state.get_texture_manager().get_texture(resource_texture_names[i]));
    }

    this->background[0].setFillColor(sf::Color{61, 33, 9});
    this->background[1].setFillColor(sf::Color{87, 44, 6});
    this->background[2].setFillColor(sf::Color{107, 51, 3});
    this->background[3].setFillColor(sf::Color{222, 182, 147});
    this->background[4].setFillColor(sf::Color{222, 182, 147});

    // test
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::KNIGHT});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::MONOPOLY});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::ROAD_BUILD});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::YEAR_OF_PLENTY});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::UNIVERSITY});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::MARKET});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::LIBRARY});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::GREAT_HALL});
    this->development_cards.push_back(new DevelopmentCard{game_state, DevelopmentCardType::CHAPEL});
}

client::Inventory::~Inventory() {
    for (DevelopmentCard* dc : this->development_cards) {
        delete dc;
        dc = nullptr;
    }
}

void client::Inventory::render(GameWindow& game_window, GameState& game_state) {
    for (sf::RectangleShape& rs : this->background) {
        game_window.draw(rs);
    }

    for (int x = 0; x < ResourceType::RESOURCE_MAX; x++) {
        game_window.draw(this->resources[x]);
        game_window.draw(this->resources[x].get_render_text());
    }

    for (client::DevelopmentCard* dc : this->development_cards) {
        game_window.draw(*dc);
    }

    game_window.draw(this->victory_point.get_sprite());
    game_window.draw(this->victory_point.get_text());
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

    int i = 0;
    float x, y;
    sf::Vector2u tex_size;
    for (DevelopmentCard* dc : this->development_cards) {
        tex_size = dc->getTexture()->getSize();
        float texture_scale = (inv_height - INV_TOP_HEIGHT - INV_BORDER_SIZE * 9.0f) / tex_size.y;
        x = INV_BORDER_SIZE * 4.0f + i * (tex_size.x * texture_scale + INV_BORDER_SIZE);
        y = (float)game_state.get_window_size().y - tex_size.y * texture_scale - INV_BORDER_SIZE * 4.0f;
        dc->setScale(texture_scale, texture_scale);
        dc->setPosition(x, y);
        dc->set_area(sf::IntRect{(int)x, (int)y, (int)(tex_size.x * texture_scale), (int)(tex_size.y * texture_scale)});
        i++;
    }
}

void client::Inventory::remove_development_card(GameState& game_state, client::DevelopmentCard* dc) {
    for (std::vector<client::DevelopmentCard*>::iterator dc_iter = this->development_cards.begin(); dc_iter != this->development_cards.end(); dc_iter++) {
        if (dc == *dc_iter) {
            this->development_cards.erase(dc_iter);
            this->on_resize(game_state);
            return;
        }
    }
}