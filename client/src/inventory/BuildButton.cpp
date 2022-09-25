#include "BuildButton.h"

#include "base/GameState.h"

client::BuildButton::BuildButton() : Clickable{sf::Rect<int>{}} {}

void client::BuildButton::init(GameState& game_state, BuildingType type) {
    this->type = type;
    game_state.get_mouse_handler().add_clickable_object(this);

    this->setTexture(game_state.get_texture_manager().get_texture(building_texture_names[this->type]));
    float scale = 40.0f / this->getTexture()->getSize().x;
    this->setScale(scale, scale);

    // TODO: tooltip
}

bool client::BuildButton::on_click(GameState& game_state, sf::Mouse::Button button) {
    if (button == sf::Mouse::Button::Left) {
        game_state.get_hexmap().place(this->type);
        return true;
    }
    return false;
}