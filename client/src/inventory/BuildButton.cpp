#include "BuildButton.h"

#include "base/GameState.h"

client::BuildButton::BuildButton(GameState& game_state) : Clickable{sf::Rect<int>{}}, game_state{game_state} {}

void client::BuildButton::init(BuildingType type) {
    this->type = type;
    this->game_state.get_mouse_handler().add_clickable_object(this);

    this->setTexture(this->game_state.get_texture_manager().get_texture(building_texture_names[this->type]));
    float scale = 40.0f / this->getTexture()->getSize().x;
    this->setScale(scale, scale);

    // TODO: tooltip
}

bool client::BuildButton::on_click(sf::Mouse::Button button) {
    if (button == sf::Mouse::Button::Left) {
        this->game_state.get_hexmap().place(this->type);
        return true;
    }
    return false;
}