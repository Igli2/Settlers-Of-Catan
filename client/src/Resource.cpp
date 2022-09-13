#include "Resource.h"

#include "GameState.h"

client::Resource::Resource() : amount{0} {}

void client::Resource::init(GameState& game_state, client::ResourceType type) {
    this->text.setFont(game_state.get_texture_manager().get_font());
    this->text.setString(std::to_string(this->amount));
    this->text.setFillColor(sf::Color::Black);
    this->type = type;

    game_state.add_resizable_object(this);
}

void client::Resource::set_amount(unsigned int amount) {
    this->amount = amount;
}

unsigned int client::Resource::get_amount() {
    return this->amount;
}

sf::Text& client::Resource::get_render_text() {
    return this->text;
}

void client::Resource::on_resize(GameState& game_state) {
    this->text.setCharacterSize(22);
    float scale = 40.0f / this->getTexture()->getSize().y;
    this->setScale(scale, scale);
}