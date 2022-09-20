#include "VictoryPoint.h"

#include "base/GameState.h"

client::VictoryPoint::VictoryPoint(GameState& game_state) : max_points{12} {
    this->image.setTexture(game_state.get_texture_manager().get_texture("victory_point"));
    this->image.setPosition(sf::Vector2f{10.0f, 10.0f});
    this->image.setScale(0.75f, 0.75f);
    this->text.setString("69 / 420");
    this->text.setCharacterSize(20);
    this->text.setPosition(sf::Vector2f{20 + 40, 20});
    this->text.setFont(game_state.get_texture_manager().get_font());
    this->text.setFillColor(sf::Color::Black);
}

void client::VictoryPoint::add_points(int points) {
    this->points += points;
    this->text.setString(std::to_string(this->points) + " / " + std::to_string(this->max_points));
}

void client::VictoryPoint::set_points_required(int points) {
    this->max_points = points;
}

const sf::Sprite& client::VictoryPoint::get_sprite() {
    return this->image;
}

const sf::Text& client::VictoryPoint::get_text() {
    return this->text;
}