#include "Clickable.h"

#include "GameState.h"

client::Clickable::Clickable(sf::Rect<int> area) : click_area{area} {}

client::Clickable::Clickable(GameState& game_state, sf::Rect<int> area) : click_area{area} {
    game_state.add_clickable_object(this);
}

bool client::Clickable::contains(const sf::Vector2i& point) {
    return this->click_area.contains(point);
}

void client::Clickable::set_area(sf::Rect<int> rect) {
    this->click_area = rect;
}