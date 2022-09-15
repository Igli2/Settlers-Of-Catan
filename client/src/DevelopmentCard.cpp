#include "DevelopmentCard.h"
#include "GameState.h"

const std::array<std::string, client::DevelopmentCardType::DEVELOPMENT_CARD_MAX> card_texture_names = {
    "development_card_knight",
    "development_card_monopoly",
    "development_card_road_build",
    "development_card_year_of_plenty",
    "development_card_university"
};

client::DevelopmentCard::DevelopmentCard(GameState& game_state, DevelopmentCardType type) : Clickable{game_state, sf::Rect<int>{}}, type{type} {
    this->setTexture(game_state.get_texture_manager().get_texture(card_texture_names[type]));
}

bool client::DevelopmentCard::on_click(sf::Mouse::Button button) {
    std::cout << "POGGERS CLICK" << this->type << std::endl;
    return true;
}

bool client::DevelopmentCard::on_enter() {
    std::cout << "Display text" << this->type << std::endl;
    return true;
}