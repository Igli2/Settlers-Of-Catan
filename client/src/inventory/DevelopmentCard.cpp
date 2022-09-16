#include "DevelopmentCard.h"

#include "base/GameState.h"

const std::array<std::string, client::DevelopmentCardType::DEVELOPMENT_CARD_MAX> card_texture_names = {
    "development_card_knight",
    "development_card_monopoly",
    "development_card_road_build",
    "development_card_year_of_plenty",
    "development_card_university",
    "development_card_market",
    "development_card_chapel",
    "development_card_library",
    "development_card_great_hall"
};

const std::array<std::string, client::DevelopmentCardType::DEVELOPMENT_CARD_MAX> card_descriptions = {
    "knight_description",
    "monopoly_description",
    "road_build_description",
    "year_of_plenty_description",
    "university_description",
    "market_description",
    "chapel_description",
    "library_description",
    "great_hall_description"
};

client::DevelopmentCard::DevelopmentCard(GameState& game_state, DevelopmentCardType type) : Clickable{game_state, sf::Rect<int>{}}, type{type} {
    this->setTexture(game_state.get_texture_manager().get_texture(card_texture_names[type]));
}

bool client::DevelopmentCard::on_click(GameState& game_state, sf::Mouse::Button button) {
    // delete all pointers to this and commit suicide
    game_state.remove_clickable_object(this);
    game_state.get_inventory().remove_development_card(game_state, this);
    delete this;
    return true;
}

bool client::DevelopmentCard::on_enter(GameState& game_state) {
    std::cout << game_state.get_localization_manager().get_translation(card_descriptions[this->type]) << std::endl;
    return true;
}