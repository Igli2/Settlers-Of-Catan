#include "DevelopmentCard.h"
#include "GameState.h"

client::DevelopmentCard::DevelopmentCard(GameState& game_state, DevelopmentCardType type) : type{type} {
    this->setTexture(game_state.get_texture_manager().get_texture("development_card_knight"));
}