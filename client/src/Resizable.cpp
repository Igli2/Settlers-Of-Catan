#include "Resizable.h"
#include "GameState.h"

client::Resizable::Resizable(GameState& game_state) {
    game_state.add_resizable_object(this);
}