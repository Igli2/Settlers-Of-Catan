#include "GameState.h"

client::GameState::GameState() {
    
}

const client::TextureManager& client::GameState::get_texture_manager() {
    return this->texture_manager;
}