#include "GameState.h"

client::GameState::GameState() : window_size{500, 500}, inventory{*this} {}

const client::TextureManager& client::GameState::get_texture_manager() {
    return this->texture_manager;
}

const sf::Vector2u& client::GameState::get_window_size() {
    return this->window_size;
}

void client::GameState::set_window_size(sf::Vector2u size) {
    this->window_size = size;
}

client::Inventory& client::GameState::get_inventory() {
    return this->inventory;
}

// call the resize method for all resizable objects to resize UI elements
void client::GameState::resize() {
    for (Resizable* r : this->resizables) {
        r->on_resize(*this);
    }
}

void client::GameState::add_resizable_object(Resizable* r) {
    this->resizables.push_back(r);
}