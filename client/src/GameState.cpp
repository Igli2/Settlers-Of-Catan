#include "GameState.h"

client::GameState::GameState() : window_size{500, 500}, inventory{*this}, last_mouse_pressed{-1, -1}, last_hovered{nullptr} {}

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

void client::GameState::add_clickable_object(Clickable* c) {
    this->clickables.push_back(c);
}

void client::GameState::call_mouse_press(sf::Mouse::Button button, const sf::Vector2i& pos) {
    this->last_mouse_pressed = pos;
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && cp->on_press(button)) {
            break;
        }
    }
}

void client::GameState::call_mouse_release(sf::Mouse::Button button, const sf::Vector2i& pos) {
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && cp->on_release(button)) {
            break;
        }
    }
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && cp->contains(this->last_mouse_pressed) && cp->on_click(button)) {
            break;
        }
    }
    this->last_mouse_pressed = sf::Vector2i{-1, -1};
}

void client::GameState::call_mouse_move(const sf::Vector2i& pos) {
    for (Clickable* cp : this->clickables) {
        if (!cp->contains(pos) && this->last_hovered == cp) {
            last_hovered = nullptr;
            cp->on_exit();
        }
    }
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && this->last_hovered == nullptr || this->last_hovered != nullptr && !this->last_hovered->contains(pos) && cp->contains(pos)) {
            last_hovered = cp;
            cp->on_enter();
        }
    }
}