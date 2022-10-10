#include "MouseHandler.h"
#include "GameState.h"

client::MouseHandler::MouseHandler(GameState& game_state) :
    game_state{game_state},
    last_mouse_pressed{-1, -1},
    last_hovered{nullptr} {

}

void client::MouseHandler::call_mouse_press(sf::Mouse::Button button, const sf::Vector2i& pos) {
    this->last_mouse_pressed = pos;
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && cp->on_press(button)) {
            break;
        }
    }
}

void client::MouseHandler::call_mouse_release(sf::Mouse::Button button, const sf::Vector2i& pos) {
    for (Clickable* cp : this->clickables) {
        if (cp->on_release(button)) {
            break;
        }
    }
    if (this->game_state.get_hexmap().contains(pos) && 
        this->game_state.get_hexmap().contains(this->last_mouse_pressed) &&
        this->game_state.get_hexmap().on_click(button)) {
            this->last_mouse_pressed = sf::Vector2i{-1, -1};
            return;
    }
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && 
            cp->contains(this->last_mouse_pressed) &&
            cp->on_click(button)) {
                break;
        }
    }
    this->last_mouse_pressed = sf::Vector2i{-1, -1};
}

void client::MouseHandler::call_mouse_move(const sf::Vector2i& pos) {
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
    this->call_on_move(pos);
}

void client::MouseHandler::call_on_move(const sf::Vector2i& pos) {
    if (game_state.get_hexmap().contains(pos) && game_state.get_hexmap().on_move()) {
        return;
    }
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos)) {
            cp->on_move();
        }
    }
}

void client::MouseHandler::add_clickable_object(Clickable* c) {
    this->clickables.push_back(c);
}

void client::MouseHandler::add_clickable_object(Clickable* c, unsigned int priority) {
    if (this->clickables.size() > priority) {
        this->clickables.insert(this->clickables.begin() + priority, c);
    } else {
        this->add_clickable_object(c);
    }
}

void client::MouseHandler::remove_clickable_object(Clickable* c) {
    if (this->last_hovered == c) {
        this->last_hovered = nullptr;
    }
    for (std::vector<client::Clickable*>::iterator c_iter = this->clickables.begin(); c_iter != this->clickables.end(); c_iter++) {
        if (c == *c_iter) {
            this->clickables.erase(c_iter);
            return;
        }
    }
}