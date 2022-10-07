#include "MouseHandler.h"
#include "GameState.h"

client::MouseHandler::MouseHandler(GameState& game_state) :
    game_state{game_state},
    last_mouse_pressed{-1, -1},
    last_hovered{nullptr} {

}

void client::MouseHandler::call_mouse_press(sf::Mouse::Button button, const sf::Vector2i& pos) {
    this->last_mouse_pressed = pos;
    for (Clickable* to_check : this->clickables) {
        if (to_check->contains(pos) && to_check->on_press(this->game_state, button)) {
            break;
        }
    }
}

//TODO: intended behaviour to allow click on multiple objects?
void client::MouseHandler::call_mouse_release(sf::Mouse::Button button, const sf::Vector2i& pos) {
    for (Clickable* to_check : this->clickables) {
        if (to_check->contains(pos) && to_check->on_release(this->game_state, button)) {
            break;
        }
    }
    if (this->game_state.get_hexmap().contains(pos) && 
        this->game_state.get_hexmap().contains(this->last_mouse_pressed) &&
        this->game_state.get_hexmap().on_click(this->game_state, button)) {
            this->last_mouse_pressed = sf::Vector2i{-1, -1};
            return;
    }
    for (Clickable* to_check : this->clickables) { // TODO: what exactly does this? merge with for loop ^?
        if (to_check->contains(pos) && 
            to_check->contains(this->last_mouse_pressed) &&
            to_check->on_click(this->game_state, button)) {
                break;
        }
    }
    this->last_mouse_pressed = sf::Vector2i{-1, -1};
}

void client::MouseHandler::call_mouse_move(const sf::Vector2i& pos) {
    for (Clickable* to_check : this->clickables) {
        if (!to_check->contains(pos) && this->last_hovered == to_check) {
            last_hovered = nullptr;
            to_check->on_exit(this->game_state);
        }
    }
    for (Clickable* to_check : this->clickables) {
        if (to_check->contains(pos) && this->last_hovered == nullptr || this->last_hovered != nullptr && !this->last_hovered->contains(pos) && to_check->contains(pos)) {
            last_hovered = to_check;
            to_check->on_enter(this->game_state);
        }
    }
    this->call_on_move(pos);
}

void client::MouseHandler::call_on_move(const sf::Vector2i& pos) {
    if (game_state.get_hexmap().contains(pos) && game_state.get_hexmap().on_move(this->game_state)) {
        return;
    }
    for (Clickable* to_check : this->clickables) {
        if (to_check->contains(pos)) {
            to_check->on_move(this->game_state);
        }
    }
}

void client::MouseHandler::add_clickable_object(Clickable* to_add) {
    this->clickables.push_back(to_add);
}

void client::MouseHandler::add_clickable_object(Clickable* to_add, unsigned int priority) {
    if (this->clickables.size() > priority) {
        this->clickables.insert(this->clickables.begin() + priority, to_add);
    } else {
        this->add_clickable_object(to_add);
    }
}

void client::MouseHandler::remove_clickable_object(Clickable* to_add) {
    if (this->last_hovered == to_add) {
        this->last_hovered = nullptr;
    }
    for (std::vector<client::Clickable*>::iterator c_iter = this->clickables.begin(); c_iter != this->clickables.end(); c_iter++) {
        if (to_add == *c_iter) {
            this->clickables.erase(c_iter);
            return;
        }
    }
}