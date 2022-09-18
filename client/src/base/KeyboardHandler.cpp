#include "KeyboardHandler.h"

#include "GameState.h"

client::KeyboardHandler::KeyboardHandler(GameState& game_state) : key_states{false}, game_state{game_state} {}

void client::KeyboardHandler::on_key_pressed(sf::Event::KeyEvent event_parameters) {
    if (event_parameters.code == sf::Keyboard::Key::W) {
        this->key_states[0] = true;
    }
    if (event_parameters.code == sf::Keyboard::Key::A) {
        this->key_states[1] = true;
    }
    if (event_parameters.code == sf::Keyboard::Key::S) {
        this->key_states[2] = true;
    }
    if (event_parameters.code == sf::Keyboard::Key::D) {
        this->key_states[3] = true;
    }
}

void client::KeyboardHandler::on_key_released(sf::Event::KeyEvent event_parameters) {
    if (event_parameters.code == sf::Keyboard::Key::W) {
        this->key_states[0] = false;
    }
    if (event_parameters.code == sf::Keyboard::Key::A) {
        this->key_states[1] = false;
    }
    if (event_parameters.code == sf::Keyboard::Key::S) {
        this->key_states[2] = false;
    }
    if (event_parameters.code == sf::Keyboard::Key::D) {
        this->key_states[3] = false;
    }
}

void client::KeyboardHandler::update_view(const float zoom) {
    if (this->key_states[0]) {
        this->game_state.get_hexmap().pos += sf::Vector2f{0.0f, -MOVEMENT_SPEED * zoom};
        this->game_state.map_view.move(0.0f, -MOVEMENT_SPEED * zoom);
    }
    if (this->key_states[1]) {
        game_state.get_hexmap().pos += sf::Vector2f{-MOVEMENT_SPEED * zoom, 0.0f};
        game_state.map_view.move(-MOVEMENT_SPEED * zoom, 0.0f);
    }
    if (this->key_states[2]) {
        game_state.get_hexmap().pos += sf::Vector2f{0.0f, MOVEMENT_SPEED * zoom};
        game_state.map_view.move(0.0f, MOVEMENT_SPEED * zoom);
    }
    if (this->key_states[3]) {
        game_state.get_hexmap().pos += sf::Vector2f{MOVEMENT_SPEED * zoom, 0.0f};
        game_state.map_view.move(MOVEMENT_SPEED * zoom, 0.0f);
    }
}