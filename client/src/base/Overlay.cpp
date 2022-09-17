#include "Overlay.h"
#include "GameState.h"
#include "GameWindow.h"

client::Overlay::Overlay(GameState& game_state, std::string title) :
    Clickable{sf::Rect<int>{}},
    accept{game_state, this, OverlayActionType::ACCEPT},
    decline{game_state, this, OverlayActionType::DECLINE},
    is_active{false} {
        this->blocker.setFillColor(sf::Color{0, 0, 0, 128});

        this->title.setFillColor(sf::Color::Black);
        this->title.setFont(game_state.get_texture_manager().get_font());
        this->title.setString(title);
        this->title.setCharacterSize(28);

        // move to TradeOfferOverlay
        this->accept.set_action_name("Request");
        this->decline.set_action_name("Cancel");

        this->background[0].setFillColor(sf::Color{61, 33, 9});
        this->background[1].setFillColor(sf::Color{87, 44, 6});
        this->background[2].setFillColor(sf::Color{107, 51, 3});
        this->background[3].setFillColor(sf::Color{222, 182, 147});
}

void client::Overlay::render(GameWindow& game_window, GameState& game_state) {
    if (this->is_active) {
        game_window.draw(this->blocker);
        for (sf::RectangleShape& rs : this->background) {
            game_window.draw(rs);
        }
        game_window.draw(this->title);
        game_window.draw(this->accept.get_text());
        game_window.draw(this->decline.get_text());
    }
}

void client::Overlay::set_active(GameState& game_state, bool state) {
    if (state) {
        game_state.add_clickable_object(this, 0);
        game_state.add_clickable_object(&this->accept, 0);
        game_state.add_clickable_object(&this->decline, 0);
    } else {
        game_state.remove_clickable_object(this);
        game_state.remove_clickable_object(&this->accept);
        game_state.remove_clickable_object(&this->decline);
    }
    this->is_active = state;
}

void client::Overlay::on_accept(GameState& game_state) {}

void client::Overlay::on_decline(GameState& game_state) {
    this->set_active(game_state, false);
}

void client::Overlay::set_dimensions(GameState& game_state, sf::Rect<int> dimensions) {
    this->dimensions = dimensions;

    // mouse event blocker
    this->set_area(sf::Rect<int>{0, 0, (int)game_state.get_window_size().x, (int)game_state.get_window_size().y});
    this->blocker.setSize(sf::Vector2f{(float)game_state.get_window_size().x, (float)game_state.get_window_size().y});

    float title_x = dimensions.left + dimensions.width / 2.0f - this->title.getLocalBounds().width / 2.0f;
    this->title.setPosition(title_x, dimensions.top + 20.0f);

    float button_y = dimensions.top + dimensions.height - 55.0f;
    float decline_width = this->decline.get_text().getLocalBounds().width;
    float accept_width = this->accept.get_text().getLocalBounds().width;
    float decline_x = dimensions.left + dimensions.width - decline_width - 30.0f;
    float accept_x = decline_x - accept_width - 30.0f;

    this->accept.get_text().setPosition(accept_x, button_y);
    this->decline.get_text().setPosition(decline_x, button_y);
    this->accept.set_area(sf::Rect<int>{this->accept.get_text().getGlobalBounds()});
    this->decline.set_area(sf::Rect<int>{this->decline.get_text().getGlobalBounds()});

    for (int x = 0; x < 4; x++) {
        this->background[x].setSize(sf::Vector2f{dimensions.width - x * INV_BORDER_SIZE * 2.0f, dimensions.height - x * INV_BORDER_SIZE * 2.0f});
        this->background[x].setPosition(dimensions.left + x * INV_BORDER_SIZE, dimensions.top + x * INV_BORDER_SIZE);
    }
}