#include "Overlay.h"

#include <cassert>

#include "GameState.h"
#include "GameWindow.h"

const sf::Color BLOCKER_COLOR = sf::Color{0, 0, 0, 128};
constexpr size_t TITLE_FONT_SIZE = 28; //TODO: merge with other in config header
const std::array<sf::Color, 4> BACKGROUND_COLOR_GRADIENT = {
    sf::Color{61, 33, 9},
    sf::Color{87, 44, 6},
    sf::Color{107, 51, 3},
    sf::Color{222, 182, 147}
};

client::Overlay::Overlay(GameState& game_state, const std::string& title) :
    Clickable{sf::Rect<int>{}},
    accept{game_state, this, OverlayActionType::ACCEPT},
    decline{game_state, this, OverlayActionType::DECLINE},
    is_active{false},
    game_state{game_state} {
        this->blocker.setFillColor(BLOCKER_COLOR);

        this->title.setFillColor(sf::Color::Black);
        this->title.setFont(game_state.get_texture_manager().get_font());
        this->title.setString(title);
        this->title.setCharacterSize(TITLE_FONT_SIZE);

        assert(this->background.size() == BACKGROUND_COLOR_GRADIENT.size() && "Background color gradient length doesn't match background segment count!");

        for(size_t i = 0; i < this->background.size(); i++) {
            this->background.at(i).setFillColor(BACKGROUND_COLOR_GRADIENT.at(i));
        }
}

void client::Overlay::render(GameWindow& game_window) {
    if (this->is_active) {
        game_window.draw(this->blocker);
        for (sf::RectangleShape& to_draw : this->background) {
            game_window.draw(to_draw);
        }
        game_window.draw(this->title);
        game_window.draw(this->accept.get_text());
        game_window.draw(this->decline.get_text());
    }
}

void client::Overlay::set_active(bool state) {
    if (state) {
        this->game_state.get_mouse_handler().add_clickable_object(this, 0);
        this->game_state.get_mouse_handler().add_clickable_object(&this->accept, 0);
        this->game_state.get_mouse_handler().add_clickable_object(&this->decline, 0);
    } else {
        this->game_state.get_mouse_handler().remove_clickable_object(this);
        this->game_state.get_mouse_handler().remove_clickable_object(&this->accept);
        this->game_state.get_mouse_handler().remove_clickable_object(&this->decline);
    }
    this->is_active = state;
}

void client::Overlay::on_accept() {
    this->set_active(false);
}

void client::Overlay::on_decline() {
    this->set_active(false);
}

void client::Overlay::set_action_names(const std::string& accept, const std::string& decline) {
    this->accept.set_action_name(accept);
    this->decline.set_action_name(decline);
}

void client::Overlay::set_dimensions(sf::Rect<int> dimensions) {
    constexpr float TITLE_OFFSET = 20.0f;
    constexpr float ACTION_BUTTONS_SPACING_X = 30.0f;
    constexpr float ACTION_BUTTONS_SPACING_Y = 55.0f;
    constexpr float HALF = 0.5f;

    this->dimensions = dimensions;

    // mouse event blocker
    this->set_area(sf::Rect<int>{0, 0, (int)this->game_state.get_window_size().x, (int)this->game_state.get_window_size().y});
    this->blocker.setSize(sf::Vector2f{(float)this->game_state.get_window_size().x, (float)this->game_state.get_window_size().y});

    float title_x = (float)dimensions.left + (float)dimensions.width * HALF - this->title.getLocalBounds().width * HALF;
    this->title.setPosition(title_x, (float)dimensions.top + TITLE_OFFSET);

    float button_y = (float)dimensions.top + (float)dimensions.height - ACTION_BUTTONS_SPACING_Y;
    float decline_width = this->decline.get_text().getLocalBounds().width;
    float accept_width = this->accept.get_text().getLocalBounds().width;
    float decline_x = (float)dimensions.left + (float)dimensions.width - decline_width - ACTION_BUTTONS_SPACING_X;
    float accept_x = decline_x - accept_width - ACTION_BUTTONS_SPACING_X;

    this->accept.get_text().setPosition(accept_x, button_y);
    this->decline.get_text().setPosition(decline_x, button_y);
    this->accept.set_area(sf::Rect<int>{this->accept.get_text().getGlobalBounds()});
    this->decline.set_area(sf::Rect<int>{this->decline.get_text().getGlobalBounds()});

    for (int i = 0; i < this->background.size(); i++) {
        this->background.at(i).setSize(sf::Vector2f{
            (float)dimensions.width - (float)i * INV_BORDER_SIZE * HALF,
            (float)dimensions.height - (float)i * INV_BORDER_SIZE * HALF
        });
        this->background.at(i).setPosition(
            (float)dimensions.left + (float)i * INV_BORDER_SIZE,
            (float)dimensions.top + (float)i * INV_BORDER_SIZE
        );
    }
}