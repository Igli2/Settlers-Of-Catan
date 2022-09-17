#include "Trade.h"

#include "base/GameState.h"
#include "base/GameWindow.h"

client::Trade::Trade(GameState& game_state) :
    Resizable{game_state},
    Clickable{game_state, sf::Rect<int>{}},
    overlay{game_state, game_state.get_localization_manager().get_translation("trade_title")} {
        this->setTexture(game_state.get_texture_manager().get_texture("trade"));
}

void client::Trade::on_resize(GameState& game_state) {
    this->setPosition(game_state.get_window_size().x - 60.0f, 10.0f);
    this->set_area(sf::Rect<int>{(int)this->getPosition().x, (int)this->getPosition().y, (int)this->getTexture()->getSize().x, (int)this->getTexture()->getSize().y});
    this->overlay.set_dimensions(game_state, sf::Rect<int>{150, 20, (int)game_state.get_window_size().x - 300, (int)(game_state.get_window_size().y / 3.0f * 2.0f - 40.0f)});
}

bool client::Trade::on_click(GameState& game_state, sf::Mouse::Button button) {
    this->overlay.set_active(game_state, true);
    return true;
}

void client::Trade::render(GameWindow& game_window, GameState& game_state) {
    game_window.draw(*this);
    this->overlay.render(game_window, game_state);
    // make derived overlay with resources
}