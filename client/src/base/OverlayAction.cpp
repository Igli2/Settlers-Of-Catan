#include "OverlayAction.h"
#include "Overlay.h"
#include "GameState.h"

client::OverlayAction::OverlayAction(GameState& game_state, Overlay* overlay_p, OverlayActionType type) :
    overlay_p{overlay_p},
    type{type},
    Clickable{sf::Rect<int>{}} {
        this->text.setFillColor(sf::Color::Black);
        this->text.setFont(game_state.get_texture_manager().get_font());
        this->text.setCharacterSize(28);
        if (this->type == OverlayActionType::ACCEPT) {
            this->text.setString(game_state.get_localization_manager().get_translation("overlay_accept"));
        } else {
            this->text.setString(game_state.get_localization_manager().get_translation("overlay_decline"));
        }
}

bool client::OverlayAction::on_click(GameState& game_state, sf::Mouse::Button button) {
    if (this->type == OverlayActionType::ACCEPT) {
        this->overlay_p->on_accept(game_state);
    } else {
        this->overlay_p->on_decline(game_state);
    }
    return true;
}

sf::Text& client::OverlayAction::get_text() {
    return this->text;
}

void client::OverlayAction::set_action_name(std::string name) {
    this->text.setString(name);
}