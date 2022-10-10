#include "OverlayAction.h"
#include "Overlay.h"
#include "GameState.h"

client::OverlayAction::OverlayAction(GameState& game_state, Overlay* overlay_p, OverlayActionType type) :
    overlay_p{overlay_p},
    type{type},
    ClickableText{game_state.get_texture_manager().get_font()} {
        if (this->type == OverlayActionType::ACCEPT) {
            this->set_string(game_state.get_localization_manager().get_translation("overlay_accept"));
        } else {
            this->set_string(game_state.get_localization_manager().get_translation("overlay_decline"));
        }
}

bool client::OverlayAction::on_click(sf::Mouse::Button button) {
    if (button == sf::Mouse::Button::Left) {
        if (this->type == OverlayActionType::ACCEPT) {
            this->overlay_p->on_accept();
        } else {
            this->overlay_p->on_decline();
        }
        return true;
    }
    return false;
}

void client::OverlayAction::set_action_name(std::string name) {
    this->set_string(name);
}