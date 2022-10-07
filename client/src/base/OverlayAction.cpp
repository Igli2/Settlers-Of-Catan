#include "OverlayAction.h"
#include "Overlay.h"
#include "GameState.h"

constexpr size_t TITLE_FONT_SIZE = 28;

client::OverlayAction::OverlayAction(GameState& game_state, Overlay* overlay_p, OverlayActionType type) :
    overlay_p{overlay_p},
    type{type},
    Clickable{sf::Rect<int>{}} {
        this->text.setFillColor(sf::Color::Black);
        this->text.setFont(game_state.get_texture_manager().get_font());
        this->text.setCharacterSize(TITLE_FONT_SIZE);
        if (this->type == OverlayActionType::ACCEPT) {
            this->text.setString(game_state.get_localization_manager().get_translation("overlay_accept"));
        } else {
            this->text.setString(game_state.get_localization_manager().get_translation("overlay_decline"));
        }
}

bool client::OverlayAction::on_click(GameState& game_state, sf::Mouse::Button button) {
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

sf::Text& client::OverlayAction::get_text() {
    return this->text;
}

void client::OverlayAction::set_action_name(const std::string& name) {
    this->text.setString(name);
}

/*
 "-*",
        "-clang-diagnostics-*",
        "cppcoreguidelines-*",
        "clang-analyzer-*",
        "bugprone-*",
        "readability-*",
        "performance-*",
        "-readability-uppercase-literal-suffix"
*/