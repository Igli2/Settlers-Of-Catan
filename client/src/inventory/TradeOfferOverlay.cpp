#include "TradeOfferOverlay.h"

#include "base/GameState.h"
#include "base/GameWindow.h"

// TODO use array from Inventory.cpp
const std::array<std::string, client::ResourceType::RESOURCE_MAX> resource_texture_names = {
    "grain",
    "brick",
    "ore",
    "wool",
    "lumber"
};





client::TradeOfferResource::TradeOfferResource() : Clickable{sf::Rect<int>{}} {}

client::TradeOfferResource::TradeOfferResource(GameState& game_state, ResourceType type, TradeOfferOverlay* overlay, TradeParty trade_party) :
    Clickable{sf::Rect<int>{}},
    res_count{0},
    overlay{overlay},
    type{type} {
        this->res_texture.setTexture(game_state.get_texture_manager().get_texture(resource_texture_names[type]));

        float scale = 50.0f / this->res_texture.getTexture()->getSize().x;
        this->res_texture.setScale(scale, scale);

        this->count_text.setFillColor(sf::Color::Black);
        this->count_text.setFont(game_state.get_texture_manager().get_font());
        this->count_text.setString(std::to_string(this->res_count));
        this->count_text.setCharacterSize(22);
}

bool client::TradeOfferResource::on_click(GameState& game_state, sf::Mouse::Button button) {
    if (button == sf::Mouse::Button::Left) {
        if (game_state.get_inventory().get_resource(this->type) > this->res_count) {
            this->res_count++;
            this->count_text.setString(std::to_string(this->res_count));
        }
        return true;
    } else if (button == sf::Mouse::Button::Right && this->res_count > 0) {
        this->res_count--;
        this->count_text.setString(std::to_string(this->res_count));
        return true;
    }
    return false;
}

void client::TradeOfferResource::set_position(float x, float y) {
    this->res_texture.setPosition(x, y);
    this->count_text.setPosition(x + 55.0f, y + 15.0f);
    this->set_area(sf::Rect<int>{(int)x, (int)y, (int)this->res_texture.getGlobalBounds().width, (int)this->res_texture.getGlobalBounds().height});
}

void client::TradeOfferResource::render(GameWindow& game_window, GameState& game_state) {
    game_window.draw(this->res_texture);
    game_window.draw(this->count_text);
}

bool client::TradeOfferResource::is_selected() {
    return this->res_count > 0;
}





client::TradeOfferOverlay::TradeOfferOverlay(GameState& game_state) :
    Overlay{game_state, game_state.get_localization_manager().get_translation("trade_title")} {
        this->set_action_names(game_state.get_localization_manager().get_translation("trade_overlay_request"), game_state.get_localization_manager().get_translation("trade_overlay_cancel"));
        this->selection_border_offer.setOutlineThickness(3);
        this->selection_border_request.setOutlineThickness(3);
        this->selection_border_offer.setOutlineColor(sf::Color::Red);
        this->selection_border_request.setOutlineColor(sf::Color::Green);
        this->selection_border_offer.setSize(sf::Vector2f{50, 50});
        this->selection_border_request.setSize(sf::Vector2f{50, 50});
        this->selection_border_offer.setFillColor(sf::Color::Transparent);
        this->selection_border_request.setFillColor(sf::Color::Transparent);

        for (int i = 0; i < ResourceType::RESOURCE_MAX; i++) {
            this->request_buttons[i] = TradeOfferResource{game_state, (ResourceType)i, this, TradeParty::REQUEST};
            this->offer_buttons[i] = TradeOfferResource{game_state, (ResourceType)i, this, TradeParty::OFFER};
        }
}

void client::TradeOfferOverlay::render(GameWindow& game_window, GameState& game_state) {
    Overlay::render(game_window, game_state);

    if (this->is_active) {
        for (int i = 0; i < ResourceType::RESOURCE_MAX; i++) {
            this->request_buttons[i].render(game_window, game_state);
            if (this->request_buttons[i].is_selected()) {
                this->selection_border_request.setPosition(dimensions.left + i * TRADE_RESOURCE_SPACING + 20.0f, dimensions.top + 100.0f);
                game_window.draw(this->selection_border_request);
            }

            this->offer_buttons[i].render(game_window, game_state);
            if (this->offer_buttons[i].is_selected()) {
                this->selection_border_offer.setPosition(dimensions.left + i * TRADE_RESOURCE_SPACING + 20.0f, dimensions.top + 160.0f);
                game_window.draw(this->selection_border_offer);
            }
        }
    }
}

void client::TradeOfferOverlay::set_dimensions(GameState& game_state, sf::Rect<int> dimensions) {
    Overlay::set_dimensions(game_state, dimensions);

    for (int i = 0; i < ResourceType::RESOURCE_MAX; i++) {
        this->request_buttons[i].set_position(dimensions.left + i * TRADE_RESOURCE_SPACING + 20.0f, dimensions.top + 100.0f);
        this->offer_buttons[i].set_position(dimensions.left + i * TRADE_RESOURCE_SPACING + 20.0f, dimensions.top + 160.0f);
    }
}

void client::TradeOfferOverlay::set_active(GameState& game_state, bool state) {
    Overlay::set_active(game_state, state);

    if (state) {
        for (int i = 0; i < ResourceType::RESOURCE_MAX; i++) {
            game_state.add_clickable_object(&this->request_buttons[i], 0);
            game_state.add_clickable_object(&this->offer_buttons[i], 0);
        }
    } else {
        for (int i = 0; i < ResourceType::RESOURCE_MAX; i++) {
            game_state.remove_clickable_object(&this->request_buttons[i]);
            game_state.remove_clickable_object(&this->offer_buttons[i]);
        }
    }
}