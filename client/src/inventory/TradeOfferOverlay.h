#pragma once

#include <SFML/Graphics.hpp>
#include <array>
#include <string>

#include "Resource.h"

#include "base/Overlay.h"
#include "base/Clickable.h"

#define TRADE_RESOURCE_SPACING 90.0f

namespace client {
    class GameState;
    class GameWindow;
    class TradeOfferOverlay;

    enum TradeParty {
        REQUEST,
        OFFER
    };

    class TradeOfferResource : public Clickable {
        private:
            unsigned int res_count;
            sf::Sprite res_texture;
            sf::Text count_text;
            TradeOfferOverlay* overlay;
            TradeParty trade_party;
            ResourceType type;
        public:
            TradeOfferResource();
            TradeOfferResource(GameState& game_state, ResourceType type, TradeOfferOverlay* overlay, TradeParty trade_party);
            bool on_click(GameState& game_state, sf::Mouse::Button button) override;
            void set_position(float x, float y);
            void render(GameWindow& game_window, GameState& game_state);
            bool is_selected();
    };

    class TradeOfferOverlay : public Overlay {
        private:
            sf::RectangleShape selection_border_offer;
            sf::RectangleShape selection_border_request;
            std::array<TradeOfferResource, ResourceType::RESOURCE_MAX> request_buttons;
            std::array<TradeOfferResource, ResourceType::RESOURCE_MAX> offer_buttons;
        public:
            TradeOfferOverlay(GameState& game_state);
            void render(GameWindow& game_window, GameState& game_state);
            void set_dimensions(GameState& game_state, sf::Rect<int> dimensions);
            void set_active(GameState& game_state, bool state);
    };
}