#pragma once

#include <SFML/Graphics.hpp>
#include <array>
#include <string>

#include "Resource.h"

#include "base/Overlay.h"
#include "base/Clickable.h"
#include "base/Registry.h"

namespace client {
    class GameState;
    class GameWindow;
    class TradeOfferOverlay;

    class TradeOfferResource : public Clickable {
        private:
            unsigned int res_count;
            sf::Sprite res_texture;
            sf::Text count_text;
            TradeOfferOverlay* overlay;
            ResourceType type;
        public:
            TradeOfferResource();
            TradeOfferResource(GameState& game_state, ResourceType type, TradeOfferOverlay* overlay);
            bool on_click(GameState& game_state, sf::Mouse::Button button) override;
            void set_position(float x, float y);
            void render(GameWindow& game_window, GameState& game_state);
            bool is_selected();
    };

    class TradeOfferOverlay : public Overlay {
        const float TRADE_RESOURCE_SPACING = 90.0f;
        private:
            sf::RectangleShape selection_border_offer;
            sf::RectangleShape selection_border_request;
            std::array<TradeOfferResource, ResourceType::RESOURCE_MAX> request_buttons;
            std::array<TradeOfferResource, ResourceType::RESOURCE_MAX> offer_buttons;
        public:
            TradeOfferOverlay(GameState& game_state);
            void render(GameWindow& game_window);
            void set_dimensions(sf::Rect<int> dimensions);
            void set_active(bool state);
            // TODO on_accept: send packet
    };
}