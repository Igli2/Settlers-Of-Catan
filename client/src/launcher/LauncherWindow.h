#pragma once

#include <SFML/Graphics.hpp>
#include <iostream>
#include <regex>

#include "network/Socket.h"

#include "base/GameState.h"
#include "base/GameWindow.h"
#include "base/LocalizationManager.h"

namespace launcher {
    enum LauncherView {
        LAUNCHER,
        CREDITS
    };

    class LauncherWindow : public sf::RenderWindow {
        const std::regex IP_REGEX{"^((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\\.?\\b){4}"
            ":([1-9]\\d{0,3}|[1-5]\\d{4}|6[1-4]\\d{3}|65[1-4]\\d{2}|655[1-2]\\d|6553[0-5])$"};

        private:
            // localization
            client::LocalizationManager localization_manager;
            // background image
            sf::Texture bg_texture;
            sf::Texture bg_texture_credits;
            sf::Sprite bg_sprite;
            // ip input border + text
            sf::Text input_text;
            sf::Font font;
            std::string input_str;
            // play button
            sf::Rect<int> play_button;
            sf::Text play_button_text;
            sf::Rect<int> credits_button;
            sf::Text credits_button_text;
            sf::Rect<int> back_button;
            sf::Text back_button_text;
            sf::Vector2i last_mouse_click;
            LauncherView view;
            // settings
            
            void on_key_press(sf::Event::KeyEvent event);
            void create_text(sf::Text& sf_text, std::string text_localization, float x, float y);

        public:
            LauncherWindow();
            void render_loop();
            void launch_game();
    };
}