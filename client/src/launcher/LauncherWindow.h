#pragma once

#include <SFML/Graphics.hpp>
#include <iostream>
#include <regex>

#include "network/Socket.h"

#include "base/GameState.h"
#include "base/GameWindow.h"
#include "base/LocalizationManager.h"
#include "base/ClickableText.h"

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
            // ip input
            sf::Text input_text;
            sf::Font font;
            std::string input_str;
            // buttons
            client::ClickableText play;
            client::ClickableText credits;
            client::ClickableText back;
            sf::Vector2i last_mouse_click;
            LauncherView view;
            // TODO: settings
            
            void on_key_press(sf::Event::KeyEvent event);
            void create_text(client::ClickableText& text, std::string text_localization, float x, float y);
            void handle_mouse_press(const sf::Event& event);
            void handle_mouse_release(const sf::Event& event);

        public:
            LauncherWindow();
            void render_loop();
            void launch_game();
    };
}