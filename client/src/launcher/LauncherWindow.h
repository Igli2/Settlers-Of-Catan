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
            // ip input border + text
            sf::Text ip_text;
            sf::Text name_text;
            sf::Font font;
            std::string ip_str;
            std::string name_str;
            std::string* focused_string;
            // buttons
            client::ClickableText play;
            client::ClickableText credits;
            client::ClickableText back;
            sf::Vector2i last_mouse_click;
            LauncherView view;
            // settings
            // TODO
            // server connection
            network::Socket server_connection;
            
            void on_key_press(sf::Event::KeyEvent event);
            void create_text(client::ClickableText& text, std::string text_localization, float x, float y);
            void handle_mouse_press(const sf::Event& event);
            void handle_mouse_release(const sf::Event& event);
            char key_to_char(sf::Keyboard::Key key, bool shift);
            void launch_game();
            void connect();

        public:
            LauncherWindow();
            void render_loop();
    };
}