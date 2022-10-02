#pragma once

#include <SFML/Graphics.hpp>
#include <iostream>
#include <regex>

#include "network/Socket.h"

#include "base/GameState.h"
#include "base/GameWindow.h"

namespace launcher {
    class LauncherWindow : public sf::RenderWindow {
        const std::regex IP_REGEX{"^((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\\.?\\b){4}"
            ":([1-9]\\d{0,3}|[1-5]\\d{4}|6[1-4]\\d{3}|65[1-4]\\d{2}|655[1-2]\\d|6553[0-5])$"};

        private:
            // background image
            sf::Texture bg_texture;
            sf::Sprite bg_sprite;
            // ip input border + text
            sf::Text input_text;
            sf::Font font;
            std::string input_str;
            // play button
            sf::Rect<int> play_button;
            sf::Vector2i last_mouse_click;
            // credits button
            // settings
            
            void on_key_press(sf::Event::KeyEvent event);

        public:
            LauncherWindow();
            void loop();
            void launch_game();
    };
}