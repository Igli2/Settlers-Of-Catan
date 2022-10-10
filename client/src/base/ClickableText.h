#pragma once

#include <SFML/Graphics.hpp>
#include <string>

#include "base/Clickable.h"

namespace client {
    class ClickableText : public Clickable {
        private:
            sf::Rect<int> bounds; // area where the text is centered in
            sf::Text text;

            void update();
        public:
            ClickableText(const sf::Font& font);
            ClickableText(const sf::Font& font, sf::Rect<int> bounds);
            ClickableText(const sf::Font& font, std::string string);
            ClickableText(const sf::Font& font, sf::Rect<int> bounds, std::string string);
            void set_string(std::string string);
            void set_bounds(sf::Rect<int> bounds);
            const sf::Text& get_render_text();
            bool on_press(sf::Mouse::Button button) override;
            bool on_release(sf::Mouse::Button button) override;
    };
}