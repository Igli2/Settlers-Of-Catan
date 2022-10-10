#include "ClickableText.h"

client::ClickableText::ClickableText(const sf::Font& font) : ClickableText{font, sf::Rect<int>{}, ""} {}

client::ClickableText::ClickableText(const sf::Font& font, sf::Rect<int> bounds) : ClickableText{font, bounds, ""} {}

client::ClickableText::ClickableText(const sf::Font& font, std::string string) : ClickableText{font, sf::Rect<int>{}, string} {}

client::ClickableText::ClickableText(const sf::Font& font, sf::Rect<int> bounds, std::string string) :
    Clickable{sf::Rect<int>{}},
    bounds{bounds} {
        this->text.setFont(font);
        this->text.setCharacterSize(24);
        this->text.setFillColor(sf::Color::Black);
        this->text.setStyle(sf::Text::Bold);

        this->text.setString(string);
}

void client::ClickableText::set_string(std::string string) {
    this->text.setString(string);
    this->update();
}

void client::ClickableText::set_bounds(sf::Rect<int> bounds) {
    this->bounds = bounds;
    this->update();
}

void client::ClickableText::update() {
    this->text.setPosition(
        this->bounds.left + this->bounds.width / 2 - this->text.getGlobalBounds().width / 2,
        this->bounds.top + 1
    );
    this->set_area(sf::Rect<int>{this->text.getGlobalBounds()});
}

const sf::Text& client::ClickableText::get_render_text() {
    return this->text;
}

bool client::ClickableText::on_press(sf::Mouse::Button button) {
    this->text.setFillColor(sf::Color::White);
    return false;
}

bool client::ClickableText::on_release(sf::Mouse::Button button) {
    this->text.setFillColor(sf::Color::Black);
    return false;
}