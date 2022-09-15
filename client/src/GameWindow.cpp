#include "GameWindow.h"

client::GameWindow::GameWindow(client::GameState& game_state) : sf::RenderWindow{sf::VideoMode(game_state.get_window_size().x, game_state.get_window_size().y), "Settlers Of Catan"} {
    this->setFramerateLimit(30);
}

void client::GameWindow::game_loop(client::GameState& game_state) {
    while (this->isOpen()) {
        sf::Event event;
        while (this->pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                this->close();
            } else if (event.type == sf::Event::Resized) {
                sf::FloatRect visibleArea(0, 0, event.size.width, event.size.height);
                this->setView(sf::View(visibleArea));
                game_state.set_window_size(sf::Vector2u{event.size.width, event.size.height});
                game_state.resize();
            } else if (event.type == sf::Event::MouseButtonPressed) {
                game_state.call_mouse_press(event.mouseButton.button, sf::Vector2i{event.mouseButton.x, event.mouseButton.y});
            } else if (event.type == sf::Event::MouseButtonReleased) {
                game_state.call_mouse_release(event.mouseButton.button, sf::Vector2i{event.mouseButton.x, event.mouseButton.y});
            } else if (event.type == sf::Event::MouseMoved) {
                game_state.call_mouse_move(sf::Vector2i{event.mouseMove.x, event.mouseMove.y});
            }
        }

        this->clear(sf::Color{120, 120, 220});

        game_state.get_inventory().render(*this, game_state);

        this->display();
    }
}