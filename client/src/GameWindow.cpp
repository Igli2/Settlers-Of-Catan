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
            }
        }

        this->clear(sf::Color{120, 120, 220});

        game_state.get_inventory().render(*this, game_state);

        this->display();
    }
}