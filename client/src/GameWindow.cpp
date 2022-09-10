#include "GameWindow.h"

client::GameWindow::GameWindow(client::GameState& game_state) : sf::RenderWindow{sf::VideoMode(500, 500), "Settlers Of Catan"} {
    this->setFramerateLimit(30);
}

void client::GameWindow::game_loop(client::GameState& game_state) {
    while (this->isOpen()) {
        sf::Event event;
        while (this->pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                this->close();
            }
        }

        this->clear();

        this->display();
    }
}