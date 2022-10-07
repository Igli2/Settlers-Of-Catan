#include "GameWindow.h"

constexpr size_t MAX_FRAME_RATE = 30;
const sf::Color BACKGROUND_COLOR = sf::Color{120, 120, 220};

client::GameWindow::GameWindow(client::GameState& game_state) :
    sf::RenderWindow{
        sf::VideoMode(game_state.get_window_size().x, game_state.get_window_size().y),
        "Settlers Of Catan - Game"
    } {
        this->setFramerateLimit(MAX_FRAME_RATE);
}

void client::GameWindow::game_loop(client::GameState& game_state) {
    KeyboardHandler keyboard_handler{game_state};

    while (this->isOpen()) {
        sf::Event event{};
        while (this->pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                this->close();
            } else if (event.type == sf::Event::Resized) {
                sf::FloatRect visibleArea(0, 0, (float)event.size.width, (float)event.size.height);
                this->setView(sf::View(visibleArea));
                game_state.set_window_size(sf::Vector2u{event.size.width, event.size.height});
                game_state.resize();
            } else if (event.type == sf::Event::MouseButtonPressed) {
                game_state.get_mouse_handler().call_mouse_press(
                    event.mouseButton.button,
                    sf::Vector2i{event.mouseButton.x, event.mouseButton.y}
                );
            } else if (event.type == sf::Event::MouseButtonReleased) {
                game_state.get_mouse_handler().call_mouse_release(
                    event.mouseButton.button,
                    sf::Vector2i{event.mouseButton.x, event.mouseButton.y}
                );
            } else if (event.type == sf::Event::MouseMoved) {
                game_state.get_mouse_handler().call_mouse_move(
                    sf::Vector2i{event.mouseMove.x, event.mouseMove.y}
                );
            } else if (event.type == sf::Event::KeyPressed) {
                keyboard_handler.on_key_pressed(event.key);
            } else if (event.type == sf::Event::KeyReleased) {
                keyboard_handler.on_key_released(event.key);
            } else if (event.type == sf::Event::MouseWheelScrolled) {
                const float default_zoom = 1.0f;
                const float zoom_speed = 0.05f;

                float zoom = default_zoom - zoom_speed * event.mouseWheelScroll.delta;
                game_state.get_hexmap().zoom *= zoom;
                game_state.map_view.zoom(zoom);
            }
        }

        keyboard_handler.update_view(game_state.get_hexmap().zoom);

        this->clear(BACKGROUND_COLOR);

        this->setView(game_state.map_view);
        game_state.get_hexmap().render(*this, game_state);
        this->setView(game_state.inventory_view);
        game_state.get_inventory().render(*this, game_state);

        this->display();
    }
}