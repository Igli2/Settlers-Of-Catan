#include "LauncherWindow.h"

launcher::LauncherWindow::LauncherWindow() :
    sf::RenderWindow{
        sf::VideoMode(800, 600),
        "Settlers Of Catan - Launcher",
        sf::Style::Titlebar | sf::Style::Close
    },
    play{this->font},
    credits{this->font},
    back{this->font},
    view{LauncherView::LAUNCHER} {
        this->setFramerateLimit(30);

        if (!this->font.loadFromFile(RESOURCE_FOLDER"/fonts/OpenSans.ttf")) {
            std::cout << "Cannot load font for launcher" << std::endl;
        }
        this->input_text.setFont(this->font);
        this->input_text.setCharacterSize(24);
        this->input_text.setFillColor(sf::Color::Black);

        this->create_text(this->play, "play", 229, 253);
        this->create_text(this->credits, "credits", 229, 290);
        this->create_text(this->back, "back", 229, 216);

        if (!this->bg_texture.loadFromFile(RESOURCE_FOLDER"/textures/launcher_background.png")) {
            std::cout << "Cannot load texture for launcher" << std::endl;
        }
        if (!this->bg_texture_credits.loadFromFile(RESOURCE_FOLDER"/textures/launcher_credits.png")) {
            std::cout << "Cannot load texture for credits" << std::endl;
        }
        this->bg_sprite.setTexture(this->bg_texture);
}

void launcher::LauncherWindow::create_text(client::ClickableText& text, std::string text_localization, float x, float y) {
    text.set_string(this->localization_manager.get_translation(text_localization));
    text.set_bounds(sf::Rect<int>{(int)x, (int)y, 336, 28});
}

void launcher::LauncherWindow::render_loop() {
    while (this->isOpen()) {
        sf::Event event;
        while (this->pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                this->close();
            } else if (event.type == sf::Event::Resized) {
                sf::FloatRect visibleArea(0, 0, event.size.width, event.size.height);
                this->setView(sf::View(visibleArea));
            } else if (event.type == sf::Event::MouseButtonPressed) {
                this->handle_mouse_press(event);
            } else if (event.type == sf::Event::MouseButtonReleased) {
                this->handle_mouse_release(event);
            } else if (event.type == sf::Event::KeyPressed) {
                // ip input
                // only numbers and colons
                this->on_key_press(event.key);
            }
        }

        this->clear(sf::Color{120, 120, 220});

        this->draw(this->bg_sprite);
        if (this->view == LauncherView::LAUNCHER) {
            this->draw(this->input_text);
            this->draw(this->play.get_render_text());
            this->draw(this->credits.get_render_text());
        } else if (this->view == LauncherView::CREDITS) {
            this->draw(this->back.get_render_text());
        }

        this->display();
    }
}

void launcher::LauncherWindow::handle_mouse_press(const sf::Event& event) {
    this->last_mouse_click = sf::Vector2i{event.mouseButton.x, event.mouseButton.y};
    if (this->view == LauncherView::CREDITS) {
        if (this->back.contains(this->last_mouse_click)) {
            this->back.on_press(event.mouseButton.button);
        }
    } else {
        if (this->play.contains(this->last_mouse_click)) {
            this->play.on_press(event.mouseButton.button);
        } else if (this->credits.contains(this->last_mouse_click)) {
            this->credits.on_press(event.mouseButton.button);
        }
    }
}

void launcher::LauncherWindow::handle_mouse_release(const sf::Event& event) {
    // check button clicks
    sf::Vector2i mouse_pos{event.mouseButton.x, event.mouseButton.y};
    if (this->view == LauncherView::CREDITS) {
        if (this->back.contains(this->last_mouse_click) && this->back.contains(mouse_pos)) {
            this->bg_sprite.setTexture(this->bg_texture);
            this->view = LauncherView::LAUNCHER;
        }
    } else {
        if (this->play.contains(this->last_mouse_click) && this->play.contains(mouse_pos)) {
            this->launch_game();
        } else if (this->credits.contains(this->last_mouse_click) && this->credits.contains(mouse_pos)) {
            // view credits
            this->bg_sprite.setTexture(this->bg_texture_credits);
            this->view = LauncherView::CREDITS;
        }
    }
    this->back.on_release(event.mouseButton.button);
    this->credits.on_release(event.mouseButton.button);
    this->play.on_release(event.mouseButton.button);

    this->last_mouse_click = sf::Vector2i{-1, -1};
}

void launcher::LauncherWindow::launch_game() {
    network::Socket server_connection(network::Socket::TCP);

    if (!std::regex_match(this->input_str, LauncherWindow::IP_REGEX)) {
        return;
    }

    const size_t port_seperator = this->input_str.find(':');
    std::string ip = this->input_str.substr(0, port_seperator);
    short port = std::stoi(this->input_str.substr(port_seperator + 1));

    server_connection.connect(ip, port);
    if (server_connection.get_status() == network::Socket::SocketStatus::ERROR) {
        this->input_str = "";
        this->input_text.setString(this->localization_manager.get_translation("cant_connect"));
        this->input_text.setPosition(400 - this->input_text.getLocalBounds().width / 2, 218);
        return;
    }

    this->setVisible(false);

    client::GameState game_state(server_connection);
    client::GameWindow game_window{game_state};
    game_state.get_hexmap().set_game_window(&game_window);
    game_window.game_loop(game_state);

    this->setVisible(true);
}

void launcher::LauncherWindow::on_key_press(sf::Event::KeyEvent event) {
    if (this->input_str.length() >= 21 && event.code != sf::Keyboard::BackSpace) {
        return;
    }
    if (this->view == LauncherView::CREDITS) {
        return;
    }

    switch (event.code) {
        case sf::Keyboard::Num0:
            this->input_str += "0";
            break;
        case sf::Keyboard::Num1:
            this->input_str += "1";
            break;
        case sf::Keyboard::Num2:
            this->input_str += "2";
            break;
        case sf::Keyboard::Num3:
            this->input_str += "3";
            break;
        case sf::Keyboard::Num4:
            this->input_str += "4";
            break;
        case sf::Keyboard::Num5:
            this->input_str += "5";
            break;
        case sf::Keyboard::Num6:
            this->input_str += "6";
            break;
        case sf::Keyboard::Num7:
            this->input_str += "7";
            break;
        case sf::Keyboard::Num8:
            this->input_str += "8";
            break;
        case sf::Keyboard::Num9:
            this->input_str += "9";
            break;
        case sf::Keyboard::Period:
            if (event.shift) {
                this->input_str += ":";
            } else {
                this->input_str += ".";
            }
            break;
        case sf::Keyboard::BackSpace:
            if (this->input_str.length() > 0) {
                this->input_str.pop_back();
            }
            break;
    }
    this->input_text.setString(this->input_str);
    this->input_text.setPosition(400 - this->input_text.getLocalBounds().width / 2, 218);
}