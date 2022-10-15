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
    view{LauncherView::LAUNCHER},
    focused_string{&this->ip_str},
    server_connection(network::Socket::TCP) {
        this->setFramerateLimit(30);

        if (!this->font.loadFromFile(RESOURCE_FOLDER"/fonts/OpenSans.ttf")) {
            std::cout << "Cannot load font for launcher" << std::endl;
        }
        this->ip_text.setFont(this->font);
        this->ip_text.setCharacterSize(24);
        this->ip_text.setFillColor(sf::Color::Black);
        this->ip_text.setString(this->localization_manager.get_translation("ip_port"));
        this->ip_text.setPosition(400 - this->ip_text.getLocalBounds().width / 2, 218);

        this->name_text.setFont(this->font);
        this->name_text.setCharacterSize(24);
        this->name_text.setFillColor(sf::Color::Black);
        this->name_text.setString(this->localization_manager.get_translation("your_name"));
        this->name_text.setPosition(400 - this->name_text.getLocalBounds().width / 2, 255);

        this->create_text(this->play, "play", 229, 290);
        this->create_text(this->credits, "credits", 229, 327);
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
                // ip and name input
                this->on_key_press(event.key);
            }
        }

        this->clear(sf::Color{120, 120, 220});

        this->draw(this->bg_sprite);
        if (this->view == LauncherView::LAUNCHER) {
            this->draw(this->ip_text);
            this->draw(this->name_text);
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
            this->connect();
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

void launcher::LauncherWindow::connect() {
    if (!std::regex_match(this->ip_str, LauncherWindow::IP_REGEX)) {
        return;
    }
    if (this->name_str.length() == 0) {
        return;
    }

    const size_t port_seperator = this->ip_str.find(':');
    std::string ip = this->ip_str.substr(0, port_seperator);
    short port = std::stoi(this->ip_str.substr(port_seperator + 1));

    this->server_connection.connect(ip, port);
    if (this->server_connection.get_status() == network::Socket::SocketStatus::ERROR) {
        this->ip_str = "";
        this->ip_text.setString(this->localization_manager.get_translation("cant_connect"));
        this->ip_text.setPosition(400 - this->ip_text.getLocalBounds().width / 2, 218);
        return;
    }

    // send connect request
    this->server_connection.send(network::Packet{network::PacketType::CONNECT_REQUEST});

    // answers:
    // nope
    // name request
    // token request

    // answer on name/token:
    // success
    this->launch_game();
}

void launcher::LauncherWindow::launch_game() {
    this->setVisible(false);

    client::GameState game_state(this->server_connection);
    client::GameWindow game_window{game_state};
    game_state.get_hexmap().set_game_window(&game_window);
    game_window.game_loop(game_state);

    this->setVisible(true);
}

void launcher::LauncherWindow::on_key_press(sf::Event::KeyEvent event) {
    if (this->view == LauncherView::CREDITS) {
        return;
    }

    char key_c = this->key_to_char(event.code, event.shift);
    if (key_c == '\0') {
        // handle special characters, etc.
        switch (event.code) {
            case sf::Keyboard::Period:
                if (this->focused_string == &this->name_str) { break; }
                if (event.shift) {
                    *this->focused_string += ":";
                } else {
                    *this->focused_string += ".";
                }
                break;
            case sf::Keyboard::BackSpace:
                if (this->focused_string->length() > 0) {
                    this->focused_string->pop_back();
                }
                break;
            case sf::Keyboard::Tab:
                if (this->focused_string == &this->ip_str) {
                    this->focused_string = &this->name_str;
                } else {
                    this->focused_string = &this->ip_str;
                }
                break;
        }
    } else if (this->focused_string->length() <= 21) {
        *this->focused_string += key_c;
    }


    if (this->ip_str.empty()) {
        this->ip_text.setString(this->localization_manager.get_translation("ip_port"));
    } else {
        this->ip_text.setString(this->ip_str);
    }
    if (this->name_str.empty()) {
        this->name_text.setString(this->localization_manager.get_translation("your_name"));
    } else {
        this->name_text.setString(this->name_str);
    }
    this->ip_text.setPosition(400 - this->ip_text.getLocalBounds().width / 2, 218);
    this->name_text.setPosition(400 - this->name_text.getLocalBounds().width / 2, 255);
}

char launcher::LauncherWindow::key_to_char(sf::Keyboard::Key key, bool shift) {
    if (key >= sf::Keyboard::Num0 && key <= sf::Keyboard::Num9) {
        return (int)key + 22;
    }
    if (key >= sf::Keyboard::A && key <= sf::Keyboard::Z) {
        if (shift) {
            return (int)key + 65;
        } else {
            return (int)key + 97;
        }
    }
    return '\0';
}