#include "LauncherWindow.h"

launcher::LauncherWindow::LauncherWindow() :
    sf::RenderWindow{
        sf::VideoMode(800, 600),
        "Settlers Of Catan - Launcher",
        sf::Style::Titlebar | sf::Style::Close
    },
    play_button{229, 253, 342, 33},
    credits_button{229, 290, 342, 33},
    back_button{229, 216, 342, 33},
    view{LauncherView::LAUNCHER} {
        this->setFramerateLimit(30);

        if (!this->font.loadFromFile(RESOURCE_FOLDER"/fonts/OpenSans.ttf")) {
            std::cout << "Cannot load font for launcher" << std::endl;
        }
        this->input_text.setFont(this->font);
        this->input_text.setCharacterSize(24);
        this->input_text.setFillColor(sf::Color::Black);

        this->create_text(this->play_button_text, "play", 400, 255);
        this->create_text(this->credits_button_text, "credits", 400, 292);
        this->create_text(this->back_button_text, "back", 400, 218);

        if (!this->bg_texture.loadFromFile(RESOURCE_FOLDER"/textures/launcher_background.png")) {
            std::cout << "Cannot load texture for launcher" << std::endl;
        }
        if (!this->bg_texture_credits.loadFromFile(RESOURCE_FOLDER"/textures/launcher_credits.png")) {
            std::cout << "Cannot load texture for credits" << std::endl;
        }
        this->bg_sprite.setTexture(this->bg_texture);
}

void launcher::LauncherWindow::create_text(sf::Text& sf_text, std::string text_localization, float x, float y) {
    sf_text.setFont(this->font);
    sf_text.setCharacterSize(24);
    sf_text.setFillColor(sf::Color::Black);
    sf_text.setString(this->localization_manager.get_translation(text_localization));
    sf_text.setPosition(x - sf_text.getLocalBounds().width / 2, y);
    sf_text.setStyle(sf::Text::Bold);
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
                this->last_mouse_click = sf::Vector2i{event.mouseButton.x, event.mouseButton.y};
                if (this->view == LauncherView::CREDITS) {
                    if (this->back_button.contains(this->last_mouse_click)) {
                        this->back_button_text.setFillColor(sf::Color::White);
                    }
                } else {
                    if (this->play_button.contains(this->last_mouse_click)) {
                        this->play_button_text.setFillColor(sf::Color::White);
                    } else if (this->credits_button.contains(this->last_mouse_click)) {
                        this->credits_button_text.setFillColor(sf::Color::White);
                    }
                }
            } else if (event.type == sf::Event::MouseButtonReleased) {
                // check button clicks
                sf::Vector2i mouse_pos{event.mouseButton.x, event.mouseButton.y};
                if (this->view == LauncherView::CREDITS) {
                    if (this->back_button.contains(this->last_mouse_click) && this->back_button.contains(mouse_pos)) {
                        this->bg_sprite.setTexture(this->bg_texture);
                        this->view = LauncherView::LAUNCHER;
                    }
                } else {
                    if (this->play_button.contains(this->last_mouse_click) && this->play_button.contains(mouse_pos)) {
                        this->launch_game();
                    } else if (this->credits_button.contains(this->last_mouse_click) && this->credits_button.contains(mouse_pos)) {
                        // view credits
                        this->bg_sprite.setTexture(this->bg_texture_credits);
                        this->view = LauncherView::CREDITS;
                    }
                }
                this->back_button_text.setFillColor(sf::Color::Black);
                this->credits_button_text.setFillColor(sf::Color::Black);
                this->play_button_text.setFillColor(sf::Color::Black);

                this->last_mouse_click = sf::Vector2i{-1, -1};
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
            this->draw(this->play_button_text);
            this->draw(this->credits_button_text);
        } else if (this->view == LauncherView::CREDITS) {
            this->draw(this->back_button_text);
        }

        this->display();
    }
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