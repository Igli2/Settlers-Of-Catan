#include "GameState.h"

client::GameState::GameState() :
    window_size{500, 500},
    inventory{*this},
    last_mouse_pressed{-1, -1},
    last_hovered{nullptr},
    socket{network::Socket::SocketType::TCP},
    is_open{true} {
        socket.connect("192.168.178.34", 50140);
        if (socket.get_status() == network::Socket::SocketStatus::ERROR) {
            throw std::runtime_error("Socket connect failed, server offline?");
        }

        this->receive_thread = std::thread{this->receive_packets, this, &this->socket};

        socket.send(network::Packet{0, "ABCDEFGHIJKLMNOPQRSTUVWXYZ GIMME DA TILEMAP\n"});
}

void client::GameState::receive_packets(GameState* game_state, network::Socket* socket) {
    network::Packet packet;
    while(game_state->is_open) {
        packet = socket->receive_packet();
        if (socket->get_status() == network::Socket::SocketStatus::ERROR) {
            throw std::runtime_error("Cannot receive packet");
        }
        std::cout << (int)packet.packet_type << ": " << packet.data << std::endl;
        // handle packet
        switch (packet.packet_type) {
            case 0:
                // GIMME DA TILEMAP
                break;
            case 1:
                return;
            default:
                break;
        }
    }
}

client::GameState::~GameState() {
    this->socket.send(network::Packet{1, "CLOSE CONNECTION; BYE BYE :3\n"});
    this->is_open = false;
    this->socket.disconnect();
    // wait 1 second before disconnect?
    // this->socket.disconnect();
    this->receive_thread.join();
}

const client::TextureManager& client::GameState::get_texture_manager() {
    return this->texture_manager;
}

const sf::Vector2u& client::GameState::get_window_size() {
    return this->window_size;
}

void client::GameState::set_window_size(sf::Vector2u size) {
    this->window_size = size;
}

client::Inventory& client::GameState::get_inventory() {
    return this->inventory;
}

client::HexMap& client::GameState::get_hexmap() {
    return this->hex_map;
}

// call the resize method for all resizable objects to resize UI elements
void client::GameState::resize() {
    this->map_view.setSize(sf::Vector2f{this->window_size});
    this->map_view.setCenter(this->window_size.x / 2.0f + this->hex_map.pos.x, this->window_size.y / 2.0f + this->hex_map.pos.y);
    this->map_view.zoom(this->hex_map.zoom);
    this->inventory_view.setSize(sf::Vector2f{this->window_size});
    this->inventory_view.setCenter(this->window_size.x / 2.0f, this->window_size.y / 2.0f);
    for (Resizable* r : this->resizables) {
        r->on_resize(*this);
    }
}

void client::GameState::add_resizable_object(Resizable* r) {
    this->resizables.push_back(r);
}

void client::GameState::add_clickable_object(Clickable* c) {
    this->clickables.push_back(c);
}

void client::GameState::add_clickable_object(Clickable* c, unsigned int priority) {
    if (this->clickables.size() > priority) {
        this->clickables.insert(this->clickables.begin() + priority, c);
    } else {
        this->add_clickable_object(c);
    }
}

void client::GameState::remove_clickable_object(Clickable* c) {
    if (this->last_hovered == c) {
        this->last_hovered = nullptr;
    }
    for (std::vector<client::Clickable*>::iterator c_iter = this->clickables.begin(); c_iter != this->clickables.end(); c_iter++) {
        if (c == *c_iter) {
            this->clickables.erase(c_iter);
            return;
        }
    }
}

void client::GameState::call_mouse_press(sf::Mouse::Button button, const sf::Vector2i& pos) {
    this->last_mouse_pressed = pos;
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && cp->on_press(*this, button)) {
            break;
        }
    }
}

void client::GameState::call_mouse_release(sf::Mouse::Button button, const sf::Vector2i& pos) {
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && cp->on_release(*this, button)) {
            break;
        }
    }
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && cp->contains(this->last_mouse_pressed) && cp->on_click(*this, button)) {
            break;
        }
    }
    this->last_mouse_pressed = sf::Vector2i{-1, -1};
}

void client::GameState::call_mouse_move(const sf::Vector2i& pos) {
    for (Clickable* cp : this->clickables) {
        if (!cp->contains(pos) && this->last_hovered == cp) {
            last_hovered = nullptr;
            cp->on_exit(*this);
        }
    }
    for (Clickable* cp : this->clickables) {
        if (cp->contains(pos) && this->last_hovered == nullptr || this->last_hovered != nullptr && !this->last_hovered->contains(pos) && cp->contains(pos)) {
            last_hovered = cp;
            cp->on_enter(*this);
        }
    }
}

const client::LocalizationManager& client::GameState::get_localization_manager() {
    return this->localization_manager;
}