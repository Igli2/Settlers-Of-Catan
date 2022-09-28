#include "GameState.h"

client::GameState::GameState(network::Socket& socket) :
    window_size{500, 500},
    mouse_handler{*this},
    inventory{*this},
    is_open{true},
    hex_map{*this},
    socket{socket} {
        this->receive_thread = std::thread{GameState::receive_packets, this, &this->socket};

        socket.send(network::Packet{2, "GET TILEMAP"});
}

void client::GameState::receive_packets(GameState* game_state, network::Socket* socket) {
    network::Packet packet;
    while(game_state->is_open) {
        packet = socket->receive_packet();
        if (socket->get_status() == network::Socket::SocketStatus::ERROR) {
            std::cout << "Error in packet, sync problems may occur" << std::endl;
            continue;
        }
        std::cout << (network::PacketType)packet.packet_type << ": " << packet.data << std::endl;
        // handle packet
        switch (packet.packet_type) {
            case network::PacketType::DISCONNECT:
                game_state->is_open = false;
                break;
            case network::PacketType::TILE_DATA:
                game_state->get_hexmap().add_tile(packet.data);
                break;
            default:
                break;
        }
    }
}

client::GameState::~GameState() {
    this->socket.send(network::Packet{0, "CLOSE CONNECTION; BYE BYE :3"});
    this->is_open = false;
    this->socket.disconnect();
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

const client::LocalizationManager& client::GameState::get_localization_manager() {
    return this->localization_manager;
}

client::MouseHandler& client::GameState::get_mouse_handler() {
    return this->mouse_handler;
}