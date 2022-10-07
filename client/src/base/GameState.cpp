#include "GameState.h"

constexpr size_t DEFAULT_WINDOW_WIDTH = 500;
constexpr size_t DEFAULT_WINDOW_HEIGHT = 500;

client::GameState::GameState(network::Socket& socket) :
    window_size{DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT},
    mouse_handler{*this},
    inventory{*this},
    is_open{true},
    hex_map{*this},
    socket{socket} {
        this->receive_thread = std::thread{&GameState::receive_packets, this};

        socket.send(network::Packet{2, "GET TILEMAP"});
}

void client::GameState::receive_packets() {
    network::Packet packet;
    while(this->is_open) {
        packet = this->socket.receive_packet();
        if (this->socket.get_status() == network::Socket::SocketStatus::ERROR) {
            std::cout << "Error in packet, sync problems may occur" << std::endl;
            continue;
        }
        // handle packet
        switch (packet.packet_type) {
            case network::PacketType::DISCONNECT:
                this->is_open = false;
                break;
            case network::PacketType::TILE_DATA:
                this->get_hexmap().add_tile(packet.data);
                break;
            default:
                std::cout << (network::PacketType)packet.packet_type << ": " << packet.data << std::endl;
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
    const sf::Vector2f window_center = sf::Vector2f((float)this->window_size.x / 2.0f, (float)this->window_size.y / 2.0f);

    this->map_view.setSize(sf::Vector2f{this->window_size});
    this->map_view.setCenter(window_center.x + this->hex_map.pos.x, window_center.y + this->hex_map.pos.y);
    this->map_view.zoom(this->hex_map.zoom);
    this->inventory_view.setSize(sf::Vector2f{this->window_size});
    this->inventory_view.setCenter(window_center);
    for (Resizable* to_resize : this->resizables) {
        to_resize->on_resize(*this);
    }
}

void client::GameState::add_resizable_object(Resizable* to_add) {
    this->resizables.push_back(to_add);
}

const client::LocalizationManager& client::GameState::get_localization_manager() {
    return this->localization_manager;
}

client::MouseHandler& client::GameState::get_mouse_handler() {
    return this->mouse_handler;
}

network::Socket& client::GameState::get_socket() {
    return this->socket;
}