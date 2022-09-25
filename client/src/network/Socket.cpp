#include "Socket.h"

#include <stdexcept>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <utility>
#include <endian.h>
#include <algorithm>

//static

const short network::Socket::MAX_BUFFER_LENGTH = 4096;

//public

std::string network::Packet::to_bytes() const {
    const uint64_t packet_length = htobe64((uint64_t)this->data.size());
    std::string binary_packet;

    binary_packet.insert(0, (const char*)&this->packet_type, sizeof(uint8_t));
    binary_packet.insert(sizeof(uint8_t), (const char*)&packet_length, sizeof(uint64_t));

    binary_packet += this->data;

    return binary_packet;
}

//public

network::Socket::Socket(const SocketType type) : socket_id{-1}, last_status(SocketStatus::SUCCESS), type(type) {
}

network::Socket::Socket(Socket&& other) : socket_id(-1), last_status(SocketStatus::SUCCESS) {
    *this = std::move(other);
}

network::Socket::~Socket() {
    this->disconnect();
}

network::Socket network::Socket::accept() {
    const int connection_id = ::accept(this->socket_id, nullptr, 0);
    if(connection_id < 0) {
        this->last_status = SocketStatus::ERROR;
        throw std::runtime_error("Accept returned invalid socket id!");
    }

    this->last_status = SocketStatus::SUCCESS;

    return std::move(Socket(connection_id));
}

void network::Socket::bind(const uint16_t port) {
    this->create_new_socket();

    const sockaddr_in target{AF_INET, htons(port), INADDR_ANY, 0};

    if(::bind(this->socket_id, (const sockaddr*)&target, sizeof(sockaddr_in)) < 0) {
        this->last_status = SocketStatus::ERROR;
        return;
    }

    this->last_status = SocketStatus::SUCCESS;
}

void network::Socket::connect(const std::string& address, const uint16_t port) {
    this->create_new_socket();

    sockaddr_in target{AF_INET, htons(port), INADDR_ANY, 0};
    if(!inet_aton(address.c_str(), &target.sin_addr)) {
        throw std::runtime_error("Tried to connect to invalid address!");
    }

    if(::connect(this->socket_id, (const sockaddr*)&target, sizeof(sockaddr_in)) < 0) {
        this->last_status = SocketStatus::ERROR;
        return;
    }

    this->last_status = SocketStatus::SUCCESS;
}

void network::Socket::disconnect() {
    if(this->socket_id < 0) return;
    
    if(shutdown(this->socket_id, SHUT_RDWR) < 0) {
        this->last_status = SocketStatus::ERROR;
        this->socket_id = -1;
        return;
    }

    if(close(this->socket_id) < 0) {
        this->last_status = SocketStatus::ERROR;
        this->socket_id = -1;
        return;
    }

    this->socket_id = -1;

    this->last_status = SocketStatus::SUCCESS;
}

void network::Socket::listen(const int max_queue_length) {
    if(::listen(this->socket_id, max_queue_length) < 0) {
        this->last_status = SocketStatus::ERROR;
        return;
    }

    this->last_status = SocketStatus::SUCCESS;
}

void network::Socket::send(const std::string& to_send) {
    if(::send(this->socket_id, to_send.c_str(), to_send.size(), 0) < 0) {
        this->last_status = SocketStatus::ERROR;
        return;
    }

    this->last_status = SocketStatus::SUCCESS;
}

void network::Socket::send(const Packet& to_send) {
    this->send(to_send.to_bytes());
}

network::Packet network::Socket::receive_packet() {
    Packet received;

    received.packet_type = *(uint8_t*)this->receive(sizeof(uint8_t), false).c_str();
    if(this->get_status() == SocketStatus::ERROR) return {};

    uint64_t packet_length = be64toh(*(uint64_t*)this->receive(sizeof(uint64_t), false).c_str());
    if(this->get_status() == SocketStatus::ERROR) return {};
    
    received.data = std::move(this->receive(packet_length, false));
    if(this->get_status() == SocketStatus::ERROR) return {};

    return received;
}


std::string network::Socket::receive(short max_bytes, const bool partial) {
    max_bytes = std::min(max_bytes, Socket::MAX_BUFFER_LENGTH);
    std::string msg(max_bytes, '\0');    

    const ssize_t received_count = ::recv(this->socket_id, &msg[0], max_bytes, (partial) ? 0 : MSG_WAITALL);
    
    if(received_count < 0) {
        this->last_status = SocketStatus::ERROR;
        return "";
    }

    msg.resize(received_count);
    this->last_status = SocketStatus::SUCCESS;

    return std::move(msg);
}

network::Socket::SocketStatus network::Socket::get_status() const {
    return this->last_status;
}

network::Socket& network::Socket::operator=(Socket&& other) {
    this->disconnect();

    this->socket_id = other.socket_id;
    this->type = other.type;
    this->last_status = other.last_status;
    other.socket_id = -1;

    return *this;
}

//private

network::Socket::Socket(const int socket_id) : socket_id(socket_id) {
}

void network::Socket::create_new_socket() {
    this->disconnect();

    if((this->socket_id = socket(AF_INET, this->type, 0)) < 0) {
        throw std::runtime_error("Creation of socket failed!"); 
    }
}