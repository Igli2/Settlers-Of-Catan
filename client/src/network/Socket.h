#pragma once

#include <sys/socket.h>
#include <cstdint>
#include <string>
#include <cstddef>

namespace network {
    struct Packet {
        uint8_t packet_type;
        std::string data;

        std::string to_binary() const;
    };

    class Socket {
        //currently unimplemented: get addr of host and client; set options
        public:
            enum SocketType {
                TCP = SOCK_STREAM,
                UDP = SOCK_DGRAM
            };

            enum class SocketStatus {
                ERROR = -1,
                SUCCESS
            };

            static const short MAX_BUFFER_LENGTH;

            explicit Socket(const SocketType type);
            Socket() = delete;
            Socket(const Socket& other) = delete;
            Socket(Socket&& other);
            ~Socket();

            Socket accept();
            void bind(const uint16_t port);
            void connect(const std::string& address, const uint16_t port);
            void disconnect();
            void listen(const int max_queue_length);
            void send(const std::string& to_send);
            void send(const Packet& to_send);
            Packet receive_packet();
            std::string receive(short max_bytes = Socket::MAX_BUFFER_LENGTH, const bool partial = true);

            SocketStatus get_status() const;

            Socket& operator=(const Socket& other) = delete;
            Socket& operator=(Socket&& other);
        private:
            Socket(const int socket_id);
            void delete_socket();

            int socket_id;
            SocketStatus last_status;
    };
}