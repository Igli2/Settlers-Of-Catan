#include <iostream>
#include <string>
#include <cstddef>

#include "base/GameState.h"
#include "base/GameWindow.h"

#include "network/Socket.h"

#include "launcher/LauncherWindow.h"

int main() {
    /* std::string address_info;
    std::cin >> address_info;

    const size_t port_seperator = address_info.find(':');
    if(port_seperator == std::string::npos) {
        std::cerr << "Invalid address format!";
        return -1;  
    }

    network::Socket server_connection(network::Socket::TCP);

    server_connection.connect(address_info.substr(0, port_seperator), std::stoull(address_info.substr(port_seperator + 1)));
    if (server_connection.get_status() == network::Socket::SocketStatus::ERROR) {
        throw std::runtime_error("Socket connect failed, server offline?");
    }

    client::GameState game_state(server_connection);
    client::GameWindow game_window{game_state};
    game_state.get_hexmap().set_game_window(&game_window);
    game_window.game_loop(game_state); */

    launcher::LauncherWindow window;
    window.loop();

    return 0;
}