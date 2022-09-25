#include "base/GameState.h"
#include "base/GameWindow.h"

int main() {
    client::GameState game_state;
    client::GameWindow game_window{game_state};
    game_state.get_hexmap().set_game_window(&game_window);
    game_window.game_loop(game_state);

    return 0;
}