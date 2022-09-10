#include "GameState.h"
#include "GameWindow.h"

int main() {
    client::GameState game_state;
    client::GameWindow game_window{game_state};
    game_window.game_loop(game_state);

    return 0;
}