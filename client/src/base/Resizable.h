#pragma once

namespace client {
    class GameState;

    class Resizable {
        public:
            Resizable() = default; // register manually
            Resizable(GameState& game_state); // registers automatically
            ~Resizable() = default;
            virtual void on_resize(GameState& game_state) = 0;
    };
}