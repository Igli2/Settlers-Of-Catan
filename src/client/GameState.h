#pragma once

#include <SFML/Graphics.hpp>

#include "TextureManager.h"

namespace client {
    class GameState {
        private:
            TextureManager texture_manager;
        public:
            GameState();
            const TextureManager& get_texture_manager();
    };
}