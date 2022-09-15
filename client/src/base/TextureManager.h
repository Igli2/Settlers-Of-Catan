#pragma once

#include <SFML/Graphics.hpp>
#include <map>
#include <string>
#include <iostream>
#include <filesystem>

namespace client {
    class TextureManager {
        private:
            std::map<std::string, sf::Texture> texture_map;
            sf::Texture missing_texture;
            sf::Font font;

            bool has_texture(const std::string& name) const;
        public:
            TextureManager();
            const sf::Texture& get_texture(const std::string& name) const;
            void load_texture(const std::string& name, const std::string& filename);
            void load_directory(const std::string& directory);
            const sf::Font& get_font() const;
    };
}