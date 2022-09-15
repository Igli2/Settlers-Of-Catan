#include "TextureManager.h"

client::TextureManager::TextureManager() {
    // create the missing texture and change the color
    this->missing_texture.create(16, 16);
    sf::Image missing_texture_image = this->missing_texture.copyToImage();
    for (int x = 0; x < missing_texture_image.getSize().x; x++) {
        for (int y = 0; y < missing_texture_image.getSize().x; y++) {
            missing_texture_image.setPixel(x, y, sf::Color::Magenta);
        }
    }
    this->missing_texture.loadFromImage(missing_texture_image);

    this->load_directory(RESOURCE_FOLDER"/textures");

    if (!this->font.loadFromFile(RESOURCE_FOLDER"/fonts/OpenSans.ttf")) {
        std::cout << "Cannot load font" << std::endl;
    }
}

const sf::Texture& client::TextureManager::get_texture(const std::string& name) const {
    if (this->has_texture(name)) {
        return this->texture_map.at(name);
    } else {
        return this->missing_texture;
    }
}

void client::TextureManager::load_texture(const std::string& name, const std::string& filename) {
    if(this->has_texture(name)) {
        std::cout << "Texture already exists: " << name << std::endl;
        return;
    }

    sf::Texture loaded;
    if (!loaded.loadFromFile(filename)) {
        std::cout << "Cannot load texture from file: " << filename << std::endl;
        return; 
    }

    this->texture_map[name] = loaded;
}

void client::TextureManager::load_directory(const std::string& directory) {
    for(const auto& entry : std::filesystem::directory_iterator(directory)) {
        if(!entry.is_directory()) {
            if(entry.path().extension() == ".png") {
                this->load_texture(entry.path().stem().string(), entry.path().string());
            }
        }
    }
}

bool client::TextureManager::has_texture(const std::string& name) const {
    return this->texture_map.find(name) != this->texture_map.end();
}

const sf::Font& client::TextureManager::get_font() const {
    return this->font;
}