#include "LocalizationManager.h"

#include <stdexcept>

client::LocalizationManager::LocalizationManager() {
    this->load_locale(RESOURCE_FOLDER"/locales/en_US.txt");
}

void client::LocalizationManager::load_locale(const std::string& file) {
    this->translations.clear(); // for changing translations runtime

    std::ifstream input{file};
    std::string line;

    while(getline(input, line)) {
        const size_t delimiter_pos = line.find(": ");
        if(delimiter_pos == std::string::npos) {
            throw std::runtime_error("Expected delimiter ':' but found none!");
        }
        
        const std::string key = line.substr(0, delimiter_pos);
        const std::string value = line.substr(delimiter_pos + 2, line.length());
        this->translations[key] = value;
    }
}

const std::string& client::LocalizationManager::get_translation(const std::string& key) const {
    if (this->translations.find(key) != this->translations.end()) {
        return this->translations.at(key);
    }
    return this->empty;
}