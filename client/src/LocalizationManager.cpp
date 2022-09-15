#include "LocalizationManager.h"

client::LocalizationManager::LocalizationManager() {
    this->load_locale(RESOURCE_FOLDER"/en_US.txt");
}

void client::LocalizationManager::load_locale(std::string file) {
    this->translations.clear(); // for changing translations runtime

    std::ifstream fs{file};
    std::string input, key, value;
    int delimiter_pos;

    while(getline(fs, input)) {
        delimiter_pos = input.find(": ");
        key = input.substr(0, delimiter_pos);
        value = input.substr(delimiter_pos + 2, input.length());
        this->translations[key] = value;
    }
}

const std::string& client::LocalizationManager::get_translation(std::string key) const {
    if (this->translations.find(key) != this->translations.end()) {
        return this->translations.at(key);
    }
    return this->empty;
}