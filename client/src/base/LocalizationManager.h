#pragma once

#include <map>
#include <string>
#include <fstream>

namespace client {
    class LocalizationManager {
        private:
            std::map<std::string, std::string> translations;
            void load_locale(const std::string& file);
            std::string empty = "";
        public:
            LocalizationManager();
            const std::string& get_translation(const std::string& key) const;
    };
}