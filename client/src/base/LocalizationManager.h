#pragma once

#include <map>
#include <string>
#include <fstream>

namespace client {
    class LocalizationManager {
        private:
            std::map<std::string, std::string> translations;
            void load_locale(std::string file);
            std::string empty = "";
        public:
            LocalizationManager();
            const std::string& get_translation(std::string key) const;
    };
}