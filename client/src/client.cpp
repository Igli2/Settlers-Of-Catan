#include <iostream>
#include <string>
#include <cstddef>

#include "base/GameState.h"
#include "base/GameWindow.h"

#include "network/Socket.h"

#include "launcher/LauncherWindow.h"

int main() {
    launcher::LauncherWindow window;
    window.render_loop();

    return 0;
}