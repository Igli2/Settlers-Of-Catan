cmake_minimum_required(VERSION 3.10)
set(CMAKE_CXX_STANDARD 17)

project(settlers_of_catan)

add_compile_definitions(RESOURCE_FOLDER="${CMAKE_SOURCE_DIR}/res")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})

find_package(SFML COMPONENTS window graphics system network REQUIRED)

set(CLIENT_SOURCE_FILES
    src/base/GameState.cpp
    src/base/GameWindow.cpp
    src/base/TextureManager.cpp
    src/base/LocalizationManager.cpp
    src/base/Resizable.cpp
    src/base/Clickable.cpp
    src/base/Overlay.cpp
    src/base/OverlayAction.cpp
    src/base/KeyboardHandler.cpp
    src/base/MouseHandler.cpp
    src/base/ClickableText.cpp

    src/inventory/Inventory.cpp
    src/inventory/DevelopmentCard.cpp
    src/inventory/Resource.cpp
    src/inventory/VictoryPoint.cpp
    src/inventory/Trade.cpp
    src/inventory/TradeOfferOverlay.cpp
    src/inventory/BuildButton.cpp

    src/launcher/LauncherWindow.cpp

    src/map/HexMap.cpp

    src/network/Socket.cpp
)

add_executable(client src/client.cpp ${CLIENT_SOURCE_FILES})
target_include_directories(client PRIVATE src)
target_link_libraries(client sfml-window sfml-graphics sfml-system sfml-network)