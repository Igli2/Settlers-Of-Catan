cmake_minimum_required(VERSION 3.10)
set(CMAKE_CXX_STANDARD 17)

project(settlers_of_catan)

add_compile_definitions(RESOURCE_FOLDER="${CMAKE_SOURCE_DIR}/res")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})

find_package(SFML COMPONENTS window graphics system network REQUIRED)

set(CLIENT_SOURCE_FILES
    src/GameState.cpp
    src/GameWindow.cpp
    src/TextureManager.cpp
    src/Inventory.cpp
    src/Resource.cpp
    src/Resizable.cpp
    src/DevelopmentCard.cpp
    src/Clickable.cpp
    src/LocalizationManager.cpp
)

add_executable(client src/client.cpp ${CLIENT_SOURCE_FILES})
target_link_libraries(client sfml-window sfml-graphics sfml-system sfml-network)