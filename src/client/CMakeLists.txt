find_package(SFML COMPONENTS window graphics system REQUIRED)

set(CLIENT_SOURCE_FILES
    
)

add_executable(client client.cpp ${CLIENT_SOURCE_FILES})
target_link_libraries(client sfml-window sfml-graphics sfml-system sfml-network)
target_include_directories(client PRIVATE ${SRC_INCLUDE_FOLDER})