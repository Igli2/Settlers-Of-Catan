find_package(SFML COMPONENTS network REQUIRED)

set(SERVER_SOURCE_FILES

)

add_executable(server server.cpp ${SERVER_SOURCE_FILES})
target_link_libraries(server sfml-network)
target_include_directories(server PRIVATE ${SRC_INCLUDE_FOLDER})