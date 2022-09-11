# Settlers-Of-Catan
Based on the card board game, as online multiplayer.  
  

# Build
To build the client executable for the project, run the following commands:  

`cd client`  
`mkdir build`  
`cd build`  
`cmake ..`  
`make -j4`  

Run with:  

`./client`  

# Notes
class Inventory
    stores resources, remaining figures (buttons, display cost on hover), development cards (buttons), victory points, trade button

class Resource: public sf::Sprite
    uint amount
    amount getter + setter
    text attribute
    text getter

resizable
    gamestate saves pointer to all
    resize event