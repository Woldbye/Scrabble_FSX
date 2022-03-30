# Scrabble_FSX
Scrabble Engine implemented as part of the 4th semester for Software Development. 

The bot follows the basic tournament [rules](https://www.scrabbleplayers.org/rules/player-rules-20170120.pdf) with a few modifications:
1. Infinite board sizes
2. Boards with holes
3. "*Pieces that are placed on the board are represented as sets of characters and point values. The wildcard piece can then be seen as the set of all characters worth zero points, whereas a letter piece is a
singleton set with that letter and its point value.*"
4. Tiles are functions that operate on the words placed above them

## Overview
Client and server published to *NuGet*-repository. Dependencies are handled automaticly by *NuGet*-package manager.

* ScrabbleBot: *Contains the bot source code*
* ScrabbleTemplate: *Executable - load bot and play against other bots.*
  
### Dependencies
* ScrabbleServer: *Allows you to hook up an arbitrary amount of clients to play against each other, or several instances of the same client to play against itself.*
* ScrabbleUtil: *a utility library that contains the minimum required datatypes, a few boards to play on, and primitives for server communication, and a means to set up a common dictionary among several bots*
* Oxyphenbutazone: *Jespers basic scrabble bot - It follows a greedy approach and always plays the highest scoring move it can find.*

### Points for implementation pieces
1. [+2] : Play against yourself on infinite board (Mandatory)
2. [+1] : Play against others                                
3. [+1] : Parse and finish game on __all__ boards using DSL  
4. [+1] : Parallelirize the solution                         
5. [+1] : Respect timeout flag                               

### Given pieces:
* Communication protocol
* Back-end communication with server.
* DSL that defines the scrabble board
* Rules of the game
  
### Missing pieces:
* *Next_Move* - What is the best possible move.
* Maintain scrabble board in memory.
* Maintain internal state that matches server state.
  
### Server
* Accepts a list of clients that should play with each other
* Communicates via a protocol
  
### Data-structures for Dictionary
- Gaddag or Trie?

## Delivery
* Hide implementation such that only .fsi files hide everything except the one function interacting with the server.