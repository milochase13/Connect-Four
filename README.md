# Connect-Four
Written for CS 151

Interactive connect four game with options to play human or bot. Implemented in Typed Racket. 

To run a game, within program call the "play" function with parameters (Controller, Controller, and formating radius).
Each Controller can be either a "Human" or a "Bot".
If calling a Human Controller, then must only specify a name.
If calling a Bot Controller, then must specify a Strategy (included in the project is a minimax strategy called 'make-minimax-strategy'),
a heuristic (included heuristic is 'count-winning-positions'), and a PLY value. Note that for most machines a PLY greater than 4 will be very slow. 
