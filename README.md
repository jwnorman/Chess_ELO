Chess Elo
=========

From a Kaggle competition dataset, I predict the ELO (chess rating system) of a player using data from only one game.

See https://www.kaggle.com/c/finding-elo for the data.

- There are 25000 chess games for training (the ELO rating of each player is given) and another 25000 chess games for testing (ELO not given).
- The file that contains these 50000 games is in portable game notation (pgn) format.
- The competition administrators ran these chess games through the Stockfish chess engine which evaluates the board position of each turn to determine whether White or Black is winning. The unit given is in centipawns (cp) where 100cp = 1 pawn with White advantage and -100cp = 1 pawn with Black advantage.
- I plan to use the centipawns, the distribution of the centipawns throughout the game, the length of game, and other attributes to predict the ELO of the players in the game.

- Note: Since the competition is over, I have organized the files into more general directories (I should've done this during the competition). Because of this, most of the file paths within the .R files are slightly off now. Also, all the 'loose' files (not within directories) are just copies of or renamed copies of the files that are in the directories
