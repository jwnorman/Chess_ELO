# Preprocess the data

# PC
setwd("C:/Kaggle/ELO")
raw <- readLines("data_uci.pgn")
fish <- read.csv("stockfish.csv", 
			colClasses = c("integer", "character"))

# Mac
raw <- readLines('~/Desktop/Kaggle/ELO/data_uci.pgn')fish <- read.csv('~/Desktop/Kaggle/ELO/stockfish.csv', 
			colClasses = c("integer", "character"))

# Extract Valuations
moveScores <- fish$MoveScores
moveScores.unlisted <- unlist(strsplit(moveScores, " "))

# Extract Information from PGN File
### Find the lines of each variable
moveLineNums <- grep("^[a-z]",raw)
eventLineNums <- grep("^\\[Event", raw)
resultLineNums <- grep("^\\[Result", raw)
whiteELOLineNums <- grep("^\\[WhiteElo", raw)
blackELOLineNums <- grep("^\\[BlackElo", raw)

### Extract the information only
events <- gsub('\\[Event \\"([^\\"]+)\\"\\]', "\\1", raw)
results <- gsub('\\[Result \\"([^\\"]+)\\"\\]', "\\1", raw)
whiteELOs <- gsub('\\[WhiteElo \\"([^\\"]+)\\"\\]', "\\1", raw)
blackELOs <- gsub('\\[BlackElo \\"([^\\"]+)\\"\\]', "\\1", raw)

### Find number of moves for each game
gameLengths <- unlist(lapply(raw[moveLineNums], function(x) { 
		return(length(strsplit(x, " ")[[1]])-1) # minus 1 because the last string is the result of the game, not a move
	}))

### Put in play by play form
moves <- raw[moveLineNums]
events <- events[eventLineNums]
events4df <- rep(events, times=gameLengths)
results <- results[resultLineNums]
results4df <- rep(results, times=gameLengths)
whiteELOs <- whiteELOs[whiteELOLineNums]
whiteELOs <- c(whiteELOs, rep(0, 25000)) # rep(0, 25000) for the test data set, which has the ELOs removed
whiteELOs4df <- rep(whiteELOs, times=gameLengths)
blackELOs <- blackELOs[blackELOLineNums]
blackELOs <- c(blackELOs, rep(0, 25000))
blackELOs4df <- rep(blackELOs, times=gameLengths)

# White/Black (Whose turn)
wb <- unlist(lapply(1:length(events), function(x) {
	wbitr <- rep(c("White", "Black"), len=gameLengths[x])
}))

# Create data frame for play by play data
plays <- data.frame(Event=as.integer(events4df), 
				    WhoseMove = as.character(wb),
				    Centipawns = as.integer(moveScores.unlisted),
				    Result = as.character(results4df),
				    WhiteElo = as.integer(whiteELOs4df),
				    BlackElo = as.integer(blackELOs4df))
				   
# Create data frame for game by game data
games <- data.frame(Event=1:length(events),
					Result=results,
					WhiteElo = as.integer(whiteELOs),
					BlackElo = as.integer(blackELOs),
					GameLength = as.integer(gameLengths),
					EloDiff = as.integer(whiteELOs) - as.integer(blackELOs),
					Moves = as.character(moves),
					Valuations = as.character(moveScores))
					

save(plays, file='~/Desktop/Kaggle/ELO/playbyplay.Rda')
  # Event WhoseMove Centipawns  Result WhiteElo BlackElo
# 1     1     White         18 1/2-1/2     2354     2411
# 2     1     Black         17 1/2-1/2     2354     2411
# 3     1     White         12 1/2-1/2     2354     2411
# 4     1     Black          8 1/2-1/2     2354     2411
# 5     1     White         -5 1/2-1/2     2354     2411
# 6     1     Black         12 1/2-1/2     2354     2411

save(games, file='~/Desktop/Kaggle/ELO/gamebygame.Rda')
  # Event  Result WhiteElo BlackElo GameLength EloDiff Moves       		Valuations
# 1     1 1/2-1/2     2354     2411         38 	   -57 g1f3 g8f6...   	18 17...
# 2     2 1/2-1/2     2523     2460         13 		63 e2e4 e7e5...   	26 44...
# 3     3     0-1     1915     1999        106 	   -84 e2e4 d7d5...   	26 51...
# 4     4     1-0     2446     2191         77 	   255 g1f3 b8d7...   	21 5 ...
# 5     5     1-0     2168     2075         49 		93 e2e4 c7c5...   	26 64...
# 6     6 1/2-1/2     2437     2254         58 	   183 g1f3 d7d5...   	18 29...