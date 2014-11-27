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

### Put in variable form
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
data <- as.data.frame(cbind(events4df, wb, moveScores.unlisted, results4df, whiteELOs4df, blackELOs4df))
names(data) <- c("Event", "WhoseMove", "Centipawns", "Result", "WhiteElo", "BlackElo")

save(data, file='~/Desktop/Kaggle/ELO/ELO_df_1.Rda')