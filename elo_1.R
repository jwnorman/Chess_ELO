setwd("C:/Kaggle/ELO")
raw <- readLines("data_uci.pgn")
fish <- read.csv("stockfish.csv", 
			colClasses = c("integer", "character"))

# Extract Valuations
moveScores <- fish$MoveScores
moveScores.split <- strsplit(moveScores, " ")

# Find the lines of each variable
moveLineNums <- grep("^[a-z]",raw)
eventLineNums <- grep("^\\[Event", raw)
resultLineNums <- grep("^\\[Result", raw)
whiteELOLineNums <- grep("^\\[WhiteElo", raw)
blackELOLineNums <- grep("^\\[BlackElo", raw)

# Extract the information only
events <- gsub('\\[Event \\"([^\\"]+)\\"\\]', "\\1", raw)
results <- gsub('\\[Result \\"([^\\"]+)\\"\\]', "\\1", events)
whiteELOs <- gsub('\\[WhiteElo \\"([^\\"]+)\\"\\]', "\\1", results)
blackELOs <- gsub('\\[BlackElo \\"([^\\"]+)\\"\\]', "\\1", whiteELOs)

# Put in variable form
events <- events[eventLineNums]
results <- results[resultLineNums]
whiteELOs <- whiteELOs[whiteELOLineNums]
blackELOs <- blackELOs[blackELOLineNums]

# Find number of moves for each game
gameLengths = integer(length(moveLineNums))
for (gameNum in 1:length(moveLineNums)) {
	gameLengths[gameNum] = 
		length(strsplit(raw[moveLineNums[gameNum]], " ")[[1]])-1
}
gameLengthsCumulative <- cumsum(gameLengths)
where2start <- c(0, gameLengthsCumulative)

# Initialize dataset
size = sum(gameLengths)
data <- data.frame(Event=integer(size), WhoseMove=integer(size), Move=integer(size), Valuation=integer(size), Result=integer(size), ELO=integer(size))

# Iterate through each game
for (gamenum in 1:length(events)) {
for (gamenum in 1:100) {
	eloWhite = whiteELOs[gamenum]
	eloBlack = blackELOs[gamenum]
	length = gameLengths[gamenum]
	start <- where2start[gamenum]+1
	data[start:(start+length-1), 1] <- rep(events[gamenum], length)
	data[start:(start+length-1), 2] <- rep(c("White", "Black"), len=length)
	data[start:(start+length-1), 3] <- moveScores.split[gamenum][[1]]
	data[start:(start+length-1), 4] <- rep(results[gamenum], length)
	data[start:(start+length-1), 5] <- rep(c(eloWhite, eloBlack), len=length)
}
head(data,284)
data$Result <- ifelse(data$Result == "1-0", "1", data$Result)
data$Result <- ifelse(data$Result == "0-1", "0", data$Result)
data$Result <- ifelse(data$Result == "1/2-1/2", ".5", data$Result)

gamenum=2
gameLengthsCumulative[5]
head(data,10)
head(data,8222)