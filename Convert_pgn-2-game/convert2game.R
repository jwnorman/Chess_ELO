# convert data_uci.pgn to .game file to explore in Mac Chess App

uci <- readLines("~/Documents/Kaggle/ELO/data_uci.pgn")
gameTemplate <- readLines("~/Documents/Kaggle/ELO/Converted_Games/mac_template.game")
moveLineNums <- grep("^[a-z]",uci)
moves <- uci[moveLineNums]

lapply(1:5, function(x) {
	moves.aschar <- unlist(strsplit(moves[[x]], split=" "))
	result <- moves.aschar[length(moves.aschar)]
	moves.aschar.woResult <- moves.aschar[1:(length(moves.aschar)-1)]
	gameTemp <- gameTemplate
	gameTemp[38] <- paste(gameTemp[38], moves.aschar.woResult[1], sep="")
	endOfTemplate <- gameTemplate[39:55]
	endOfTemplate[5] <- paste("\t<string>", result, "</string>", sep="")
	gameTemp <- gameTemp[1:38]
	gameTempNew <- c(gameTemp, moves.aschar.woResult[-1])
	gameTempFinal <- c(gameTempNew, endOfTemplate)
	write(gameTempFinal, paste("~/Documents/Kaggle/ELO/Converted_Games/test", x, ".game", sep=""))
})