library(parallel)
cls <- makeCluster(rep("localhost", 4))

?clusterApply
clusterApply(cls)

# Computing distance matrix of valuations
vals <- strsplit(as.character(train$Valuations), split=" ")
GameLengths <- train$GameLength

# break game up in to midgame and endgame
# how to divide game up
compare <- function(gameno=680, turn.start=10, turn.end=GameLengths[gameno], turn.max=turn.end+10, k=1000, show=FALSE) {
	ids <- 1:25000
	# split up by game length
	if (GameLengths[gameno] < 6) return(0)
	if (GameLengths[gameno] < ) {
		turn.start = 
		
	}
	
	
	
	if (GameLengths[gameno] < turn.end) return(0)
	if (GameLengths[gameno] < turn.start) return(0)
	if (turn.start > turn.end) return(0)
	vals_v0 <- vals[GameLengths >= turn.end & GameLengths <= turn.max]
	if (length(vals_v0) < 5) return(0)
	if (length(vals_v0) < k) k = length(vals_v0)-1
	ids <- ids[GameLengths >= turn.end & GameLengths <= turn.max]
	vals_v1 <- lapply(vals_v0, function(x) return(x[turn.start:turn.end]))
	vals_v2 <- t(matrix(as.integer(unlist(vals_v1)), nrow=turn.end-turn.start+1))
	gameno <- which(ids==gameno)
	vals_v3 <- sapply((1:nrow(vals_v2)), function(x) {
	#vals_v3 <- clusterApply(cls, (1:nrow(vals_v2)), function(x) {
		sqrt(sum((vals_v2[gameno,] - vals_v2[x,])^2, na.rm=TRUE))
	})
	# names(vals_v3) <- (1:nrow(vals_v2))[-gameno]
	# names(vals_v3) <- (ids)[-gameno]
	names(vals_v3) <- (ids)
	vals_v4 <- as.data.frame(sort(vals_v3))

	if (show == TRUE) {
		plot(unlist(vals[as.integer(rownames(vals_v4)[1])])[turn.start:turn.end], type='l', ylim=c(min(vals_v2[gameno,]),max(vals_v2[gameno,])), col=rgb(1/k,0,0))
		Sys.sleep(.5)
		for (i in 2:k) {
			#lines(unlist(vals_v0[as.integer(rownames(vals_v4)[i])])[turn.start:turn.end], col=rgb(i/k,0,0))
			lines(unlist(vals[as.integer(rownames(vals_v4)[i])])[turn.start:turn.end], col=rgb(i/k,0,0))
			Sys.sleep(.5)
		}
	}
	return(head(vals_v4, k))
}

cmp <- compare(gameno=1,k=100,show=FALSE)
cmp.df <- train[rownames(cmp),c(3,4,5,6,9)]
correctAnswer <- cmp.df[1,]
closestGames  <- cmp.df[-1,]
closestGames$cummean <- cumsum(closestGames$meanElo) / (1:nrow(closestGames))
closestGames$cumdiff <- cumsum(closestGames$EloDiff) / (1:nrow(closestGames))
closestGames$EstimatedWhite <- closestGames$cummean + closestGames$cumdiff/2
closestGames$EstimatedBlack <- closestGames$cummean - closestGames$cumdiff/2
closestGames$mae <- (abs(correctAnswer$WhiteElo[1]-closestGames$EstimatedWhite) + abs(correctAnswer$WhiteElo[1]-closestGames$EstimatedBlack))/2
mae <- closestGames$mae
#mae <- data.frame(ks=1:nrow(closestGames), mae=closestGames$mae)
#mae <- mae[order(mae$mae),]
#bestmae <- mae[1,2]
#bestk <- mae[1,1]
#bestks <- mae[1:5,1]
#bestmae;bestk


# use apply from parallel package

start <- Sys.time()
maes <- lapply(1:1000, function(x) {
	cmp <- compare(gameno=x,k=300,show=FALSE)
	if (is.integer(cmp) | is.numeric(cmp)) {
		#bestmae = NA
		#bestk = NA
		#bestks = rep(NA,5)
		#return(c(bestmae, bestk, list(bestks)))
		#return(c(bestmae, bestk))
		return(data)
	}
	cmp.df <- train[rownames(cmp),c(3,4,6,9)]
	correctAnswer <- cmp.df[1,]
	closestGames  <- cmp.df[-1,]
	closestGames$cummean <- cumsum(closestGames$meanElo) / (1:nrow(closestGames))
	closestGames$cumdiff <- cumsum(closestGames$EloDiff) / (1:nrow(closestGames))
	closestGames$EstimatedWhite <- closestGames$cummean + closestGames$cumdiff/2
	closestGames$EstimatedBlack <- closestGames$cummean - closestGames$cumdiff/2
	closestGames$mae <- (abs(correctAnswer$WhiteElo[1]-closestGames$EstimatedWhite) + abs(correctAnswer$WhiteElo[1]-closestGames$EstimatedBlack))/2
	mae <- closestGames$mae
	#mae <- data.frame(ks=1:nrow(closestGames), mae=closestGames$mae)
	#mae <- mae[order(mae$mae),]
	#bestmae <- mae[1,2]
	#bestk <- mae[1,1]
	#bestks <- mae[1:5,1]
	#return(c(bestmae, bestk, list(bestks)))
	#print(x)
	#return(c(bestmae, bestk))
	return(mae)
})

finish <- Sys.time()
totalTime <- finish-start
secPerGame <- totalTime / 1000
# expected 62 minutes

save(maes, file='~/Documents/Kaggle/ELO/maes.Rda')
save(maes.big.mat, file='~/Documents/Kaggle/ELO/maes.big.mat.Rda')
save(avgmaes, file='~/Documents/Kaggle/ELO/avgmaes.Rda')
load(file='~/Documents/Kaggle/ELO/maes.Rda')
load(file='~/Documents/Kaggle/ELO/maes.big.mat.Rda')
load(file='~/Documents/Kaggle/ELO/avgmaes.Rda')

lens <- sapply(maes,length)
maes.big <- maes[lens==499]
maes.big.mat <- matrix(unlist(maes.big), nrow=499)
avgmaes <- apply(maes.big.mat, 1, mean, na.rm=TRUE)

## Break out by result

# find games that have 499 comparisons
lens <- sapply(maes,length)
complete.game.nums <- which(lens==499)
maes.complete <- maes[complete.game.nums]
train.complete <- train[complete.game.nums, ]
maes.complete.whitewins <- maes.complete[train.complete$Result == "1-0"]
maes.complete.blackwins <- maes.complete[train.complete$Result == "0-1"]
maes.complete.tie <- maes.complete[train.complete$Result == "1/2-1/2"]

maes.complete.whitewins.mat <- matrix(unlist(maes.complete.whitewins), nrow=499)
maes.complete.blackwins.mat <- matrix(unlist(maes.complete.blackwins), nrow=499)
maes.complete.tie.mat <- matrix(unlist(maes.complete.tie), nrow=499)
avgmaes.whitewins <- apply(maes.complete.whitewins.mat, 1, mean, na.rm=TRUE)
avgmaes.blackwins <- apply(maes.complete.blackwins.mat, 1, mean, na.rm=TRUE)
avgmaes.ties <- apply(maes.complete.tie.mat, 1, mean, na.rm=TRUE)

# if there are no good comparisons, or if gamelength is too small, just give median?
# what is the best k? what is the best turn.max,start,end
# if the game is too long, comparing the whole game will only match to like one other game...