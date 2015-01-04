library(parallel)
library(caTools) # for runmean
cls <- makeCluster(rep("localhost", 2))

# Computing distance matrix of valuations
val <- strsplit(as.character(train$Valuations), split=" ")
vals <- lapply(val, function(x) as.integer(x))
vals <- lapply(vals, function(x) {
	nums <- unlist(x)
	nums2 <- ifelse(abs(nums)>300, (nums/abs(nums))*(300+sqrt(abs(abs(nums)-300))), nums)
	runmean(nums2, k=30, alg="C", endrule="trim")
})

GameLengths <- sapply(vals, length)

head(GameLengths)

# break game up in to midgame and endgame
# how to divide game up
compare <- function(gameno=8, turn.start=10, turn.end=GameLengths[gameno], turn.max=turn.end+10, k=1000, show=FALSE,graphic=c("flash", "pile"), speed=.25) {
	originalk <- k
	ids <- 1:25000
	# split up by game length
	if (GameLengths[gameno] < 15) return(0)
	if (GameLengths[gameno] < 25) {
		turn.start = 7
		turn.end = GameLengths[gameno]
		turn.max = 30
	} else {
		turn.start = 7
		turn.end = GameLengths[gameno]
		turn.max = GameLengths[gameno]+10
	}
	vals_v0 <- vals[GameLengths >= turn.end & GameLengths <= turn.max]
	ids <- ids[GameLengths >= turn.end & GameLengths <= turn.max]
	if (length(vals_v0) < 5) return(0)
	if (length(vals_v0) < k) k = length(vals_v0)-1
	vals_v1 <- lapply(vals_v0, function(x) return(x[turn.start:turn.end]))
	vals_v2 <- t(matrix(as.integer(unlist(vals_v1)), nrow=turn.end-turn.start+1))
	gameno <- which(ids==gameno)
	vals_v3 <- sapply((1:nrow(vals_v2)), function(x) {
	#vals_v3 <- unlist(clusterApply(cls, (1:nrow(vals_v2)), function(x) {
		sqrt(sum((vals_v2[gameno,] - vals_v2[x,])^2, na.rm=TRUE)) # Euclidian
		# you could adjust Euclidian so the later turns have more weight.
	})
	names(vals_v3) <- ids
	vals_v4 <- as.data.frame(sort(vals_v3))
	colnames(vals_v4) <- "mae"
	#if (k < originalk) {
	#	difference <- originalk - k
	#	vals_v4$mae <- c(vals_v4$mae, rep(NA, difference))
	#}
	if (show==TRUE) {
		graphic = graphic[1]
		plot(vals[[as.integer(rownames(vals_v4)[1])]][turn.start:turn.end], type='l', col='red')
		Sys.sleep(speed)
		if (k > 1) {
			for (i in 2:k) {
				lines(vals[[as.integer(rownames(vals_v4)[i])]][turn.start:turn.end])
				Sys.sleep(speed)
				if (graphic=="flash") {
					plot(vals[[as.integer(rownames(vals_v4)[1])]][turn.start:turn.end], type='l', col='red')
					Sys.sleep(speed)
				} # else pile
			}
		}
	}
	return(head(vals_v4, k))
}

cmp <- compare(gameno=3,k=3,show=TRUE,graphic="pile",speed=.05)

{
kval = 100
ssize = 100
start <- Sys.time()
maes <- lapply(sample(1:25000,ssize), function(x) {
	cmp <- compare(gameno=x,k=kval)
	if (is.integer(cmp) | is.numeric(cmp)) {
		return(cmp)
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
	return(mae)
})

finish <- Sys.time()
totalTime <- finish-start
secPerGame <- totalTime / 25000

lens <- sapply(maes,length)
maes.big <- maes[lens==(kval-1)]
maes.big.mat <- matrix(unlist(maes.big), nrow=(kval-1))
avgmaes <- apply(maes.big.mat, 1, mean, na.rm=TRUE)

## Break out by result
lens <- sapply(maes,length)
complete.game.nums <- which(lens==kval-1)
maes.complete <- maes[complete.game.nums]
train.complete <- train[complete.game.nums, ]
maes.complete.whitewins <- maes.complete[train.complete$Result == "1-0"]
maes.complete.blackwins <- maes.complete[train.complete$Result == "0-1"]
maes.complete.tie <- maes.complete[train.complete$Result == "1/2-1/2"]
maes.complete.whitewins.mat <- matrix(unlist(maes.complete.whitewins), nrow=kval-1)
maes.complete.blackwins.mat <- matrix(unlist(maes.complete.blackwins), nrow=kval-1)
maes.complete.tie.mat <- matrix(unlist(maes.complete.tie), nrow=kval-1)
avgmaes.whitewins <- apply(maes.complete.whitewins.mat, 1, mean, na.rm=TRUE)
avgmaes.blackwins <- apply(maes.complete.blackwins.mat, 1, mean, na.rm=TRUE)
avgmaes.ties <- apply(maes.complete.tie.mat, 1, mean, na.rm=TRUE)

min(avgmaes)
.38816*min(avgmaes.whitewins)+.30020*min(avgmaes.blackwins)+.31164*min(avgmaes.ties)
}


# break out by length
gl.cut <- cut(GameLengths, 40, labels=1:40)
gl.cut.condensed <- gl.cut[complete.game.nums]
a <- lapply(1:40, function(x) {
	maes.complete[gl.cut == x]
})
maes.complete.whitewins <- maes.complete[train.complete$Result == "1-0"]

# try breaking out by game length
# try using only the maes that have euclidian distances of <500 or something
# if there are no good comparisons, or if gamelength is too small, just give median?
# what is the best k? what is the best turn.max,start,end
# if the game is too long, comparing the whole game will only match to like one other game...


# Use compare to compare features instead of centipawns
# get numerical train
head(train)
tr <- train[,c("GameLength", "ResultNum2", "modeNum", "kur", "skw", "avg", "med", "sdv", "rng", "iqr", "kurd", "skwd", "avgd", "medd", "sdvd", "rngd", "iqrd", "isna")]
te <- test[,c("GameLength", "ResultNum2", "modeNum", "kur", "skw", "avg", "med", "sdv", "rng", "iqr", "kurd", "skwd", "avgd", "medd", "sdvd", "rngd", "iqrd", "isna")]

names(train)
train.temp <- train[,c("WhiteElo", "BlackElo")]
trz <- apply(tr, 2, scale, center=TRUE, scale=TRUE)
tez <- apply(te, 2, scale, center=TRUE, scale=TRUE)
trd <- dist(tr, diag=TRUE)
trdmat <- as.matrix(trd)

gameno = 1
trdmat1 <- trdmat[gameno,]
trdmat1.sorted <- sort(trdmat1)
train.temp.tmp <- train.temp[as.integer(names(trdmat1.sorted)), ]
label <- train.temp.tmp[1, ]
closes <- train.temp.tmp[-1, ]
closes$WhiteDif <- closes$WhiteElo - label$WhiteElo
closes$BlackDif <- closes$BlackElo - label$BlackElo
closes$lala <- (abs(closes$WhiteDif) + abs(closes$BlackDif))/2



# do i need to standardize the variables first? ResultNum2 >>> skw so yes
# weight votes by relative distance
# weight each game's guess closer to median the higher the relative distance
# and closer to the voted estimate the lower the relative distance





