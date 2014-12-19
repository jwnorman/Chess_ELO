library(e1071)
library(plyr)
library(lattice)

#mac
load(file='~/Desktop/Kaggle/ELO/train.Rda')	 #train
load(file='~/Desktop/Kaggle/ELO/test.Rda')	 #test
load(file='~/Desktop/Kaggle/ELO/gamebygame.Rda') #games
load(file='~/Desktop/Kaggle/ELO/playbyplay.Rda') #plays
#pc
load(file='C:/Kaggle/ELO/train.Rda')		 #train
load(file='C:/Kaggle/ELO/test.Rda')			 #test
load(file='C:/Kaggle/ELO/gamebygame.Rda')		 #games
load(file='C:/Kaggle/ELO/playbyplay.Rda')		 #plays

# To Reset train/test
{
train <- games[1:25000,]
test  <- games[25001:50000,]
train$meanElo <- (train$WhiteElo+train$BlackElo)/2
}

# Result tells a lot about the EloDiff
tapply(train$meanElo, train$Result, median)
summary(abs(train$EloDiff)) # Summary of EloDiff
tapply(train$EloDiff, train$Result, mean) # -160.45583, -12.01694, 146.52082
tapply(train$EloDiff, train$Result, median) # -172, -17, 158

# Plot of EloDiff distributions for each result
plot(density(train$EloDiff[train$Result=="0-1"]), 
xlim=c(-1000,1000), col="red",
main="Densities of EloDiff\nFor Different Results",
xlab="EloDiff")
lines(density(train$EloDiff[train$Result=="1/2-1/2"]), col="blue")
lines(density(train$EloDiff[train$Result=="1-0"]), col="green")
legend(-1000, .0025, legend=c("0-1", "1/2-1/2", "1-0"), col=c("red","blue","green"),
border="black", lty=1)

# Give numeric values to result in order to allow for 
# calculating the distance
tmp <- train$Result
train$ResultNum <- as.numeric(as.character(revalue(train$Result, c("1-0" = 146.52082, "1/2-1/2" = -12.01694, "0-1" = -160.45583))))
test$ResultNum <- as.numeric(as.character(revalue(test$Result, c("1-0" = 146.52082, "1/2-1/2" = -12.01694, "0-1" = -160.45583))))
train$ResultNum2 <- as.numeric(as.character(revalue(train$Result, c("1-0" = 158, "1/2-1/2" = -17, "0-1" = -172))))
test$ResultNum2 <- as.numeric(as.character(revalue(test$Result, c("1-0" = 158, "1/2-1/2" = -17, "0-1" = -172))))
save(train, file='~/Desktop/Kaggle/ELO/train.Rda')
save(test, file='~/Desktop/Kaggle/ELO/test.Rda')
save(train, file='C:/Kaggle/ELO/train.Rda')
save(test, file='C:/Kaggle/ELO/test.Rda')

# Compare Valuations and Valuation Densities from game to game
plot.elo <- function(data=train, gameno=1) {
	moves <- as.integer(unlist(strsplit(as.character(data$Valuations[gameno]), split=" ")))
	plot(moves, type='l')
}

density.elo <- function(data=train, gameno=1) {
	moves <- as.integer(unlist(strsplit(as.character(data$Valuations[gameno]), split=" ")))
	print(data[gameno,1:6])
	cat("Kurtosis: ", kurtosis(moves), "\n")
	cat("Skewness: ", skewness(moves), "\n")
	plot(density(moves))
}

density.elo(train, gameno=91)

tmp1 <- train[train$kur>0, ]
plot.elo(tmp1, gameno=6)
density.elo(tmp1, gameno=6)

# Playing with differences to find local maximums
# to be used in Extract.R
game <- 3
plot.elo(gameno=game)
density.elo(gameno=game)
tmp <- as.integer(unlist(strsplit(as.character(train$Valuations[game]), split=" ")))	
tmp2 <- density(tmp)$y
x <- 1:length(tmp2)
tmp3 <- loess(tmp2~x,span=.75)
plot(tmp3, type='l')
lines(tmp3$fitted, col="red")
#length(which(diff(sign(diff(tmp)))==-2)+1)
length(which(diff(sign(diff(tmp2)))==-2)+1)
#length(which(diff(sign(diff(tmp3$fitted)))==-2)+1)
rng <- max(tmp)-min(tmp)
IQR(tmp)

# Looking at correlation matrix
names(train)
nonnumerical <- c("Event", "Result", "Moves", "Valuations")
u <- train[,setdiff(names(train),nonnumerical)]
cormat <- cor(u, use="complete.obs")
write.csv(cormat, file="C:/Kaggle/ELO/cormat.csv", row.names=FALSE)

# Simple models to see where they stand
# mod0 (median) # 217.70369
medianElo <- median(c(train$WhiteElo, train$BlackElo))
res0 <- data.frame(Event=25001:50000, WhiteElo=rep(medianElo,25000), BlackElo=rep(medianElo,25000))
write.csv(res0, file='~/Desktop/Kaggle/ELO/res0.csv', row.names=FALSE)

# mod1 # 226.54436
mod1 <- lm(cbind(train$WhiteElo, train$BlackElo) ~ train$ResultNum2 + train$GameLength + train$GameLength^2 + train$ResultNum2*train$GameLength + train$ResultNum2*train$GameLength^2)
res1 <- predict(mod1, test)
res1 <- as.data.frame(cbind(25001:50000, res1))
names(res1) <- c("Event", "WhiteElo", "BlackElo")
head(res1,20)
write.csv(res1, file='~/Desktop/Kaggle/ELO/res1.csv', row.names=FALSE)

# from paper posted in forum
# - drop first 8 moves because they are so similar; all experts know all the popular openings
# - drop moves with abs() score > 300, or replace with 300, or do something here... currently nothingh
# - what to do with NAs?
# - they don't account for long term plans; they have each move independent of each other...

# Questions
# - compare valuation distributions for games where the winner is rates lower than loser
# - dist of elodiff given result
# - table of result given elodiff near 0 and ELOs are high, medium, low
# - ELO dist of winning, ELO dist of losing
# - ELO dist of winning when EloDiff is high, low, same
# - GameLength average for different results
# - GameLength average for different EloDiffs
# - dist of stockfish moves with EloDiff large, small
# - dist of stockfish moves with EloDiff near 0 and ELOs high, medium, low
# - if a better player can see farther ahead, perhaps this can be seen in the distribution of the stockfish... 
# - if you graph CPs of all games, does there seem to be a point in the game that favors certain rated players?
# - ways to summarize the valuations. % of -/+ given result, difference
# - are end games more telling than the beginning of games?
# - number of modes (local maximums) in distribution
# - look at variances of valuations, look at diffs too
# - how to get the significant local max's not ALL; use diff with lag,differences>1
# - how to determine whether a feature should be added as a dimension to, say, a knn model.