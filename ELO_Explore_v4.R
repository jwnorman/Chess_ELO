library(e1071)
library(plyr)
library(lattice)

# PC
load(file='C:/Kaggle/ELO/train.Rda')		 #train
load(file='C:/Kaggle/ELO/test.Rda')			 #test
load(file='C:/Kaggle/ELO/gamebygame.Rda')		 #games
load(file='C:/Kaggle/ELO/playbyplay.Rda')		 #plays
save(train, file='C:/Kaggle/ELO/train.Rda')
save(test, file='C:/Kaggle/ELO/test.Rda')

# Mac
load(file='~/Documents/Kaggle/ELO/train.Rda')	 #train
load(file='~/Documents/Kaggle/ELO/test.Rda')	 #test
load(file='~/Documents/Kaggle/ELO/gamebygame.Rda') #games
load(file='~/Documents/Kaggle/ELO/playbyplay.Rda') #plays
save(train, file='~/Documents/Kaggle/ELO/train.Rda')
save(test, file='~/Documents/Kaggle/ELO/test.Rda')

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
legend(-1000, .0025, legend=c("0-1", "1/2-1/2", "1-0"), col=c("red","blue","green"), border="black", lty=1)

# Compare Valuations and Valuation Densities from game to game
plot.elo <- function(data=train, gameno=1, secondaryPlot=FALSE, color="black") {
	moves <- as.integer(unlist(strsplit(as.character(data$Valuations[gameno]), split=" ")))
	if (secondaryPlot) {
		lines(moves, col=color)
	} else {
		plot(moves, type='l', col=color)
	}
}

density.elo <- function(data=train, gameno=1, secondaryPlot=FALSE, color="black") {
	moves <- as.integer(unlist(strsplit(as.character(data$Valuations[gameno]), split=" ")))
	print(data[gameno,1:6])
	cat("Kurtosis: ", kurtosis(moves), "\n")
	cat("Skewness: ", skewness(moves), "\n")
	if (secondaryPlot) {
		lines(density(moves), col=color)
	} else {
		plot(density(moves), col=color)
	}
}

plot.elo.z <- function(data=train, gameno=1, secondaryPlot=FALSE, color="black") {
	moves <- as.integer(unlist(strsplit(as.character(data$Valuations[gameno]), split=" ")))
	moves <- (moves - 29.38542) / 690.7567
	if (secondaryPlot) {
		lines(moves, col=color)
	} else {
		plot(moves, type='l', col=color, xlim=c(0,150), ylim=c(-2,2))
	}
}

density.elo.z <- function(data=train, gameno=1, secondaryPlot=FALSE, color="black") {
	moves <- as.integer(unlist(strsplit(as.character(data$Valuations[gameno]), split=" ")))
	moves <- (moves - 29.38542) / 690.7567
	print(data[gameno,1:6])
	cat("Kurtosis: ", kurtosis(moves), "\n")
	cat("Skewness: ", skewness(moves), "\n")
	if (secondaryPlot) {
		lines(density(moves), col=color)
	} else {
		plot(density(moves), col=color, xlim=c(0,150), ylim=c(-2,2))
	}
}

# Plot densities of different types of games
# xxxx.yyyy = elo.dif
WhiteCut <- cut(train$WhiteElo, 6, labels=c(1,2,3,4,5,6))
BlackCut <- cut(train$BlackElo, 6, labels=c(1,2,3,4,5,6))
data11 <- train[WhiteCut==1 & BlackCut==1, ] # 9
data12 <- train[WhiteCut==1 & BlackCut==2, ] # 21
data13 <- train[WhiteCut==1 & BlackCut==3, ] # 10
data14 <- train[WhiteCut==1 & BlackCut==4, ] # 3
data15 <- train[WhiteCut==1 & BlackCut==5, ] # 1
data16 <- train[WhiteCut==1 & BlackCut==6, ] # 0
data22 <- train[WhiteCut==2 & BlackCut==2, ] # 123
data23 <- train[WhiteCut==2 & BlackCut==3, ] # 288
data24 <- train[WhiteCut==2 & BlackCut==4, ] # 71
data25 <- train[WhiteCut==2 & BlackCut==5, ] # 14
data26 <- train[WhiteCut==2 & BlackCut==6, ] # 1
data33 <- train[WhiteCut==3 & BlackCut==3, ] # 842
data34 <- train[WhiteCut==3 & BlackCut==4, ] # 1195
data35 <- train[WhiteCut==3 & BlackCut==5, ] # 251
data36 <- train[WhiteCut==3 & BlackCut==6, ] # 8
data44 <- train[WhiteCut==4 & BlackCut==4, ] # 3586
data45 <- train[WhiteCut==4 & BlackCut==5, ] # 2435
data46 <- train[WhiteCut==4 & BlackCut==6, ] # 104
data55 <- train[WhiteCut==5 & BlackCut==5, ] # 5274
data56 <- train[WhiteCut==5 & BlackCut==6, ] # 865
data66 <- train[WhiteCut==6 & BlackCut==6, ] # 1033
plot.elo.z(data=data11, gameno=1)
for(i in 2:50) {
	plot.elo.z(data=data66, gameno=i, TRUE, "black")
	Sys.sleep(.2)
}


# Playing with differences to find local maximums
# to be used in Extract.R
tmp <- as.integer(unlist(strsplit(as.character(train$Valuations[game]), split=" ")))
tmp2 <- density(tmp)$y
x <- 1:length(tmp2)
tmp3 <- loess(tmp2~x,span=.75)
plot(tmp3, type='l')
lines(tmp3$fitted, col="red")
#length(which(diff(sign(diff(tmp)))==-2)+1)
length(which(diff(sign(diff(tmp2)))==-2)+1)
#length(which(diff(sign(diff(tmp3$fitted)))==-2)+1)

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
write.csv(res0, file='~/Documents/Kaggle/ELO/res0.csv', row.names=FALSE)

# mod1 # 226.54436
mod1 <- lm(cbind(train$WhiteElo, train$BlackElo) ~ train$ResultNum2 + train$GameLength + train$GameLength^2 + train$ResultNum2*train$GameLength + train$ResultNum2*train$GameLength^2)
res1 <- predict(mod1, test)
res1 <- as.data.frame(cbind(25001:50000, res1))
names(res1) <- c("Event", "WhiteElo", "BlackElo")
head(res1,20)
write.csv(res1, file='~/Documents/Kaggle/ELO/res1.csv', row.names=FALSE)

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

# - i think the farther a player can look ahead and excuse successfully, the better ranking they'll be; if you find, perhaps, an exchange that starts at +20 and ends at +40 and in between goes down to -80 or something (without just one large jump), this looks good for white... you need to find which features contribute best for diff and which are best for elo... you need to graphically look at each variable and how it looks down for each elo rating, and each elo difference