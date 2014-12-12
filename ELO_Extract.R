library(e1071)
library(caTools) # for runmean
test <- head(g.train[,c(2,3,4,5,6,8)], 311)
names(test)
row <- 33
extract <- as.data.frame(t(sapply(1:nrow(test), function(row) {
	if (test$GameLength[row] <= 8) {
		return(c(kur=NA,skw=NA,avg=NA,sdv=NA))
	} else {
		val <- as.integer(unlist(strsplit(as.character(test$Valuations[row]), split=" ")))	
		val.wo6 <- val[-(1:6)]
		val.large <- ifelse(abs(val.wo6)>300, (val.wo6/abs(val.wo6))*(300+sqrt(abs(abs(val.wo6)-300))), val.wo6)
		val.parsed <- runmean(x=val.large, k=4, alg="C", endrule="trim")
		# how long can exchanges be? 1 turn? 2? 3? 4?
		peaks <- findPeaks(val.parsed)
		kur <- kurtosis(val.parsed, na.rm=TRUE)
		skw <- skewness(val.parsed, na.rm=TRUE)
		avg <- mean(val.parsed, na.rm=TRUE)
		sdv <- sd(val.parsed, na.rm=TRUE)
		return(c(kur,skw,avg,sdv))
	}
})))


findPeaks <- function(val) {
	densy <- density(val)
	first <- diff(densy$y[1:(length(densy$x)-1)])
	second <- diff(densy$y[2:length(densy$x)])
	peaks <-(sum(ifelse((first<0) != (second<0), 0, 1))+1)/2 # number of peaks
}
plot(density(val.parsed))
plot(first, type='l')
plot(density(val))#8?

test[row, ]
# count the number of peaks in density plot.

Animations {
# Densities
for(i in 1:45) {
	plot(density(runmean(x=val.large, k=i, alg="C", endrule="trim")))
}
for(i in 1:45) {
	plot(density(runmean(x=val.parsed, k=i, alg="C", endrule="trim")))
}

# Valuations
for(i in 1:45) {
	plot(runmean(x=val.large, k=i, alg="C", endrule="trim"), type='l')
}

for(i in 1:45) {
	plot(runmean(x=val.parsed, k=i, alg="C", endrule="trim"), type='l')
}
}
cbind(first,second)

bla <- 
first <- diff(bla$y[1:(length(bla$x)-1)])
second <- diff(bla$y[2:length(bla$x)])
(sum(ifelse(first/abs(first)==second/abs(second), 0, 1))+1)/2 # number of peaks
plot(density(runmean(x=val.large, k=4, alg="C", endrule="trim")))

colnames(extract) <- c("kur", "skw", "avg", "sdv")
?diff
head(g.train$GameLength,1000)

# be weary of exchanges, valuation will go from 20 to 300 to 20 and the 300 shouldn't be looked at
# look at every other valuation? find who has control by finding who initiates an exchange.. are exchanges notable?
# try a different smoothing technique, like HoltWinters instead of MA?
plot(val, type='l')
plot(diff(val,25), type='l')
row
# it seems when the result is 0-1 or 1-0, the variance starts small and grows large;
# so its not heteroskedastistic.. could you model the change in variance by taking a 
# moving variance (as oppoed to MA)?
g.train[200,]

# look at the games where the higher rated player lost and look for statistics that indicate the quality of play was more equal than expected
# diff(val, lag=2) shows you the difference from white turn to white turn and from left turn to left turn
# look at mean(diff(val,lag=2))
# if the large jump in valuation (from -200 to -600) happens on whites turn, then isn't it white's fault (assuming the valuation isn't made up on the next play)