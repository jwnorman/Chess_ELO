library(e1071)
library(caTools) # for runmean

# before running extract, make sure train is in its gamebygame state
# or else you'll get duplicate columns
# see Explore_vX.R

middle <- train[train$GameLength >= 42 & train$GameLength <= 139, ]
tr <- train[1:300, ]
val <- as.integer(unlist(strsplit(as.character(middle$Valuations[600]), split=" ")))

extract <- function(data) {
	tmp <- as.data.frame(t(sapply(1:nrow(data), function(row) {
		val <- as.integer(unlist(strsplit(as.character(data$Valuations[row]), split=" ")))	
		GameLength <- length(which(!is.na(val)))
		if (GameLength <= 11) {
			return(c(modeNum=0, kurd=0, skwd=0, avgd=0, medd=0, sdvd=0, rngd=0, iqrd=0, kur=0, skw=0, avg=0, med=0, sdv=0, rng=0, iqr=0))
		} else {
			val.wo6 <- val[-(1:6)]
			val.wo6.wona <- val.wo6[!is.na(val.wo6)]
			val.wo6.scaled <- scale(val.wo6.wona)
			val.wo6.woexch <- val.wo6.wona[abs(val.wo6.scaled) < 3]
			numExchanges   <- length(val.wo6.scaled[abs(val.wo6.scaled) >= 3])
			valDensity <- density(val.wo6.woexch, na.rm=TRUE)$y
			modeNum <- length(which(diff(sign(diff(valDensity)))==-2)+1)
			val.large <- ifelse(abs(val.wo6.woexch)>300, (val.wo6.woexch/abs(val.wo6.woexch))*(300+sqrt(abs(abs(val.wo6.woexch)-300))), val.wo6.woexch)
			val.spline <- smooth.spline(val.large, spar=.75)$y
			val.parsed <- val.spline
			dif <- diff(val.parsed)
			kurd <- kurtosis(dif, na.rm=TRUE)
			skwd <- skewness(dif, na.rm=TRUE)
			avgd <- mean(dif, na.rm=TRUE)
			medd <- median(dif, na.rm=TRUE)
			sdvd <- sd(dif, na.rm=TRUE)
			rngd <- max(dif, na.rm=TRUE) - min(dif, na.rm=TRUE)
			iqrd <- IQR(dif, na.rm=TRUE)
			kur <- kurtosis(val.parsed, na.rm=TRUE)
			skw <- skewness(val.parsed, na.rm=TRUE)
			avg <- mean(val.parsed, na.rm=TRUE)
			med <- median(val.parsed, na.rm=TRUE)
			sdv <- sd(val.parsed, na.rm=TRUE)
			rng <- max(val.parsed, na.rm=TRUE) - min(val.parsed, na.rm=TRUE)
			iqr <- IQR(val.parsed, na.rm=TRUE)
			return(c(modeNum, kur, skw, avg, med, sdv, rng, iqr, kurd, skwd, avgd, medd, sdvd, rngd, iqrd))
		} #else
	} # function
	) # sapply
	) # t
	) # as.data.frame
	colnames(tmp) <- c("modeNum", "kur", "skw", "avg", "med", "sdv", "rng", "iqr", "kurd", "skwd", "avgd", "medd", "sdvd", "rngd", "iqrd")
	tmp$isna <- ifelse(tmp$kur==0 & tmp$modeNum==0, 1, 0)
	newdata <- cbind(data, tmp)
}
train2 <- extract(train)
test2 <- extract(test)
save(train2, file='~/Documents/Kaggle/ELO/train2.Rda')
save(test2, file='~/Documents/Kaggle/ELO/test2.Rda')

Animations {
row <- 4 # param1
sleeptime <- .1 #param2
val <- as.integer(unlist(strsplit(as.character(test$Valuations[row]), split=" ")))	
val.wo6 <- val[-(1:6)]
valDensity <- density(val.wo6, na.rm=TRUE)$y
modeNum <- length(which(diff(sign(diff(valDensity)))==-2)+1) # http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
val.large <- ifelse(abs(val.wo6)>300, (val.wo6/abs(val.wo6))*(300+sqrt(abs(abs(val.wo6)-300))), val.wo6)

# Densities
for(i in 1:45) {
	plot(density(runmean(x=val.large, k=i, alg="C", endrule="trim")))
	Sys.sleep(sleeptime )
}
for(i in 1:45) {
	plot(density(runmean(x=val.parsed, k=i, alg="C", endrule="trim")))
	Sys.sleep(sleeptime )
}

# Valuations
for(i in 1:45) {
	plot(runmean(x=val.large, k=i, alg="C", endrule="trim"), type='l')
	Sys.sleep(sleeptime )
}

for(i in 1:45) {
	plot(runmean(x=val.parsed, k=i, alg="C", endrule="trim"), type='l')
	Sys.sleep(sleeptime)
}
}

# be weary of exchanges, valuation will go from 20 to 300 to 20 and the 300 shouldn't be looked at
# look at every other valuation? find who has control by finding who initiates an exchange.. are exchanges notable?
# try a different smoothing technique, like HoltWinters instead of MA?
# it seems when the result is 0-1 or 1-0, the variance starts small and grows large;
# so its not heteroskedastistic.. could you model the change in variance by taking a 
# moving variance (as oppoed to MA)?
# look at the games where the higher rated player lost and look for statistics that indicate the quality of play was more equal than expected
# diff(val, lag=2) shows you the difference from white turn to white turn and from left turn to left turn
# look at mean(diff(val,lag=2))
# if the large jump in valuation (from -200 to -600) happens on whites turn, then isn't it white's fault (assuming the valuation isn't made up on the next play)