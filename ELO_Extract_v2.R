library(e1071)
library(caTools) # for runmean

# before running extract, make sure train is in its gamebygame state
# or else you'll get duplicate columns
# see Explore_vX.R
tr <- train[1:311,]
val <- as.integer(unlist(strsplit(as.character(tr$Valuations[45]), split=" ")))	
ls()
density.elo(tr,45)
plot(val, type='l')
lines(val.parsed, col="red")
lines(val.large, col="blue")

extract <- function(data) {
	tmp <- as.data.frame(t(sapply(1:nrow(data), function(row) {
		val <- as.integer(unlist(strsplit(as.character(data$Valuations[row]), split=" ")))	
		GameLength <- length(which(!is.na(val)))
		if (GameLength <= 8) {
			#return(c(kurd=NA,skwd=NA,avgd=NA,medd=NA,sdvd=NA,rngd=NA,iqrd=NA))
			return(c(modeNum=NA,kurd=NA,skwd=NA,avgd=NA,medd=NA,sdvd=NA,rngd=NA,iqrd=NA,kur=NA,skw=NA,avg=NA,med=NA,sdv=NA,rng=NA,iqr=NA))
		} else {
			val <- (val - 29.38542) / 690.7567
			val.wo6 <- val[-(1:6)]
			valDensity <- density(val.wo6, na.rm=TRUE)$y
			modeNum <- length(which(diff(sign(diff(valDensity)))==-2)+1) 
				# http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima; 
				# works more accurately with the estimated density values
			# val.large <- ifelse(abs(val.wo6)>300, (val.wo6/abs(val.wo6))*(300+sqrt(abs(abs(val.wo6)-300))), val.wo6)
			# val.parsed <- runmean(x=val.large, k=4, alg="C", endrule="trim")
			val.parsed <- val.wo6
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
			# return(c(modeNum,kur,skw,avg,med,sdv,rng,iqr))
			# return(c(kurd,skwd,avgd,medd,sdvd,rngd,iqrd))
			#val.z <- val
			#return(c(val.z,modeNum,kur,skw,avg,med,sdv,rng,iqr,kurd,skwd,avgd,medd,sdvd,rngd,iqrd))
			return(c(modeNum,kur,skw,avg,med,sdv,rng,iqr,kurd,skwd,avgd,medd,sdvd,rngd,iqrd))
		} #else
	} # function
	) # sapply
	) # t
	) # as.data.frame
	#colnames(tmp) <- c("modeNum","kur","skw","avg","med","sdv","rng","iqr")
	#colnames(tmp) <- c("kurd","skwd","avgd","medd","sdvd","rngd","iqrd")
	colnames(tmp) <- c("modeNum","kur","skw","avg","med","sdv","rng","iqr","kurd","skwd","avgd","medd","sdvd","rngd","iqrd")
	tmp$isna <- ifelse(is.na(tmp$kur), 1, 0)
	newdata <- cbind(data, tmp)
}
tr <- extract(tr)
train <- extract(train)
test <- extract(test)
save(train, file="C:/Kaggle/ELO/train.Rda")
save(test,  file="C:/Kaggle/ELO/test.Rda")

# Compute mean and sd of all Valuations for z-score
all.valuations <- c(as.integer(unlist(strsplit(as.character(train$Valuations), split=" "))), as.integer(unlist(strsplit(as.character(test$Valuations), split=" "))))
mean(all.valuations,na.rm=TRUE)
sd(all.valuations,na.rm=TRUE)

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