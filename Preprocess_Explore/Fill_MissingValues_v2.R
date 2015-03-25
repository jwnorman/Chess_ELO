# Fill in NAs for modeNum, kur, skw, avg, med, sdv, rng, iqr
# For now just look at medians, means (depending on distributions) and perhaps their relatedness to the variables that never are missing
# note that all the missing come on the same observations for all variables that have missing values
combined <- rbind(train[,c("GameLength", "ResultNum2", "modeNum", "kur", "skw", "avg", "med", "sdv", "rng", "iqr", "kurd", "skwd", "avgd", "medd", "sdvd", "rngd", "iqrd")], test[,c("GameLength", "ResultNum2", "modeNum", "kur", "skw", "avg", "med", "sdv", "rng", "iqr", "kurd", "skwd", "avgd", "medd", "sdvd", "rngd", "iqrd")])

# Look at distributions
plot(density(combined$modeNum, na.rm=TRUE)) # use median
plot(density(combined$kur, na.rm=TRUE)) # use median
plot(density(combined$skw, na.rm=TRUE)) # use mean; correlated w ResultNum2, r=.7370
plot(density(combined$avg, na.rm=TRUE)) # use mean; correlated w ResultNum2, r=.6835
plot(density(combined$med, na.rm=TRUE)) # use mean; correlated w ResultNum2, r=.6252
plot(density(combined$sdv, na.rm=TRUE)) # use median
plot(density(combined$rng, na.rm=TRUE)) # trimodal, use median
plot(density(combined$iqr, na.rm=TRUE)) # use median
plot(density(combined$kurd, na.rm=TRUE)) # use median
plot(density(combined$skwd, na.rm=TRUE)) # use mean; correlated w ResultNum2, r=.6464
plot(density(combined$avgd, na.rm=TRUE)) # use mean; correlated w ResultNum2, r=.5783
plot(density(combined$medd, na.rm=TRUE)) # use mean; correlated w ResultNum2, r=.6050
plot(density(combined$sdvd, na.rm=TRUE)) # use median
plot(density(combined$rngd, na.rm=TRUE)) # use median
plot(density(combined$iqrd, na.rm=TRUE)) # use median

skwna <- tapply(combined$skw, combined$ResultNum2, mean, na.rm=TRUE)
avgna <- tapply(combined$avg, combined$ResultNum2, mean, na.rm=TRUE)
medna <- tapply(combined$med, combined$ResultNum2, mean, na.rm=TRUE)
skwdna <- tapply(combined$skwd, combined$ResultNum2, mean, na.rm=TRUE)
avgdna <- tapply(combined$avgd, combined$ResultNum2, mean, na.rm=TRUE)
meddna <- tapply(combined$medd, combined$ResultNum2, mean, na.rm=TRUE)

# replace missing with the mean/median/other value
train$modeNum <- ifelse(is.na(train$modeNum), median(combined$modeNum, na.rm=TRUE), train$modeNum)
test$modeNum <- ifelse(is.na(test$modeNum), median(combined$modeNum, na.rm=TRUE), test$modeNum)
train$kur <- ifelse(is.na(train$kur), median(combined$kur, na.rm=TRUE), train$kur)
test$kur <- ifelse(is.na(test$kur), median(combined$kur, na.rm=TRUE), test$kur)
train$skw <- ifelse(is.na(train$skw), skwna[as.character(train$ResultNum2)], train$skw)
test$skw <- ifelse(is.na(test$skw), skwna[as.character(test$ResultNum2)], test$skw)
train$avg <- ifelse(is.na(train$avg), avgna[as.character(train$ResultNum2)], train$avg)
test$avg<- ifelse(is.na(test$avg), avgna[as.character(test$ResultNum2)], test$avg)
train$med <- ifelse(is.na(train$med), medna[as.character(train$ResultNum2)], train$med)
test$med <- ifelse(is.na(test$med), medna[as.character(test$ResultNum2)], test$med)
train$sdv <- ifelse(is.na(train$sdv), median(combined$sdv, na.rm=TRUE), train$sdv)
test$sdv <- ifelse(is.na(test$sdv), median(combined$sdv, na.rm=TRUE), test$sdv)
train$rng <- ifelse(is.na(train$rng), median(combined$rng, na.rm=TRUE), train$rng)
test$rng <- ifelse(is.na(test$rng), median(combined$rng, na.rm=TRUE), test$rng)
train$iqr <- ifelse(is.na(train$iqr), median(combined$iqr, na.rm=TRUE), train$iqr)
test$iqr <- ifelse(is.na(test$iqr), median(combined$iqr, na.rm=TRUE), test$iqr)
train$kurd <- ifelse(is.na(train$kurd), median(combined$kurd, na.rm=TRUE), train$kurd)
test$kurd <- ifelse(is.na(test$kurd), median(combined$kurd, na.rm=TRUE), test$kurd)
train$skwd <- ifelse(is.na(train$skwd), skwdna[as.character(train$ResultNum2)], train$skwd)
test$skwd <- ifelse(is.na(test$skwd), skwdna[as.character(test$ResultNum2)], test$skwd)
train$avgd <- ifelse(is.na(train$avgd), avgdna[as.character(train$ResultNum2)], train$avgd)
test$avgd <- ifelse(is.na(test$avgd), avgdna[as.character(test$ResultNum2)], test$avgd)
train$medd <- ifelse(is.na(train$medd), meddna[as.character(train$ResultNum2)], train$medd)
test$medd <- ifelse(is.na(test$medd), meddna[as.character(test$ResultNum2)], test$medd)
train$sdvd <- ifelse(is.na(train$sdvd), median(combined$sdvd, na.rm=TRUE), train$sdvd)
test$sdvd <- ifelse(is.na(test$sdvd), median(combined$sdvd, na.rm=TRUE), test$sdvd)
train$rngd <- ifelse(is.na(train$rngd), median(combined$rngd, na.rm=TRUE), train$rngd)
test$rngd <- ifelse(is.na(test$rngd), median(combined$rngd, na.rm=TRUE), test$rngd)
train$iqrd <- ifelse(is.na(train$iqrd), median(combined$iqrd, na.rm=TRUE), train$iqrd)
test$iqrd <- ifelse(is.na(test$iqrd), median(combined$iqrd, na.rm=TRUE), test$iqrd)
save(train, file='~/Desktop/Kaggle/ELO/train.Rda')
save(test, file='~/Desktop/Kaggle/ELO/test.Rda')
