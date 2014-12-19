# Fill in NAs for modeNum, kur, skw, avg, med, sdv, rng, iqr
# For now just look at medians, means (depending on distributions) and perhaps their relatedness to the outcome
combined <- rbind(train[,c("modeNum", "kur", "skw", "avg", "med", "sdv", "rng", "iqr", "ResultNum2")], test[,c("modeNum", "kur", "skw", "avg", "med", "sdv", "rng", "iqr", "ResultNum2")])

# Look at distributions
plot(density(combined$modeNum, na.rm=TRUE)) # use median
plot(density(combined$kur, na.rm=TRUE)) # use median
plot(density(combined$skw, na.rm=TRUE)) # use mean
plot(density(combined$avg, na.rm=TRUE)) # highly correlated with ResultNum2
plot(density(combined$med, na.rm=TRUE)) # use mean
plot(density(combined$sdv, na.rm=TRUE)) # bimodal
plot(density(combined$rng, na.rm=TRUE)) # bimodal
plot(density(combined$iqr, na.rm=TRUE)) # use median

# Assign value to replace missing
median(combined$modeNum, na.rm=TRUE) # 3
median(combined$kur, na.rm=TRUE) # -.7398962
mean(combined$skw, na.rm=TRUE) # .06639355
missingAvgNums <- which(is.na(combined$avg))
fit.avg <- lm(combined$avg ~ combined$ResultNum2) # avg = 19.1447 + .5762*ResultNum2
mean(combined$med, na.rm=TRUE) # 19.88523
median(combined$sdv, na.rm=TRUE) # 91.43482; bimodal.. look in to this
median(combined$rng, na.rm=TRUE) # 316.8807; bimodal.. look in to this
median(combined$iqr, na.rm=TRUE) # 98.4375

# replace missing with the assigned value
names(combined)
train$modeNum <- ifelse(is.na(train$modeNum), 3, train$modeNum)
test$modeNum <- ifelse(is.na(test$modeNum), 3, test$modeNum)
train$kur <- ifelse(is.na(train$kur), -.7398962, train$kur)
test$kur <- ifelse(is.na(test$kur), -.7398962, test$kur)
train$skw <- ifelse(is.na(train$skw), .06639355, train$kur)
test$skw <- ifelse(is.na(test$skw), .06639355, test$kur)
train$avg[which(is.na(train$avg))] <- 19.1447 + .5762*train$ResultNum2[which(is.na(train$avg))]
test$avg[which(is.na(test$avg))] <- 19.1447 + .5762*test$ResultNum2[which(is.na(test$avg))]
train$med <- ifelse(is.na(train$med), 19.88523, train$med)
test$med <- ifelse(is.na(test$med), 19.88523, test$med)
train$sdv <- ifelse(is.na(train$sdv), ifelse(train$ResultNum2 == -17, 38.71526, 105.5797), train$sdv)
test$sdv <- ifelse(is.na(test$sdv), ifelse(test$ResultNum2 == -17, 38.71526, 105.5797), test$sdv)
train$rng <- ifelse(is.na(train$rng),316.8807, train$rng)
test$rng <- ifelse(is.na(test$rng),316.8807, test$rng)
train$iqr <- ifelse(is.na(train$iqr),98.4375, train$iqr)
test$iqr <- ifelse(is.na(test$iqr),98.4375, test$iqr)
save(train, file='~/Desktop/Kaggle/ELO/train.Rda')
save(test, file='~/Desktop/Kaggle/ELO/test.Rda')

