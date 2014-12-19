# use KNN
library(kknn)

#################################################################
# Using kknn to test training data using homemade cv

# to keep track of mae
mae.1 <- c()
mae.2 <- c()
mae.3 <- c()
mae.4 <- c()
# split training in to train and test
tr.nums <- sample(1:25000, 15000, replace=FALSE)
tr <- train[tr.nums, ]
te <- train[-tr.nums,]
kval <- 10
######## 1. all variables
# fit.diff <- kknn(EloDiff ~ GameLength + GameLength^2 + modeNum + kur + skw + avg + med + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
# fit.mean <- kknn(meanElo ~ GameLength + GameLength^2 + modeNum + kur + skw + avg + med + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
######## 2. w variables of last submission
# fit.diff <- kknn(EloDiff ~ GameLength + GameLength^2 + kur + skw + avg + sdv + ResultNum2, tr, te, k=kval, kernel="rectangular")
# fit.mean <- kknn(meanElo ~ GameLength + GameLength^2 + kur + skw + avg + sdv + ResultNum2, tr, te, k=kval, kernel="rectangular")
######## 3. w variables that make sense: 
#	- no GameLength^2 (there is nothing that suggests GameLength^2 has better info; the correlation between the two and meanElo and EloDiff are not distinguishable;)
#	- using only mean, not median because they supply almost the same information so we only need one but i think mean is better because slightly higher correlation with EloDiff and because for the missing values we were able to use the Result as a predictor of the missing
# fit.diff <- kknn(EloDiff ~ GameLength + modeNum + kur + skw + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
# fit.mean <- kknn(meanElo ~ GameLength + modeNum + kur + skw + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
######## 3. w variables that make sense and no skw
fit.diff <- kknn(EloDiff ~ GameLength + modeNum + kur + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
fit.mean <- kknn(meanElo ~ GameLength + modeNum + kur + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
White <- fit.mean$fitted.values + fit.diff$fitted.values/2
Black <- fit.mean$fitted.values - fit.diff$fitted.values/2
mae <- sum((abs(te$WhiteElo-White)+abs(te$BlackElo-Black)))/(nrow(te)*2)

#mae.1 <- c(mae.1, mae)
#mae.2 <- c(mae.2, mae)
#mae.3 <- c(mae.3, mae)
mae.4 <- c(mae.4, mae)

plot(density(mae.1), ylim=c(0.00,0.75), col="black")
lines(density(mae.2), col="red")
lines(density(mae.3), col="blue")
lines(density(mae.4), col="green")
#################################################################
# Using kknn to test training data using cv, with no gamelength
# split training in to train and test
tr.nums <- sample(1:25000, 15000, replace=FALSE)
tr <- train[tr.nums, ]
te <- train[-tr.nums,]
fit.diff <- kknn(EloDiff ~ kur + avg + sdv, tr, te, k=84, kernel="rectangular")
fit.mean <- kknn(meanElo ~ kur + avg + sdv, tr, te, k=84, kernel="rectangular")
White <- fit.mean$fitted.values + fit.diff$fitted.values/2
Black <- fit.mean$fitted.values - fit.diff$fitted.values/2
sum((abs(te$WhiteElo-White)+abs(te$BlackElo-Black)))/(nrow(te)*2)

#################################################################
# Using kknn to test training data using cv, with gamelength only
# split training in to train and test
tr.nums <- sample(1:25000, 15000, replace=FALSE)
tr <- train[tr.nums, ]
te <- train[-tr.nums,]
fit.diff <- kknn(EloDiff ~ GameLength + GameLength^2 + ResultNum2, tr, te, k=84, kernel="rectangular")
fit.mean <- kknn(meanElo ~ GameLength + GameLength^2 + ResultNum2, tr, te, k=84, kernel="rectangular")
White <- fit.mean$fitted.values + fit.diff$fitted.values/2
Black <- fit.mean$fitted.values - fit.diff$fitted.values/2
sum((abs(te$WhiteElo-White)+abs(te$BlackElo-Black)))/(nrow(te)*2)

#################################################################
# Using train.kknn for resknn model
train.tmp <- train[,c(1,2,5,7,8,10:13,15)]
test.tmp <- test[,c(1,2,5,7:12,14)]
EloDiffs <- train[,"EloDiff"]
EloMeans <- train[,"meanElo"]

fit.dif <- train.kknn(EloDiff ~ GameLength + modeNum + kur + avg + sdv + iqr + rng + ResultNum2, data=train, kmax=200, distance=2, kernel="rectangular") # 136
test.dif <- predict(fit.dif, test)

fit.avg <- train.kknn(meanElo ~ GameLength + modeNum + kur + avg + sdv + iqr + rng + ResultNum2, data=train, kmax=100, distance=2, kernel="rectangular") # 90
test.avg <- predict(fit.avg, test)

resknn <- data.frame(Event=25001:50000, Mean=rep(0,25000), Diff=rep(0,25000))
resknn$HalfDiff <- test.dif / 2
resknn$WhiteElo <- test.avg + resknn$HalfDiff
resknn$BlackElo <- test.avg - resknn$HalfDiff
resknn <- resknn[,c("Event", "WhiteElo", "BlackElo")]
write.csv(resknn, file='~/Desktop/Kaggle/ELO/resknn2.csv', row.names=FALSE)

# best model so far (k=60), but cv says it should be k=84
##################################################################

fit.dif <- train.kknn(EloDiffs ~ GameLength + GameLength^2 + ResultNum2, 
data=train.tmp, kmax=10, distance=2, kernel="rectangular")
test.dif <- predict(fit.dif, test.tmp)

fit.avg <- train.kknn(EloMeans ~ GameLength + GameLength^2 + ResultNum2,
data=train.tmp, kmax=10, distance=2, kernel="rectangular")
test.avg <- predict(fit.avg, test.tmp)

resknn <- res0
names(resknn) <- c("Event", "Mean", "Diff")
resknn$HalfDiff <- test.dif / 2
resknn$WhiteElo <- test.avg + resknn$HalfDiff
resknn$BlackElo <- test.avg - resknn$HalfDiff
resknn <- resknn[,c("Event", "WhiteElo", "BlackElo")]
write.csv(resknn_simpler, file='C:/Kaggle/ELO/resknn.csv', row.names=FALSE)

