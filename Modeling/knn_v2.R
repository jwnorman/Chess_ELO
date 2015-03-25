# use KNN
library(kknn)

# Standardize first before knning

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
fit.diff <- kknn(EloDiff ~ GameLength + GameLength^2 + kur + skw + avg + sdv + ResultNum2, tr, te, k=kval, kernel="rectangular")
fit.mean <- kknn(meanElo ~ GameLength + GameLength^2 + kur + skw + avg + sdv + ResultNum2, tr, te, k=kval, kernel="rectangular")
######## 3. w variables that make sense: 
#	- no GameLength^2 (there is nothing that suggests GameLength^2 has better info; the correlation between the two and meanElo and EloDiff are not distinguishable;)
#	- using only mean, not median because they supply almost the same information so we only need one but i think mean is better because slightly higher correlation with EloDiff and because for the missing values we were able to use the Result as a predictor of the missing
# fit.diff <- kknn(EloDiff ~ GameLength + modeNum + kur + skw + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
# fit.mean <- kknn(meanElo ~ GameLength + modeNum + kur + skw + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
######## 3. w variables that make sense and no skw
#fit.diff <- kknn(EloDiff ~ GameLength + modeNum + kur + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
#fit.mean <- kknn(meanElo ~ GameLength + modeNum + kur + avg + sdv + iqr + rng + ResultNum2, tr, te, k=kval, kernel="rectangular")
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

fit.dif <- train.kknn(EloDiff ~ GameLength + ResultNum2 + modeNum + kur + skw + avg + med + sdv + rng + iqr + kurd + skwd + avgd + medd + sdvd + rngd + iqrd + isna, data=train, kmax=200, distance=2, kernel="rectangular") # 136
test.dif <- predict(fit.dif, test)

fit.avg <- train.kknn(meanElo ~ GameLength + ResultNum2 + modeNum + kur + skw + avg + med + sdv + rng +iqr + kurd + skwd + avgd + medd + sdvd + rngd + iqrd + isna, data=train, kmax=100, distance=2, kernel="rectangular") # 90
test.avg <- predict(fit.avg, test)

resknn <- data.frame(Event=25001:50000, Mean=rep(0,25000), Diff=rep(0,25000))
resknn$HalfDiff <- test.dif / 2
resknn$WhiteElo <- test.avg + resknn$HalfDiff
resknn$BlackElo <- test.avg - resknn$HalfDiff
resknn <- resknn[,c("Event", "WhiteElo", "BlackElo")]
write.csv(resknn, file='~/Documents/Kaggle/ELO/resknnTEST.csv', row.names=FALSE)

# best model so far (k=60), but cv says it should be k=84
##################################################################

#################################################################
# Using train.kknn for resknn model
# same as above but first scale()ing the numerical variables
tr <- train[,-c(1,2,3,4,6,7,8,9,10)]
te <- test[,-c(1,2,3,4,6,7,8,9)]
meanElo <- train$meanElo
diffElo <- train$EloDiff
combined <- rbind(tr,te)
combined.scaled <- scale(combined)
tr.scaled <- as.data.frame(combined.scaled[1:25000,])
te.scaled <- as.data.frame(combined.scaled[25001:50000,])
head(tr.scaled)
fit.dif <- train.kknn(diffElo ~ ., data=tr.scaled, kmax=150, distance=2, kernel="rectangular") # 145
pred.dif <- predict(fit.dif, te.scaled)

fit.avg <- train.kknn(meanElo ~ ., data=tr.scaled, kmax=150, distance=2, kernel="rectangular") # 90
pred.avg <- predict(fit.avg, te.scaled)

resknn <- data.frame(Event=25001:50000, Mean=rep(0,25000), Diff=rep(0,25000))
resknn$HalfDiff <- pred.dif / 2
resknn$WhiteElo <- pred.avg + resknn$HalfDiff
resknn$BlackElo <- pred.avg - resknn$HalfDiff
resknn <- resknn[,c("Event", "WhiteElo", "BlackElo")]
write.csv(resknn, file='~/Documents/Kaggle/ELO/knn20150220.csv', row.names=FALSE)
# this improved score by like .11
# but it is best!
# scaling helped a little!
# best is now 199.42086, but i didn't change in the ranks

names(tr)
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



# pca
tr <- train[,c(5,11:27)]
pc.tr <- princomp(tr)
te <- test[,c(5,10:26)]
pc.te <- princomp(te)
tr.scores <- as.data.frame(cbind(Dif=train$EloDiff, Elo=train$meanElo, pc.tr$scores[,1:6]))
te.scores <- as.data.frame(cbind(pc.te$scores[,1:6]))

fit.dif.knn.pc <- train.kknn(Dif ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6, data=tr.scores, kmax=250, distance=2, kernel="rectangular")
test.dif.knn.pc <- predict(fit.dif.knn.pc, te.scores)
fit.elo.knn.pc <- train.kknn(Elo ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6, data=tr.scores, kmax=250, distance=2, kernel="rectangular")
test.elo.knn.pc <- predict(fit.elo.knn.pc, te.scores)

resknnpc <- data.frame(Event=25001:50000, Mean=rep(0,25000), Diff=rep(0,25000))
resknnpc$HalfDiff <- test.dif.knn.pc / 2
resknnpc$WhiteElo <- test.elo.knn.pc + resknnpc$HalfDiff
resknnpc$BlackElo <- test.elo.knn.pc - resknnpc$HalfDiff
resknnpc <- resknnpc[,c("Event", "WhiteElo", "BlackElo")]
write.csv(resknnpc, file='~/Desktop/Kaggle/ELO/resknnpc.csv', row.names=FALSE)

