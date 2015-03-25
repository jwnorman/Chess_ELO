# use KNN
library(kknn)

# Using train.kknn for resknn model
# same as above but first scale()ing the numerical variables
tr <- train2[,-c(1,2,3,4,6,7,8,9,10)]
te <- test2[,-c(1,2,3,4,6,7,8,9)]
meanElo <- train$meanElo
diffElo <- train$EloDiff
combined <- rbind(tr,te)
combined.scaled <- scale(combined)
tr.scaled <- as.data.frame(combined.scaled[1:25000,])
te.scaled <- as.data.frame(combined.scaled[25001:50000,])

fit.dif <- train.kknn(diffElo ~ ., data=tr.scaled, kmax=150, distance=2, kernel="rectangular") # 92
pred.dif <- predict(fit.dif, te.scaled)

fit.avg <- train.kknn(meanElo ~ ., data=tr.scaled, kmax=150, distance=2, kernel="rectangular") # 93
pred.avg <- predict(fit.avg, te.scaled)

resknn <- data.frame(Event=25001:50000, Mean=rep(0,25000), Diff=rep(0,25000))
resknn$HalfDiff <- pred.dif / 2
resknn$WhiteElo <- pred.avg + resknn$HalfDiff
resknn$BlackElo <- pred.avg - resknn$HalfDiff
resknn <- resknn[,c("Event", "WhiteElo", "BlackElo")]
write.csv(resknn, file='~/Documents/Kaggle/ELO/knn20150322.csv', row.names=FALSE)

# best is now 199.42086, but i didn't change in the ranks

meanEloEst <- glm(meanElo ~ tr)