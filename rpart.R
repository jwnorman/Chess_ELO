# explore variables through tree/vis of tree
library(rpart)
library(rpart.plot)
library(scales)

# fit tree for EloDiff
fit.dif2 <- rpart(formula = EloDiff ~ GameLength + Result + modeNum + kur + skw + avg + med + sdv + rng +iqr + kurd + skwd + avgd + medd + sdvd + rngd + iqrd + isna, data = train, method = "anova", control=rpart.control(cp = .0005))
prp(fit.dif2)
fit.dif.test2 <- predict(fit.dif2, test)

# fit tree for meanElo
fit.elo2 <- rpart(formula = meanElo ~ GameLength + Result + modeNum + kur + skw + avg + med + sdv + rng +iqr + kurd + skwd + avgd + medd + sdvd + rngd + iqrd + isna, data = train, method = "anova", control=rpart.control(cp = .0005))
prp(fit.elo2)
fit.elo.test2 <- predict(fit.elo2, test)

# Arrange in appropriate dataframe
restree2 <- data.frame(Event=25001:50000, Mean=rep(0,25000), Diff=rep(0,25000))
restree2$HalfDiff <- fit.dif.test2 / 2
restree2$WhiteElo <- fit.elo.test2 + restree2$HalfDiff
restree2$BlackElo <- fit.elo.test2 - restree2$HalfDiff
restree2 <- restree2[,c("Event", "WhiteElo", "BlackElo")]
write.csv(restree2, file='~/Desktop/Kaggle/ELO/restree2.csv', row.names=FALSE)

# Use model of restree2 and rescale to the dist of train
# bad idea
white.rescaled <- rescale(train$WhiteElo, to=c(min(c(train$WhiteElo, train$BlackElo)), max(c(train$WhiteElo, train$BlackElo))))
black.rescaled <- rescale(train$BlackElo, to=c(min(c(train$WhiteElo, train$BlackElo)), max(c(train$WhiteElo, train$BlackElo))))

restree2.scaled <- restree2
restree2.scaled$WhiteElo <- white.rescaled
restree2.scaled$BlackElo <- black.rescaled
write.csv(restree2.scaled, file='~/Desktop/Kaggle/ELO/restree2.scaled.csv', row.names=FALSE)


# Using pca
# fit tree for EloDiff
fit.dif <- rpart(formula = Dif ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6, data = tr.scores, method = "anova", control=rpart.control(cp = .001))
prp(fit.dif)
fit.dif.test <- predict(fit.dif, te.scores)

# fit tree for meanElo
fit.elo <- rpart(formula = Elo ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6, data = tr.scores, method = "anova", control=rpart.control(cp = .001))
prp(fit.elo)
fit.elo.test <- predict(fit.elo, te.scores)

# Arrange in appropriate dataframe
treecp <- data.frame(Event=25001:50000, Mean=rep(0,25000), Diff=rep(0,25000))
treecp$HalfDiff <- fit.dif.test / 2
treecp$WhiteElo <- fit.elo.test + treecp$HalfDiff
treecp$BlackElo <- fit.elo.test - treecp$HalfDiff
treecp <- treecp[,c("Event", "WhiteElo", "BlackElo")]
write.csv(treecp, file='~/Desktop/Kaggle/ELO/treecp.csv', row.names=FALSE)