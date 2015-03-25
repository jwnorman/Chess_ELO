# Rebuild dataset from gamebygame point

# PC
load(file='C:/Kaggle/ELO/gamebygame.Rda')		 #games

# Mac
load(file='~/Documents/Kaggle/ELO/gamebygame.Rda') #games

# Split in to train and test
train <- games[1:25000,]
test  <- games[25001:50000,]

# Calculate mean Elo of White and Black
train$meanElo <- (train$WhiteElo+train$BlackElo)/2

# Give numeric values to result in order to allow for 
# calculating the distance in knn
train$ResultNum <- as.numeric(as.character(revalue(train$Result, c("1-0" = 146.52082, "1/2-1/2" = -12.01694, "0-1" = -160.45583))))
test$ResultNum <- as.numeric(as.character(revalue(test$Result, c("1-0" = 146.52082, "1/2-1/2" = -12.01694, "0-1" = -160.45583))))
train$ResultNum2 <- as.numeric(as.character(revalue(train$Result, c("1-0" = 158, "1/2-1/2" = -17, "0-1" = -172))))
test$ResultNum2 <- as.numeric(as.character(revalue(test$Result, c("1-0" = 158, "1/2-1/2" = -17, "0-1" = -172))))

# Proceed to Extract.R

# And then to Fill_MissingValues.R