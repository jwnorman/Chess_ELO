

localMaximumSize <- 2
numOffCenter <- localMaximumSize
zerosLR <- rep(0, numOffCenter*2)
zerosC  <- rep(0, numOffCenter)
ttL <- c(zerosLR, tt)
ttC <- c(zerosC, tt, zerosC)
ttR <- c(tt, zerosLR)
ttL <- ttL[(1+length(zerosLR)):(length(ttL)-length(zerosLR))]
ttC <- ttC[(1+length(zerosLR)):(length(ttC)-length(zerosLR))]
ttR <- ttR[(1+length(zerosLR)):(length(ttR)-length(zerosLR))]
which(ttC > ttL & ttC > ttR)
