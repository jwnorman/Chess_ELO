# Look at distributions given different ELOs and ELO differences

maxPlots = 50
rating1 = 2500
plusminus1 = 50
rating2 = 1800
plusminus2 = 50

subtrain <- train[train$WhiteElo >= rating1-plusminus1 & 
				  train$WhiteElo <= rating1+plusminus1 &
				  train$BlackElo >= rating2-plusminus2 & 
				  train$BlackElo <= rating2+plusminus2, ]
				  
maxPlots = min(maxPlots, nrow(subtrain))

for (i in sample(1:nrow(subtrain), maxPlots)) {
	vals <- unlist(strsplit(as.character(subtrain$Valuations[i]), split=" "))
	vals <- ifelse(is.na(vals), 0, vals)
	print(i)
	plot(vals, type = 'l', ylim=c(-600,600), xlim=c(0, 150))
	Sys.sleep(.25)
}