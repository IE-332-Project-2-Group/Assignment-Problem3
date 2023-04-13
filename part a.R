library(dplyr)
set.seed(678)
path <- "C:\\Users\\surre\\Desktop\\IE 332\\A2 Q3\\aps_failure_training_set.csv"
aps <-read.csv(path, stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
shuffle_index <- sample(1:nrow(aps))
aps <- aps[shuffle_index, ]
#head(aps)
#preparing the data for the model
subset <- aps[1:3000]
subset[(subset=="na")]=NA
sub2 <- sapply(subset, as.numeric)
subset2 <- cbind.data.frame(subset,sub2)
colnames(subset2) <- "class"
glimpse(subset2)