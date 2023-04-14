
#Import the data and clean it of Na's. Convert neg,pos to 0,1.

install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)
aps_failure_data_clean <- read.csv("C:\\Users\\surre\\Desktop\\IE 332\\A2 Q3\\aps_failure_training_set.csv", skip = 20, na.strings = "na")
aps_failure_data_clean$class <- ifelse(aps_failure_data_clean$class == "pos", 1, 0)

#Setting the dimension of the data set to 20000
aps_failure_data_clean <- head(aps_failure_data_clean, 20000)

#splitting the clean data set into train/test sets using 70% for training and 20% for testing
create_train_test <- function(aps_failure_data_clean, size = 0.7, train = TRUE) {
  n_row = nrow(aps_failure_data_clean)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (aps_failure_data_clean[train_sample, ])
  } else {
    return (aps_failure_data_clean[-train_sample, ])
  }
}
data_train1 <- create_train_test(aps_failure_data_clean, 0.7, train = TRUE)
data_test1 <- create_train_test(aps_failure_data_clean, 0.7, train = FALSE)


#Part a

#Installing the necessary packages
install.packages("rpart", repos="http://cran.us.r-project.org")
install.packages("rpart.plot", repos="http://cran.us.r-project.org")
install.packages("caret", repos="http://cran.us.r-project.org")
install.packages("parsnip", repos="http://cran.us.r-project.org")

# Loading the necessary packages
library(rpart.plot)
library(rpart)
library(caret)
library(parsnip)

#Training the decision tree using training data
fit1 <- rpart(class ~ ., data = data_train1, maxdepth = 20)

#Plotting decision tree
rpart.plot(fit1, type = 2)

#Applying to the testing data
predict_test1 <- predict(fit1, data_test1, type = "vector")

#Calculating testing risk
risk_test1 <- mean(predict_test1 != data_test1$class)

#Applying to training data and calculating training risk
predict.train1 <- predict(fit1, data_train1, type = "vector")
risk_train1 <- mean(predict.train1 != data_train1$class)

# Calculating the false negative rates for training and testing data sets
train_confusion_matrix1 <- table(data_train1$class, predict.train1)
test_confusion_matrix1 <- table(data_test1$class, predict_test1)

train_false_negative_rate <- train_confusion_matrix1[2, 1]/sum(train_confusion_matrix1[2, ])
test_false_negative_rate <- test_confusion_matrix1[2, 1]/sum(test_confusion_matrix1[2, ])

dev.new()


#Part b

#Creating 2 plots with different cp values: X axis: max depth/size of decision tree, Y axis: false negative rate

# Define a sequence of values for cp for pre-pruning
sequence_cp_pre_pruning <- c(0, 0.05)

# Generate a window for plots
par(mfrow = c(2, 2))

# Looping through each value of cp in the assigned sequence and generating a plot for each
for (j in sequence_cp_pre_pruning) {
  
  train.fnrs <- vector("numeric", 20)
  test.fnrs <- vector("numeric", 20)
  
  for (i in 1:20) {
    # Training a decision tree model using the training data 
    fit2 <- rpart(class ~ ., data = data_train1, cp = j, maxdepth = i + 1)
    predict.train2 <- predict(fit2, data_train1, type = "vector")
    predict_test2 <- predict(fit2, data_test1, type = "vector")
    
    # Calculate the false negative rates
    train_confusion_matrix2 <- table(data_train1$class, predict.train2)
    test_confusion_matrix2 <- table(data_test1$class, predict_test2)
    
    train.fnr <- train_confusion_matrix2[2, 1]/sum(train_confusion_matrix2[2, ])
    test.fnr <- test_confusion_matrix2[2, 1]/sum(test_confusion_matrix2[2, ])
    
    train.fnrs[i] <- train.fnr
    test.fnrs[i] <- test.fnr
  }
  
  plot <- plot(1:20, train.fnrs, type = "o", col = "blue", ylim = range(train.fnrs,
                                                                       test.fnrs), xlab = "Size of tree", ylab = "false negative rates", main = paste("CP=", j))
  lines(1:20, test.fnrs, type = "o", col = "red")
  legend("center", legend = c("train.fnr", "test.fnr"), col = c("blue", "red"),
         lty = 1)
}

par(mfrow = c(1, 1))

dev.new()


#Part c

#Creating 2 plots with different cp values: X axis: max depth/size of decision tree, Y axis: false negative rate

# Define a sequence of values for cp for post-pruning
sequence_cp_post_pruning <- c(0, 0.23)

# Generate a window for plots
par(mfrow = c(2, 2))

# Looping through each value of cp in the assigned sequence and generating a plot for each
for (j in sequence_cp_post_pruning) {
  
  train.fnrs1 <- vector("numeric", 20)
  test.fnrs1 <- vector("numeric", 20)
  
  for (i in 1:20) {
    # Training a decision tree model using the training data 
    fit3 <- rpart(class ~ ., data = data_train1, maxdepth = i + 1)
    
    # Implement post-pruning on the model
    fit3 <- prune(fit3, cp = j)
    predict.train3 <- predict(fit3, data_train1, type = "vector")
    predict_test3 <- predict(fit3, data_test1, type = "vector")
    
    # Calculating the false negative rates
    train_confusion_matrix3 <- table(data_train1$class, predict.train3)
    test_confusion_matrix3 <- table(data_test1$class, predict_test3)
    
    train.fnr1 <- train_confusion_matrix3[2, 1]/sum(train_confusion_matrix3[2, ])
    test.fnr1 <- test_confusion_matrix3[2, 1]/sum(test_confusion_matrix3[2, ])
    
    train.fnrs1[i] <- train.fnr1
    test.fnrs1[i] <- test.fnr1
  }
  
  plot(1:20, train.fnrs1, type = "o", col = "blue", ylim = range(train.fnrs1, test.fnrs1),
       xlab = "Size of tree", ylab = "false negative rates", main = paste("Post-pruning value=", j))
  lines(1:20, test.fnrs1, type = "o", col = "red")
  legend("center", legend = c("train.fnr1", "test.fnr1"), col = c("blue", "red"),
         lty = 1)
}

par(mfrow = c(1, 1))

dev.new()

#Part d

aps_failure_data_clean2 <- read.csv("C:\\Users\\surre\\Desktop\\IE 332\\A2 Q3\\aps_failure_training_set.csv", skip = 20, na.strings = "na")
aps_failure_data_clean2$class <- ifelse(aps_failure_data_clean2$class == "pos", 1, 0)

#Setting the dimension of the data set to 60000
aps_failure_data_clean2 <- head(aps_failure_data_clean2, 60000)

#splitting the clean data set into train/test sets using 80% for training and 20% for testing
create_train_test2 <- function(aps_failure_data_clean2, size = 0.80, train = TRUE) {
  n_row = nrow(aps_failure_data_clean2)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (aps_failure_data_clean2[train_sample, ])
  } else {
    return (aps_failure_data_clean2[-train_sample, ])
  }
}
data_train2 <- create_train_test2(aps_failure_data_clean2, 0.80, train = TRUE)
data_test2 <- create_train_test2(aps_failure_data_clean2, 0.80, train = FALSE)

#Training the decision tree using training data
fit4 <- rpart(class ~ ., data = data_train2, cp = 0, maxdepth = 5)

#Plotting decision tree
rpart.plot(fit4, type = 2)

#Applying to the testing data
predict_test4 <- predict(fit4, data_test2, type = "vector")

#Calculating testing risk
risk_test2 <- mean(predict_test4 != data_test2$class)

#Applying to training data and calculating training risk
predict.train4 <- predict(fit4, data_train2, type = "vector")
risk_train2 <- mean(predict.train4 != data_train2$class)

train.fnrs2 <- vector("numeric", 20)
test.fnrs2 <- vector("numeric", 20)

for (i in 1:20) {
  # Training a decision tree model using the training data  
  fit4 <- rpart(class ~ ., data = data_train2, cp = 0, maxdepth = i + 1)
  predict.train2 <- predict(fit4, data_train2, type = "vector")
  predict_test2 <- predict(fit4, data_test2, type = "vector")
  
  # Calculate the false negative rates
  train_confusion_matrix2 <- table(data_train2$class, predict.train2)
  test_confusion_matrix2 <- table(data_test2$class, predict_test2)
  
  train.fnr2 <- train_confusion_matrix2[2, 1]/sum(train_confusion_matrix2[2, ])
  test.fnr2 <- test_confusion_matrix2[2, 1]/sum(test_confusion_matrix2[2, ])
  
  train.fnrs2[i] <- train.fnr2
  test.fnrs2[i] <- test.fnr2
}

plot <- plot(1:20, train.fnrs2, type = "o", col = "red", ylim = range(train.fnrs2,
                                                                     test.fnrs2), xlab = "Size of tree", ylab = "false negative rates", main = paste("Data Size = 60000, Train dataset = 0.85%, CP=",
                                                                                                                                0))
lines(1:20, test.fnrs2, type = "o", col = "blue")
legend("center", legend = c("train.fnr2", "test.fnr2"), col = c("red", "blue"), lty = 1)

#Comparing the decision tree from part d to the tree from part a, we can see that the training and testing risks are lower in part d. 
# In part d, the training risk is 0.995 and the testing risk is 0.996. These are lower than the training risk of 0.996 and testing risk of 0.997
# in part a, while still being close to each other in value.
#The risk was reduced in part d by increasing the size of the data set to 60000, making the training dataset equal to 80%,
# and reducing the maxdepth of the tree from 20 to 5.


#Part e

#We believe there are 3 characteristics are affecting the overfitting behavior: 1) The size of the data set itself- the size 
#of the data set should be several times bigger (about 10 times according to certain rules of thumb) than the dimensionality of the dataset.
#, 2) The max depth/size of the tree (pre-pruning)- this determines when the tree will stop growing. A very large depth of tree can lead to overfitting.
#, 3) Pruning- post-pruning can help determine the optimal cp value and can reduce overfitting.

#Part f

# Load the randomForest library
library(randomForest)
library(base)

# Using the model in Q4 and removing NA's
aps_failure_data_clean3 <- na.omit(aps_failure_data_clean2)

#Do the ramdom forest test
random_forest <- randomForest(formula = class ~ ., data = aps_failure_data_clean3) 

# Getting the importance of different variables and identifying 2 most important ones
importance <- data.frame(random_forest$importance)

# Plot the variable importance
varImpPlot(random_forest)

# Answer to the question:
#Based on the dataset for part d, the two most important variables with the most important are bj_000 and am_0.

dev.new()


#Part g

install.packages("remotes", repos="http://cran.us.r-project.org")
remotes::install_github("grantmcdermott/parttree")
library(parttree)
library(parsnip)

# Create a new subset that contain class, bj_000, and am_0 (the 2 most important factors)
aps_failure_data_clean4 <- aps_failure_data_clean2[, c(1, 23, 72)]

#removing Na's
aps_failure_data_clean4 <- na.omit(aps_failure_data_clean4)

#splitting the clean data set into train/test sets using 80% for training and 20% for testing
create_train_test3 <- function(aps_failure_data_clean4, size = 0.7, train = TRUE) {
  n_row = nrow(aps_failure_data_clean4)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (aps_failure_data_clean4[train_sample, ])
  } else {
    return (aps_failure_data_clean4[-train_sample, ])
  }
}
data_train3 <- create_train_test3(aps_failure_data_clean4, 0.7, train = TRUE)
data_test3 <- create_train_test3(aps_failure_data_clean4, 0.7, train = FALSE)


data_train3$class = as.factor(data_train3$class)
# Building the tree by following the code from the GitHub link provided
ti_tree =
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  fit(class ~ bj_000 + am_0, data = data_train3)
ti_tree <- na.omit(ti_tree)

## Plot the data and model partitions
data_train3 %>%
  ggplot(aes(x=bj_000, y=am_0)) +
  geom_jitter(aes(col=class), alpha=0.7) +
  geom_parttree(data = ti_tree, aes(fill=class), alpha = 0.1) +
  theme_minimal()
