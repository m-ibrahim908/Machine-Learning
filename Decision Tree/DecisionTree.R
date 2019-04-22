#decision trees

library(rpart)
library(rpart.plot)
library(ROCR)

census <- read.csv("census.csv", header = TRUE)

str(census)
unique(census$education)
table(census$education)
table(census$education,  census$over50k)
set.seed(123)

#extracting 80% random data
sample_size <- floor(0.80 *nrow(census))
train_indices <- sample(seq_len(nrow(census)), size = sample_size)
train_indices

#actual splitting
train_data <- census[train_indices,]
test_data <- census[-train_indices, ]

#building tree
censusTree = rpart( over50k ~ . , method = "class", data = train_data)
censusTree

rpart.plot(censusTree)
unique(census$relationship)

censusTree$variable.importance

summary(censusTree)

#print also shows loss, 6197
print(censusTree)
path.rpart(censusTree, c(1:7))
6
#testing
predictTest <- predict(censusTree, newdata = test_data, type = "class")
predictProb <- predict(censusTree, newdata = test_data)

table(predictTest, test_data$over50k)

#calculating accuracy: (4689+745)/ (4689)
