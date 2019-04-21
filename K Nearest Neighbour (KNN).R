library(ISLR) # for data set

library(class) # for KNN

# This data set includes 85 predictors that measure demographic characteristics for 5,822 individuals. The response variable is
#Purchase , which indicates whether or not a given individual purchases a
#caravan insurance policy

dim(Caravan) 
attach(Caravan)
data(Caravan)
summary(Purchase)
348/5822   # only 6% of people actually purchase insurance


# We need standardized data for our knn model

standardized.X=scale(Caravan[,-86])

test <- 1:1000

train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)


#testing for different values of k and looking at precision.
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(test.Y, knn.pred)
5/26

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(test.Y, knn.pred)
4/15

knn.pred=knn(train.X,test.X,train.Y,k=7)
table(test.Y, knn.pred)
2/7
