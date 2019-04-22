#install.packages("randomForest")

library(randomForest)
#the heart disease data set.
# http://archive.ics.uci.edu/ml/datasets/Heart+Disease

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

df_heart_dis <- read.csv(url, header=FALSE)

head(df_heart_dis) # data has no column names


#taking column names from the UCI data set description
colnames(df_heart_dis) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar greater than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)


head(df_heart_dis) # now we have data and column names


str(df_heart_dis)  #shows structure of our data
#note that there are "?" values

#################################  tidying data  #############################
## replacing "?"s with "NA"s.
df_heart_dis[df_heart_dis == "?"] <- NA

#making factors for some variables

df_heart_dis$sex <- ifelse(test=df_heart_dis$sex == 0, yes="F", no="M")
df_heart_dis$sex <- as.factor(df_heart_dis$sex)

df_heart_dis$cp <- as.factor(df_heart_dis$cp)
df_heart_dis$fbs <- as.factor(df_heart_dis$fbs)
df_heart_dis$restecg <- as.factor(df_heart_dis$restecg)
df_heart_dis$exang <- as.factor(df_heart_dis$exang)
df_heart_dis$slope <- as.factor(df_heart_dis$slope)

df_heart_dis$ca <- as.integer(df_heart_dis$ca) 
df_heart_dis$ca <- as.factor(df_heart_dis$ca)  
df_heart_dis$thal <- as.integer(df_heart_dis$thal)
df_heart_dis$thal <- as.factor(df_heart_dis$thal)

df_heart_dis$hd <- ifelse(test=df_heart_dis$hd == 0, yes="Healthy", no="Unhealthy")
df_heart_dis$hd <- as.factor(df_heart_dis$hd) 

str(df_heart_dis) 
#columns to be made factors are done!


########################################################

#Now we are ready to build a random forest.

set.seed(42)

# impute any missing values in the training set using proximities
data.imputed <- rfImpute(hd ~ ., data = df_heart_dis, iter=6)
#iter = the number of iterations to run. Breiman says 4 to 6 iterations.


#Our variable to be predicted is heart disease.
rf_model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)
rf_model
#OOB error = 16.83

rf_model <- randomForest(hd ~ ., data=data.imputed, ntree=300, proximity=TRUE)
rf_model
#changing number of trees doesnot affect OOB error much 

rf_model <- randomForest(hd ~ ., data=data.imputed, mtry= 8, ntree=300, proximity=TRUE)
rf_model
#increasing number of variable at each split increases OOB error, the default was optimal.


########################################################