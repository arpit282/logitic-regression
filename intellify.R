###########################################################################
###############          Logistic regression                 ###########


setwd("E:/R (Data Science)/excel files")


data <- read.csv("binary.csv")

str(data)

data$admit <- as.factor(data$admit)
data$rank <- as.factor(data$rank)


############ training and testing data #############
set.seed(123)
id <- sample(2,nrow(data),replace = T,prob = c(.8,.2))

train <- data[id == 1, ]
test <- data[id ==2,]


##### Model ######

model <-glm(admit ~ gpa+rank,train,family = "binomial")

summary(model)

# prediction training dataset
p1<-predict(model,train,type="response")
head(p1)
head(train)

# misclassification train
pred1 <- ifelse(p1>.5,1,0)
tab1 <- table(prediction=pred1,actual=train$admit)
tab1

1-sum(diag(tab1))/sum(tab1)


# misclassification on test dataset
p2 <-predict(model,test,type="response")
head(p2)
head(test)


pred2 <- ifelse(p2>.5,1,0)
tab2 <- table(predication =pred2,actual = test$admit)
tab2


1-sum(diag(tab2))/sum(tab2)


#######################  K-Nearest Neighbour (KNN)  #####################3


data <- read.csv("Prostate_Cancer.csv")

data <- data[,-1]

str(data)

t <-table(data$diagnosis_result)
pie(t)


# Normalisation 

normalise <- function(x){
  return( (x - min(x)) / (max(x)- min(x)))     ### 0-1
}


data_n <- as.data.frame(lapply(data[2:9], normalise))

### training and test set 
train <- data_n[1:70,]
test <- data_n[71:100,]


### train and test labels
train_lab <- data[1:70,1]
test_lab <- data[71:100,1]
table(test_lab)

### Knn 
install.packages("class")
library(class)

k <- sqrt(nrow(data))  #### K is determined by square root of no . of rows

model <- knn(train = train,test = test,cl = train_lab, k= 9)
table(model)


t1 <- table(Prediction =model, Actual=test_lab)
sum(diag(t1))/sum(t1)






















