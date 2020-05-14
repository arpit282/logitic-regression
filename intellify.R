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

# misclassification on train
pred1 <- ifelse(p1>.5,1,0)
tab1 <- table(prediction=pred1,actual=train$admit)
tab1

1-sum(diag(tab1))/sum(tab1)


# prediction on test dataset
p2 <-predict(model,test,type="response")
head(p2)
head(test)

#misclassification on test
pred2 <- ifelse(p2>.5,1,0)
tab2 <- table(predication =pred2,actual = test$admit)
tab2


1-sum(diag(tab2))/sum(tab2)

























