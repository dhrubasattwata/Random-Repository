# ---
# title: "Neural Network"
# author: "Dhrubasattwata Roy Choudhury"
# ---
install.packages("keras")

### Load the packages
library(ISLR)
library("caret")
library(tidyverse)
library("keras")
library(neuralnet)
library(Hmisc)
library(caret)

#Dataset

attach(Carseats)
data("Carseats")
names(Carseats)

# Normalizing the Dataset

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data <- Carseats[complete.cases(Carseats), ]
data <- data %>% select_if(is.numeric)
dfNorm <- as.data.frame(lapply(data, normalize))

# Test-Train Split

n = nrow(Carseats)
smp_size <- floor(0.8 * n) # 
set.seed(123) 
index<- sample(seq_len(n),size = smp_size)
train <- dfNorm[index,]
test <- dfNorm[-index,]

## Neural Network Training
# Selecting the best correlated variables

corrTrain <- cor(train) # Select best 5 correlated values.
library(corrplot)
corrplot(corrTrain, method= "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Building the Neural Network Model
nn=neuralnet(Sales~Income+Advertising+Population+Price+Age,data=train, hidden=10,act.fct = "logistic",
             linear.output = TRUE,stepmax=10^5,threshold = 0.01)

plot(nn, rep = "best")

nn=neuralnet(Sales~Income+Advertising+Population+Price+Age,data=train, hidden=c(7,6,5),act.fct = "logistic",
             linear.output = TRUE,stepmax=10^5,threshold = 0.01)
plot(nn, rep = "best")


### Prediction and Performance Metrics

Predict=compute(nn,test)
cbind(test$Sales,Predict$net.result)


RMSE <- function(actual,predicted) {
  return(sqrt(sum(actual^2-predicted^2)/length(actual)))
}
Predict <- is.numeric(Predict)
RMSE(test$Sales,Predict)


