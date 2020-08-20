library(adabag)
library(caret)

# Preparing data

indexes=createDataPartition(iris$Species, p=.90, list = F)
train = iris[indexes, ]
test = iris[-indexes, ]

# Classification with boosting

# The 'boosting' function applies the AdaBoost.M1 and SAMME algorithms using classification trees. 
# A 'boos' is a bootstrap uses the weights for each observation in an iteration if it is TRUE. 
# Otherwise, each observation is used with its weight. A 'mfinal' is the number of iterations or trees to use.

model = boosting(Species~., data=train, boos=TRUE, mfinal=50)

print(names(model))

print(model$trees[1])

pred = predict(model, test)

print(pred$confusion)

print(pred$error)

result = data.frame(test$Species, pred$prob, pred$class)
print(result)

# Classification with boosting.cv
cvmodel = boosting.cv(Species~., data=iris, boos=TRUE, mfinal=10, v=5)

# Check the accuracy.

print(cvmodel[-1])

data.frame(iris$Species, cvmodel$class)



