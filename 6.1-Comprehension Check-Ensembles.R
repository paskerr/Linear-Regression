#Comprehension Check : Ensembles##############################

#we use mnist_27 dataset

#Question1#########

# we will use the models below.

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

#run the following code to train the various models

library(caret)
library(dslabs)
library(tidyverse)
library(dplyr)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


##Question2###############

#Now that you have all the trained models in a list, use sapply() or map() to create a matrix of predictions for the test set. 
#You should end up with a matrix with length(mnist_27$test$y) rows and length(models) columns.

y_hat=sapply(fits, function(model){predict(model,mnist_27$test)})
  
  

#What are the dimensions of the matrix of predictions?

#number of rows?
nrow(y_hat)

#number of colums?
ncol(y_hat)


##Question3##############

y_all=sapply(seq(1,10,1),function(x){confusionMatrix(as.factor(y_hat[,x]),mnist_27$test$y)$overall["Accuracy"]})
mean(y_all)  
dim(y_hat)
#in the solution### 
acc <- colMeans(y_hat == mnist_27$test$y)
acc
mean(acc)
###

##Question4######
#Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble. 
#Vote 7 if more than 50% of the models are predicting a 7, and 2 otherwise.

y_hat=as.data.frame(y_hat)
y_hat_ensemble=y_hat%>%mutate(ensemble=0)
ff=function(x) {
  y_hat_ensemble[x,"ensemble"]=ifelse(sum((y_hat_ensemble[x,])=="7")>sum((y_hat_ensemble[x,])=="2"),7,2)
} 
ensemble=sapply(X=1:200,FUN=ff)                          
mean(ensemble==mnist_27$test$y)                          

#in the solution
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Question5####

#In Q3, we computed the accuracy of each method on the test set and noticed that the individual accuracies varied.

#How many of the individual methods do better than the ensemble?

sum(y_all>0.815)

#Which individual methods perform better than the ensemble?
index=which(y_all>0.815)
models[index]


#Question6####

#It is tempting to remove the methods that do not perform well and re-do the ensemble. 
#The problem with this approach is that we are using the test data to make a decision. 
#However, we could use the minimum accuracy estimates obtained from cross validation with the training data for 
#each model from fit$results$Accuracy. Obtain these estimates and save them in an object. Report the mean of these training 
#set accuracy estimates.

#What is the mean of these training set accuracy estimates?
y_min=sapply(fits,function(fits) min(fits$results$Accuracy))
mean(x)
#Qeustion7####

#Now let's only consider the methods with a minimum accuracy estimate of greater than or equal to 0.8 when constructing the ensemble. 
#Vote 7 if 50% or more of those models are predicting a 7, and 2 otherwise.

#What is the accuracy of the ensemble now?










