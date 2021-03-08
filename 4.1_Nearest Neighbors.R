####Comprehension Check:Distance####

####Question1####
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

#Which of the following lines of code computes the Euclidean distance between each observation and stores it in the object d?
d <- dist(tissue_gene_expression$x)

####Question2####

d <- dist(tissue_gene_expression$x)
as.matrix(d)[c(1,2,39,40,73,74), c(1,2,39,40,73,74)]

####Question3####
#Make a plot of all the distances using the image()
image(as.matrix(d))

####Comprehension Check:Nearest Neighbors####

####Question1####

#find the max F1 scores with the k values of seq(1, 101, 3).

library(caret)
library(dslabs)
library(dplyr)
set.seed(1,sample.kind = "Rounding")
index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train <-heights %>% slice(-index)
test <-heights %>% slice(index)

ks <- seq(1, 101, 3)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train, k = k)
  y_hat <- predict(fit, train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference =train$sex)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit,test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test$sex)
  test_error <- cm_test$overall["Accuracy"]
  
  f1=F_meas(data=y_hat,reference = factor(test$sex))
  tibble(train = train_error, test = test_error,f_score=f1)
})

max(accuracy$f_score)
ks[which.max(accuracy$f_score)]

####Question2####
library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1,sample.kind = "Rounding")

index=createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)

index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train <-heights %>% slice(-index)
test <-heights %>% slice(index)?????









