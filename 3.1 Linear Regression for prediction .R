library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#####Question1 ############################################

#Build 100 linear models using dat data frame above. 
#calculate the standard deviation and means of the RMSEs from all 100 models that we build. 


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
linear<-replicate(100,{
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat%>% slice(test_index)
fit <- lm(y ~ x, data = train_set)
y_hat <- predict(fit, test_set)
RMSE_s<-mean((y_hat - test_set$y)^2)
})
mean(sqrt(linear))
sd(sqrt(linear))

##########Q2######################3

Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)

f1=function(n){
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

linear<-replicate(100,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat%>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  RMSE_s<-mean((y_hat - test_set$y)^2)
})
print(n)
print(mean(sqrt(linear)))
print(sd(sqrt(linear)))

}

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later   
n <- c(100, 500, 1000, 5000, 10000)
sapply(n,f1)

#####Q3 how RMSE changes as the data size increases?################################################################

#variability of the RMSE decreases since the n increases

#####Q4 change the correlation between x and y ; calculate the mean of RMSE and sd of RMSE again from Q1#########################3

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
linear<-replicate(100,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat%>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  RMSE_s<-mean((y_hat - test_set$y)^2)
})
mean(sqrt(linear))
sd(sqrt(linear))

#####Q6 ##############################################################################

#Create a data set using the following code.

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
data.frame() %>% setNames(c("y", "x_1", "x_2"))

#set seed 1
#use caret package and partition data into test and training with p=0.5
#train a single linear model
# calculate the RMSE using just x_1
#calculate the RMSE using just x_2
#calculate the RMSE using both x_1 and x_2 
#compare the RMSEs

#data partition
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat%>% slice(test_index)

#just using x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
RMSE 

#just using x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
RMSE


#using both x_1 and x_2
fit <- lm(y ~ x_1+ x_2, data = train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
RMSE

#####Q7 what is the lowest RMSE form Q6###################################################################

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
fit <- lm(y ~ x_1+ x_2, data = train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
RMSE

####Q8 repeat the Q6 with highly correlated x_1 and x_2 as below.##################################

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
data.frame() %>% setNames(c("y", "x_1", "x_2"))


#data partition 
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat%>% slice(test_index)

#just x_1
fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
RMSE 

#just x_2
fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
RMSE

#both x_1 and x_2
fit <- lm(y ~ x_1+ x_2, data = train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
RMSE































































