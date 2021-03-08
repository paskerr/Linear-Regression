
####Question1####
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#Which code correctly uses rpart() to fit a regression tree and saves the result to fit?
fit <- rpart(y ~ ., data = dat) 

####Question2#####
#What is the tree shape of Question1?
plot(fit)
text(fit)

####question3####
#Below is most of the code to make a scatter plot of y versus x along with the predicted values based on the fit.
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

####Question4######
# run random forest instead of regression tree and remake scatterplot

fit=randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

####Question5#####
#plot the random forest from Q4
plot(fit)

#####Question6#####

#It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). 
#Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

  
  
  
  
  
  
  
  
  








