##Question1####
#How many times do 3, 4, and 7 appear in the first resampled index?

library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes[[1]]==3) #1
sum(indexes[[1]]==4) #4
sum(indexes[[1]]==7) #0


##Question2####
#What is the total number of times that 3 appears in all of the resampled indexes?

three=sapply(1:10,function(x){indexes[[x]]==3})

sum(three) #11

##Question3#####

y <- rnorm(100, 0, 1)

Estimate the 75th quantile, which we know is qnorm(0.75), with the sample quantile: quantile(y, 0.75)?
  
set.seed(1,sample.kind = 'Rounding')
x=replicate(10000,{y <- rnorm(100, 0, 1)
quantile(y,0.75)

})
mean(x)
sd(x)

##Question4####

#In practice, we can't run a Monte Carlo simulation. Use the sample:

# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

#Set the seed to 1 again after generating y and use 10 bootstrap samples to estimate the expected value and standard error 
#of the 75th quantile.?

set.seed(1,sample.kind = "Rounding")
index=createResample(y,10)

q=sapply(index,function(x){
  quantile(y[x],0.75)
})
mean(q)
sd(q)

####Question5#####
#Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1 first

set.seed(1,sample.kind = "Rounding")
index=createResample(y,10000)

q=sapply(index,function(x){
  quantile(y[x],0.75)
})
mean(q)
sd(q)





