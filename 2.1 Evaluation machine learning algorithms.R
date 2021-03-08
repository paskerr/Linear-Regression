
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)
install.packages("e1071")

#load the reported_heights dataset

data(reported_heights)
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Q1 The type column of dat indicates whether students took classes in person ("inclass") or online ("online"). What proportion of the inclass group is female? What proportion of the online group is female?
dat%>%group_by(type,sex)%>%summarize(n=n())%>%summarise(sex,freq=n/sum(n))%>%spread(type,freq)


#Q2 Prediction of sex based on type.
y_hat <- factor(ifelse(dat$type == 'inclass', "Female", "Male"),c("Female","Male"))
accuracy=mean(y == y_hat)

#Q3 Confussion matrix
table(y_hat, y)

#Q4-Q5-Q6 Sensitivity,specificity,prevalence

confusionMatrix(y_hat,y)


#load the iris data set; keep only versicolor and virginica iris species
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Q7 split the data into train and test partitions

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#Q8 which feature produces the highest accuracy?

cutoff=seq(min(train[1:4]),max(train[1:4]),0.1)
vector=NA
for (i in 1:4) {
accurate=map_dbl(cutoff, function(x){
  y_hat <- ifelse(train[i] > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})

vector[i]=max(accurate)
}
vector
colnames(train[which.max(vector)])

#Q9 for the selected feature and cutoff value, what is the overall accuracy in the test data?

accurate=map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length> x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
which.max(accurate)
cutoff[38]

y_hat_test <- ifelse(test$Petal.Length> 4.7, "virginica", "versicolor")
mean(y_hat_test == test$Species)

#Q10 use the test data instead of train data and repeat Q8

cutoff=seq(min(test[1:4]),max(test[1:4]),0.1)
vector=NA
for (i in 1:4) {
  accurate=map_dbl(cutoff, function(x){
    y_hat <- ifelse(test[i] > x, "virginica", "versicolor")
    mean(y_hat == test$Species)
  })
  
  vector[i]=max(accurate)
}
vector
colnames(test[which.max(vector)])

#Q11

plot(iris,pch=21,bg=iris$Species)

##find the best cutoff for "pedal.width"
accurate=map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width> x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
which.max(accurate) ## 6th cutoff is the best cutoff
cutoff[6]  ## the best value is 1.5 for 'Petal.Width"

## We found the best cutoff for Petal Length is 4.7 in Q8

##Overall Accuracy
y_hat <- ifelse(test$Petal.Length> 4.7 | test$Petal.Width>1.5, "virginica", "versicolor")
mean(y_hat == test$Species)



















