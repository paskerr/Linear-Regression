####Question1####
library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
class(x)

lda=train(x,y,method="lda")
lda #accuracy is 0.8946508

####Question2####
#In this case, LDA fits two 10-dimensional normal distributions. 
#Look at the fitted model by looking at the finalModel component of the result of train(). 
#Notice there is a component called means that includes the estimated means of both distributions. 
#Plot the mean vectors against each other and determine which predictors (genes) appear to be driving the algorithm.

#Which TWO genes appear to be driving the algorithm (i.e. the two genes with the highest means)?

lda$finalModel

as.data.frame(t(lda$finalModel$means))%>%
  ggplot(aes(cerebellum,hippocampus,label=colnames(lda$finalModel$means)))+
  geom_point() +
  geom_text() +
  geom_abline()


####Question3####
#repeat the exercise in Q1 with QDA

qda=train(x,y,method="qda")
qda #accuracy is 0.7976565

####Question4####
#Which TWO genes drive the algorithm when using QDA instead of LDA (i.e. the two genes with the highest means)?

as.data.frame(t(qda$finalModel$means))%>%
  ggplot(aes(cerebellum,hippocampus,label=colnames(qda$finalModel$means)))+
  geom_point() +
  geom_text() +
  geom_abline()

####Question5####
colMeans(x)
x
#Re-run LDA with preProcess = "center".
lda=train(x,y,method="lda",preProcess="center")
as.data.frame(t(lda$finalModel$means))%>%
  ggplot(aes(cerebellum,hippocampus,label=colnames(lda$finalModel$means)))+
  geom_point() +
  geom_text() +
  geom_abline()


####Question6####

# Repeat the LDA analysis from Q5 but using all tissue types?

library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

lda=train(x,y,method="lda",preProcess="center")
lda






