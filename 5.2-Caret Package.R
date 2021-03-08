####Question1####
library(rpart)
library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1991,sample.kind = "Rounding")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)
max(fit$results$Accuracy)

####Question2#######

set.seed(1991,sample.kind = "Rounding")
fit_r <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",control = rpart.control(minsplit = 0),
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
####Question3####
#Which gene is the first split?
set.seed(1991,sample.kind = "Rounding")
fit1 <- with(tissue_gene_expression,rpart(y ~ x,rpart.control(cp=0)))
plot(fit1)
text(fit1)
#or

plot(fit$finalModel)
text(fit$finalModel)

####Question4####
library(randomForest)
set.seed(1991,sample.kind = "Rounding")
fit=with(tissue_gene_expression, train(x,y,method="rf",tuneGrid=data.frame(mtry=seq(50,200,25)),nodesize=1 ))
ggplot(fit)

#####Question5#####
varImp(fit)

#####Question6####

tree_terms <- as.character(unique(fit_r$finalModel$frame$var[!(fit_r$finalModel$frame$var == "<leaf>")])) # for the fit in Question 2
tree_terms

#what is the importance of the CFHR4 in random forest
varImp(fit)
#what is the rank of the CFHR4 gene in the random forest




