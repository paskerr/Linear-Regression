#Q1 what is the P(disease|test+)

#test+|disease)= 0.85
#P(test-|healthy)= 0.90
#P(test+|healthy)=1-P(test-|healthy)= 0.10
#P(disease)= 0.02
#P(healthy)=1-P(disease)= 0.9
#P(disease|test+)=P(test+|disease)*P(disease)/P(test+|disease)*P(disease)+P(test+|healthy*P(healthy))

0.85*0.02/(0.85*0.02+0.10*0.98)

#Q2 probability of  positive test                

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
mean(test)

#Q3 probability of negative test given disease P(test-|disease)
mean(disease[test==0])

#Q4 probability of disease given test positive P(disease|test-)
mean(disease[test==1])

#Q5 If a patient's test is positive, by how many times does that increase their risk of having the disease?
mean(disease[test==1])/mean(disease)







