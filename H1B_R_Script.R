## (Casey) Juanxi Li
## STAT471 Final Project
## December 21, 2014

#------------------------------------------------------------------------
# Data Prep and Cleanup
#------------------------------------------------------------------------

setwd("C:/Users/Casey/Desktop/STAT471/Enigma Data")
data = read.csv("2014.csv", header=TRUE)
head(data)
summary(data)

## Remove levels other than Certified or Denied

data = data[-grep("CERTIFIED-WITHDRAWN", data$status),]
data = data[-grep("INVALIDATED", data$status),]
data = data[-grep("REJECTED", data$status),]
data = data[-grep("WITHDRAWN", data$status),]

levels(data$status)
data = droplevels(data)

write.csv(data, file = "2014_cleaned.csv")

table(data$status)

a = table(data$status)

cert_count = a[1]
cert_count

denied_count = a[2]
denied_count

# ---------------------------------------------------
# Undersampling to obtain a balanced dataset
#----------------------------------------------------

set.seed(1)
random = runif(denied_count)

undersample = as.integer(random * cert_count)

undersample

all_denied = data[which(data$status=="DENIED"),]
all_denied
dim(all_denied)

all_certified = data[which(data$status=="CERTIFIED"),]
all_certified
dim(all_certified)

undersample_certified = all_certified[undersample,]
undersample_certified
dim(undersample_certified)

fixed_set = rbind(all_denied, undersample_certified)
fixed_set
dim(fixed_set)
head(fixed_set)

write.csv(fixed_set, file = "2014_fixed.csv")

#---------------------------------------------------------
# Handcleaning and rearrangement of this .csv happened in Excel
# --------------------------------------------------------

data2 = read.csv("2014_fixed_handcleaned_condensed.csv")
head(data2)
summary(data2)
str(data2)

data3 = data2[,1:16]
str(data3)

# remove unworkable columns, fix data types

data4 = data3[,-c(4,6,9,10,14)]
str(data4)


data4$YEAR_SUBMITTED = as.factor(data4$YEAR_SUBMITTED)
data4$MONTH_SUBMITTED = as.factor(data4$MONTH_SUBMITTED)
data4$DECISION_MONTH = as.factor(data4$DECISION_MONTH)

data4$YEARLY_WAGE_CALC = as.numeric(as.character(data4$YEARLY_WAGE_CALC))
data4$PW_YEARLY_CALC = as.numeric(as.character(data4$PW_YEARLY_CALC))

data4$YEARLY_WAGE_CALC
data4$PW_YEARLY_CALC

mean(data4$PW_YEARLY_CALC, na.rm=T)
median(data4$PW_YEARLY_CALC, na.rm=T)

## Remove submitted or salaries above $200,000: they make up little of the set, 
## and could too easily
## be the result of a calculation error

data4 = data4[which(data4$YEARLY_WAGE_CALC < 200000),]
data4 = data4[which(data4$PW_YEARLY_CALC < 200000),]

# remove rows with missing data

data5 = data4[complete.cases(data4),]

str(data5)

dim(data5)

# Check that data is balanced
table(data5$status)

## Collinearity:

plot(data5$YEARLY_WAGE_CALC, data5$PW_YEARLY_CALC)
plot(as.numeric(data5$MONTH_SUBMITTED), as.numeric(data5$DECISION_MONTH))


# -------------------------------------------------------
# Some basic plots to see if anything obvious is going on
#--------------------------------------------------------

library(ggplot2)

#### 2013 vs 2014:

ggplot(data5, aes(x=data5$YEAR_SUBMITTED, fill=data5$status)) + 
  geom_histogram(binwidth=1, alpha=.5, position="dodge")

#### Full-time vs part-time:

ggplot(data5, aes(x=data5$full_time_pos, fill=data5$status)) + 
  geom_histogram(binwidth=1, alpha=.5, position="dodge")

#### Histogram of days requested:

ggplot(data5, aes(x=data5$DAYS_REQUESTED, fill=data5$status)) + 
  geom_histogram(binwidth=100, alpha=.5, position="dodge")

#### Histogram of month submitted:

ggplot(data5, aes(x=data5$MONTH_SUBMITTED, fill=data5$status)) + 
  geom_histogram(binwidth=1, alpha=.5, position="identity")

# Spike in March due to the fact that April 1 applications and onwards count towards
# the fiscal year's cap (October)


#### Histogram of DECISION_MONTH:

ggplot(data5, aes(x=data5$DECISION_MONTH, fill=data5$status)) + 
  geom_histogram(binwidth=1, alpha=.5, position="identity")

# Everyone is rushing for the April 1st, 2014 deadline to count towards the 2015 cap


#### Histogram of DAYS_TIL_DECISION:

ggplot(data5, aes(x=data5$DAYS_TIL_DECISION, fill=data5$status)) + 
  geom_histogram(binwidth=1, alpha=.5, position="identity") +
  geom_vline(aes(xintercept=mean(data5$DAYS_TIL_DECISION, na.rm=T)), color="red", linetype="solid", size=0.5)

mean(data5$DAYS_TIL_DECISION, na.rm=T) # 6.77


#### Histogram of YEARLY_WAGE_CALC:

ggplot(data5, aes(x=data5$YEARLY_WAGE_CALC, fill=data5$status)) + 
  geom_histogram(binwidth=1000, alpha=.5, position="identity")


#### Histogram of PW_YEARLY_CALC:

ggplot(data5, aes(x=data5$PW_YEARLY_CALC, fill=data5$status)) + 
  geom_histogram(binwidth=1000, alpha=.5, position="identity")


#------------------------------------------
# OKAY TIME TO FIT A CLASSIFICATION MODEL
#------------------------------------------

## Remove PW_YEARLY_CALC and MONTH_SUBMITTED since they are colinear with
## YEARLY_WAGE_CALC and DECISION_MONTH, respectively

fit1 = glm(status~.-YEARLY_WAGE_CALC -DECISION_MONTH, data = data5, family="binomial")
summary(fit1)

dim = dim(data5)
dim = dim[1]

fit1.pred=rep("CERTIFIED", nrow(data5))   # prediction step 1
fit1.pred[fit1$fitted > 0.5]="DENIED"  #prediction step 2 to get a classifier
fit1.pred

cm=table(data5$status, fit1.pred) # confusion matrix
cm

training_error = (cm[1,2] + cm[2,1])/nrow(data5)
training_error # 33%, not very good

table(data5$status)


## ROC curves of individual predictors:
----------------------------------------

library(pROC)
plot(roc(data5$status, data5$PW_YEARLY_CALC)) #.5607
plot(roc(data5$status, data5$YEARLY_WAGE_CALC)) #.5974

plot(roc(data5$status, as.numeric(data5$DECISION_MONTH))) #.5216
plot(roc(data5$status, as.numeric(data5$MONTH_SUBMITTED))) #.5123

plot(roc(data5$status, data5$DAYS_TIL_DECISION)) #.6211
plot(roc(data5$status, data5$DAYS_REQUESTED)) #.4636


## Can I get a better fit by taking just the features with better ROC curves?
------------------------------------------------------------------------

fit2 = glm(status~DAYS_TIL_DECISION+YEARLY_WAGE_CALC, data = data5, family="binomial")
summary(fit2)

dim = dim(data5)
dim = dim[1]

fit2.pred=rep("CERTIFIED", nrow(data5))   # prediction step 1
fit2.pred[fit2$fitted > 0.5]="DENIED"  #prediction step 2 to get a classifier
fit2.pred

cm=table(data5$status, fit2.pred) # confusion matrix
cm

training_error = (cm[1,2] + cm[2,1])/nrow(data5)
training_error

#-------------
## 41% training error. Nope.
#-------------

#--------
# RIDGE
#--------

library(MASS)
lm.ridge(data5$status~., data=data5, model=TRUE, lambda = seq(0,1,0.1))

#------------
# Lasso
#-----------

library(glmnet)

X=model.matrix(status~., data5)[, -1]
Y=data5[,1]

head(X)
head(Y)

fit.lasso.lambda=glmnet(X, Y, alpha=1)    
summary(fit.lasso.lambda)
fit.lasso.lambda$beta

## Not sure why this isn't working :(


## Conclusion: High error rate indicates that the non-text features of the H1-B
## application are not terribly useful for prediction. Ideally I would repeat this
## several times using different undersamples, but time and computational power
## prevent me from doing so (computer crashes when I try to work with the full dataset).




## ------------------------------------------------------
## Next step: Text Classification
## ------------------------------------------------------

words = read.csv("2014_words.csv")
head(words)
str(words)

library(RTextTools)

# ADD TEXTUAL DESCRIPTORS FOR EACH FORM FIELD FOR THE DOCUMENT-TERM MATRIX
employer = as.vector(apply(as.matrix(words[2], mode="character"),1,paste,"employer",sep="",collapse=""))
name = as.vector(apply(as.matrix(words[3], mode="character"),1,paste,"name",sep="",collapse=""))
title = as.vector(apply(as.matrix(words[4], mode="character"),1,paste,"title",sep="",collapse=""))



##---------------------
## Zip code information
##---------------------



