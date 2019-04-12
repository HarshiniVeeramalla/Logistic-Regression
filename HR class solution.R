

#-------Importing the data---------

setwd("D:\\jigsaw\\Regression\\11.3")

hr<-read.csv("HR dataset for Employee Attrition.csv")

# Data Exploration

dim(hr)
str(hr)
View(hr)

# Convert Attrition to numeric variable

hr$AttritionTarget <- as.numeric(hr$Attrition)-1    # here if u check str r has taken 2,1 values to change it to 0,1 we use -1
View(hr)

# What is the ratio of Attrited vs. not Attritted?

# Frequency Distribution of the binary dependent variable 

table(hr$AttritionTarget)
table(hr$AttritionTarget)/nrow(hr)

#Checking for missing values in all the columns

colSums(is.na(hr))

# No missing values

# Partition the dataset into training and validation dataset

sampling<-sort(sample(nrow(hr), nrow(hr)*.7)) # select from sample and run to see what are the rows selected

length(sampling)

#Row subset and create training and validation samples using the index numbers

train<-hr[sampling,] # creating training data set
dim(train)
nrow(train)

test<-hr[-sampling,]    # creating test/validation set
dim(test)
nrow(test)

# Checking the frequency Distribution of the target variable 

table(train$AttritionTarget)
table(train$AttritionTarget)/nrow(train)    # instead of nrow we can use directly 1029
table(test$AttritionTarget)/nrow(test)              # instead of nrow can also use 441

#Renaming Age column

# renaming is done to make column names readable

colnames(train)
names(train)[1] <- "Age"    # names(train) gives column names
colnames(train)
names(test)[1] <- "Age"

#Are any of the independent variables correlated?      # checking multicolinearity

install.packages("corrplot", dependencies = T)
library(corrplot)

#Finding correlation between numeric variables

# corelation can only be done for numeric variables
# therefore taking only numeric variables

str(train)
traincor<-cor(train[,c(1,4,6,7,11,13,14,15,17,19,20,21,24,25,26,28:35)])  # column index number of train data set

install.packages("corrgram")
library(corrgram)
class(traincor)
?corrgram
cormat<-corrgram(traincor)

write.csv(cormat,"Correlation.csv")

# After Conditional formatting, we find :
# High correlation between:
# Job Level and Monthly Income
# Job Level and Total Working Years
# Monthly Income and Total Working Years
# Percent Salary Hike and Performance Rating

str(train)
colnames(train)

?glm()      

# Family of dependent variable is binary or binomial 

myresult<-glm(data=train,AttritionTarget ~ Age+BusinessTravel+
                +DailyRate+Department+DistanceFromHome+Education+
                EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+
                JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+
                NumCompaniesWorked+OverTime+PercentSalaryHike+
                RelationshipSatisfaction+StandardHours+StockOptionLevel+
                TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
                YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,family=binomial)

summary(myresult)

#Gives best fitted model
#To choose a good model

?step       # to find lowest AIC value

reduced<-step(myresult,direction="backward")   

# it will remove variables when given backward
# it will add variables when given forward


# Iteration 2: 
myresult<-glm(data=train,AttritionTarget ~  Age + BusinessTravel + Department + DistanceFromHome + 
                EnvironmentSatisfaction + JobInvolvement + JobSatisfaction + 
                MaritalStatus + MonthlyIncome + NumCompaniesWorked + OverTime + 
                RelationshipSatisfaction + TrainingTimesLastYear + WorkLifeBalance + 
                YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager)

summary(myresult)

# Creating dummy variables (for both test and train data sets)

# create dummy variables only for significant variables

train$BTF <- ifelse(train$BusinessTravel == "Travel_Frequently",1,0) # btf : business travel frequently

# as travel frequently is only significant, we only keep that as 1 otherwise frequently = 0

train$OTY <- ifelse(train$OverTime == "Yes",1,0) # oty : over time yes

# creating dummy variables for test dataset

test$BTF <- ifelse(test$BusinessTravel == "Travel_Frequently",1,0)
test$OTY <- ifelse(test$OverTime == "Yes",1,0)

#Iteration # 3:


myresult<-glm(data=train,AttritionTarget ~ BTF + EnvironmentSatisfaction + JobInvolvement + 
                JobSatisfaction + MonthlyIncome + NumCompaniesWorked + 
                OTY + YearsSinceLastPromotion,family=binomial)

summary(myresult)

# Iteration # 4


myresult<-glm(data=train,AttritionTarget ~ BTF + EnvironmentSatisfaction + JobInvolvement + 
                JobSatisfaction + MonthlyIncome +  
                OTY,family=binomial)

summary(myresult)


#Finding Predicted Values

?glm

# myresult is the name of model

# to know values of residuals, co.efficients etc you can use modelname$(select from dropbox)

myresult$fitted.values


train$predicted <- myresult$fitted.values   # here fitted values are probability where attrition is 1
train$predicted


# Compare with actual data

head(train$AttritionTarget)

head(train$predicted)    # here the values pridicted are how much probability of leaving company

# Let us convert the probabilities also into Attrited/Not Attrited 
# based on a cut-off probability   ( since we need to decide what percent would be correct to estimate)

#Confusion Matrix

train$predclass<-ifelse(train$predicted>0.5,1,0)   # if the probability is > 50% take it as 1 otherwise take it as 0
table(train$predclass,train$AttritionTarget)

# attrition target is the given or actual data and predclass is the model prediction

#True Positive+ True Negative should be high. 

# Accuracy = (TP+TN)/(P+N)

(840+36)/(840+36+136+17) # model has predicted with 85% accuracy with 50% cutoff



# For different cutoff probabilities, the confusion matrix will be different

# To find accuracies for different cut-off probabilities

# There are a lot of performance parameters available in ROCR package

install.packages("ROCR")
library(ROCR)


# The prediction function of the ROCR library basically creates 
# a structure to validate our predictions with actual values

pred<-prediction(train$predicted,train$AttritionTarget)
class(pred)


?performance

perf <- performance(pred,"acc")   
class(perf)
perf
# x values contain the cut-off probabilities

#use @ to access the slots

class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))    # unlisting and changing to numeric

cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))

cutoffs <- data.frame(cutoffprob, accuracies )   # making x,y together using data.frame

# In the decreasing order of accuracy

cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]
cutoffs

# Pick cutoff for which Accuracy is highest 

train$predclass <- ifelse(train$predicted >  0.4763888,1,0)

# Kappa values and Confusion Matrix from caret package

install.packages("caret")     # used for confusion matrix and kappa values
library(caret)
install.packages("irr")
library(irr)

kappa2(data.frame(train$AttritionTarget,train$predclass)) # kappa tells difference between observed and predicted metric

install.packages("e1071")
library(e1071)

confusionMatrix(as.factor(train$AttritionTarget),as.factor(train$predclass), positive = "1")


## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N    # (y-axis,x-axis)
plot(perf,col="red")

# Receiver Operating Characteristic Curve (ROC) a plot of TPR versus FPR 
# for the possible cut-off classification probability values.
# A good ROC curve should be almost vertical in the beginning and 
# almost horizontal in the end.
# "tpr" and "fpr" are arguments of the "performance" function 
# indicating that the plot is between the true positive rate and 
# the false positive rate.

?abline # roc should be above abline

# Draw a straight line with intercept 0 and slope = 1
# lty is the line type (dotted or dashed etc.)
# The straight line is a random chance line
# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")


# Area under the curve should be more than 50%

auc<-performance(pred,"auc")
auc

#Creating a Gains chart

install.packages("gains")

library(gains)


gains(as.numeric(train$AttritionTarget),train$predicted, groups =10)   # % of 1's in each group
quantile(train$predicted, seq(0,1,0.1))    # top 30% (70%, 80% ,90%, 100%) gives 70% of 1's 

# quantile gives the probability of people leaving the company

# ex : prob of 20% leaving company is 0.399 onwards

targeted <- which(train$predicted >= 0.185868869)

targeted

# To obtain predictions from the model, use the predict() function.

?predict()

# predict applys the model found using trsaining dataset to the test data set

# predict(model name, type, newdata = dataset name)


test$pred <- predict(myresult, type = "response",newdata = test)  # using the model on a test data set


# The value 'response' to the parameter type would make sure 
# that these predictions are returned as probability of events.

