##


#clear the environemnt

rm(list = ls(all=TRUE))


## Set Working Directory to Relevant Folder##

getwd()
setwd("D:/course/Rstart/CUTE")


## read the file

incgrp<-read.csv("train_data.csv",header = T) ## Read train data into variable incgrp
#######################################
## Basic data analysis ##
########################################

str(incgrp) ## Checking data structure

summary(incgrp) ## Summarizing data

#############################################
#Data viewwing
#############################################
library(ggplot2)

plot(x=incgrp$age,y =incgrp$financial_weight ,xlab = "age",ylab="financial_weight",main= "continuous v/s continuous")

hist(incgrp$financial_weight, main="Histogram", xlab="Financial_weight", ylab="frequency", border="blue", 
     col="green", 
     las=1, 
     breaks=20, 
     prob = FALSE)

boxplot(incgrp$financial_weight~incgrp$working_sector, main = "Fin Wt by Working Sector")

boxplot(incgrp$financial_weight~incgrp$qualification, main = "Fin Wt by Qualification")

boxplot(incgrp$financial_weight~incgrp$marital_status, main = "Fin Wt by Marital Status")

boxplot(incgrp$financial_weight~incgrp$ethnicity, main = "Fin Wt by Ethnicity")

boxplot(incgrp$financial_weight~incgrp$relationship, main = "Fin Wt by Relationship")

boxplot(incgrp$financial_weight~incgrp$occupation, main = "Fin Wt by Occupation")

boxplot(incgrp$financial_weight~incgrp$country, main = "Fin Wt by Country")

boxplot(incgrp$financial_weight~incgrp$gender, main = "Fin Wt by Gender")



##############################################
#PREProcessing
##############################################

## Preliminary Observations & Changes ##

## 1 - Loan taken is integer. Can be classified as Factor

incgrp$loan_taken<-as.factor(incgrp$loan_taken)

## 2 - Tax Paid has 92.4% NAs. Imputation not recommended. Column can be removed.

rem<-c("tax_paid") ## Assigning columns to be removed to variable 'rem'

validcols <- setdiff(x=colnames(incgrp),y=rem) ## Getting list of valid columns

incgrptemp<-incgrp[,validcols] ##Removing column(s) in variable 'rem' and assigning to temp variable

incgrp<-incgrptemp ## Assigning temp values to original variable

rm(incgrptemp) ## Remove temp variable

rm(rem) ## Remove variable 'rem' since it is not required anymore

rm(validcols) ## Remove variable 'validcols' since it is not required anymore

## The variable index can be moved to rownames ##

rownames(incgrp)<-incgrp$index ## Assigning values in variable index to rownames

incgrp$index<-NULL ## Converting index to NULL & hence removing it

## Checking Data Again ##

str(incgrp)

summary(incgrp)

## Grouping Levels - Qualification ##

## Order of education level - Preschool < 1st-4th < 5th-6th < 7th-8th < 9th < 10th < 11th < 12th 
##< HS-grad < Prof-school < Assoc-acdm < Assoc-voc < Some-college < Bachelors < Masters < Doctorate

table(incgrp$qualification,incgrp$target) ## Checking count of target value by qualification

## From the table above, it can be deduced that there is a lower proportion of High Income Grp
## people when education level < HS-Grad

## Therefore, combining everything before HS - Grad into one category "LeftStudy"

incgrptemp<-incgrp ## Assigning incgrp to temp variable incgrptemp

table(incgrptemp$qualification) ## Frequency table of all columns

incgrptemp$qualification<-as.character(incgrptemp$qualification) ## Changing to character

incgrptemp$qualification <- gsub("Preschool","LeftStudy",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification <- gsub("10th","LeftStudy",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification <- gsub("11th","LeftStudy",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification <- gsub("12th","LeftStudy",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification <- gsub("1st-4th","LeftStudy",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification <- gsub("5th-6th","LeftStudy",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification <- gsub("7th-8th","LeftStudy",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification <- gsub("9th","LeftStudy",incgrptemp$qualification,fixed = TRUE)

table(incgrptemp$qualification,incgrptemp$target) ## Assoc-acdm & Assoc-voc have similar 0,1 proportion

incgrptemp$qualification<-gsub("Assoc-acdm","Associate",incgrptemp$qualification,fixed = TRUE)
incgrptemp$qualification<-gsub("Assoc-voc","Associate",incgrptemp$qualification,fixed = TRUE)

table(incgrptemp$qualification,incgrptemp$target)

incgrptemp$qualification<-as.factor(incgrptemp$qualification) ## Changing back to factor

incgrp<-incgrptemp ## Reassigning incgrptemp values to incgrp

rm(incgrptemp) ## Removing temp variable

## Grouping Levels Working Sector ##

table(incgrp$working_sector,incgrp$target) ## Frequency table of target by working sector

## Not worked and Without Pay categories can be merged since they show similar behavior

incgrptemp<-incgrp ## Creating temp variable with all data from incgrp

incgrptemp$working_sector<-as.character(incgrptemp$working_sector) ## Changing to character

incgrptemp$working_sector<-gsub("without_pay","not_worked",incgrptemp$working_sector,fixed = TRUE)

incgrptemp$working_sector<-as.factor(incgrptemp$working_sector) ## Changing back to factor

incgrp<-incgrptemp ## Reassigning incgrptemp values to incgrp

rm(incgrptemp) ## Removing temp variable

## Grouping Levels Marital Status ##

table(incgrp$marital_status,incgrp$target)

incgrptemp<-incgrp ## Assigning values of incgrp to incgrptemp

incgrptemp$marital_status<-as.character(incgrptemp$marital_status) ## Changing to character

## Married-civilian & Married-defence have similar target proportion

incgrptemp$marital_status<-gsub("Married-civilian","Married-resident",incgrptemp$marital_status,fixed = TRUE)
incgrptemp$marital_status<-gsub("Married-defence","Married-resident",incgrptemp$marital_status,fixed = TRUE)

## Separated & Widowed have similar target proportion so can be combined within separated

incgrptemp$marital_status<-gsub("Widowed","Separated",incgrptemp$marital_status,fixed = TRUE)

table(incgrptemp$marital_status,incgrptemp$target)

incgrptemp$marital_status<-as.factor(incgrptemp$marital_status) ## Changing back to factor

incgrp<-incgrptemp ## Reassigning incgrptemp values to incgrp

rm(incgrptemp) ## Removing temp variable incgrptemp

## Grouping Levels - Occupation ##

table(incgrp$occupation,incgrp$target)

incgrptemp<-incgrp ## Assigning values to temp variable incgrptemp

incgrptemp$occupation<-as.character(incgrptemp$occupation) ## Changing to character

## cleaner, house servant & other have similar target proportion

incgrptemp$occupation<-gsub("cleaner","lowest",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("house_servant","lowest",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("other","lowest",incgrptemp$occupation,fixed = TRUE)

## clerical, defence, farming & inspector have similar target proportion

incgrptemp$occupation<-gsub("clerical","low",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("defence","low",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("farming","low",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("inspector","low",incgrptemp$occupation,fixed = TRUE)

## sales can be renamed to lower mid to maintain consistency

incgrptemp$occupation<-gsub("sales","lower mid",incgrptemp$occupation,fixed = TRUE)

## repair & transport have similar target proportion

incgrptemp$occupation<-gsub("repair","mid",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("transport","mid",incgrptemp$occupation,fixed = TRUE)

## guard & support have similar target proportion

incgrptemp$occupation<-gsub("guard","upper mid",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("support","upper mid",incgrptemp$occupation,fixed = TRUE)

## managerial & specialty have similar target proportion

incgrptemp$occupation<-gsub("managerial","upper",incgrptemp$occupation,fixed = TRUE)
incgrptemp$occupation<-gsub("specialty","upper",incgrptemp$occupation,fixed = TRUE)

incgrptemp$occupation<-as.factor(incgrptemp$occupation) ## Changing back to factor

incgrp<-incgrptemp ## Reassigning values

rm(incgrptemp) ## Removing temp variable incgrptemp

## Grouping Levels for Relationship ##

table(incgrp$relationship,incgrp$target)
incgrptemp<-incgrp ## Assigning values to a temp variable
incgrptemp$relationship<-as.character(incgrptemp$relationship) ## Changing to character

## Husband & wife can be combined. Other can be combined based on target proportion

incgrptemp$relationship<-gsub("Husband","Spouse",incgrptemp$relationship,fixed = TRUE)
incgrptemp$relationship<-gsub("Wife","Spouse",incgrptemp$relationship,fixed = TRUE)
incgrptemp$relationship<-gsub("Not-in-family","Other",incgrptemp$relationship,fixed = TRUE)
incgrptemp$relationship<-gsub("Other-relative","Other",incgrptemp$relationship,fixed = TRUE)
incgrptemp$relationship<-gsub("Own-child","Other",incgrptemp$relationship,fixed = TRUE)
incgrptemp$relationship<-gsub("Unmarried","Other",incgrptemp$relationship,fixed = TRUE)

incgrptemp$relationship<-as.factor(incgrptemp$relationship) ## Changing back to factor

incgrp<-incgrptemp ## Reassigning values

rm(incgrptemp) ## Removing temp variable

## Grouping Levels for Ethnicity ##

table(incgrp$ethnicity,incgrp$target)
incgrptemp<-incgrp ## Assigning values to a temp variable
incgrptemp$ethnicity<-as.character(incgrptemp$ethnicity) ## Converting to character values

## Amer - Indian - Eskimo, Black & Other can be combined based on target proportion

incgrptemp$ethnicity<-gsub("Amer-Indian-Eskimo","Bk",incgrptemp$ethnicity,fixed = TRUE)
incgrptemp$ethnicity<-gsub("Black","Bk",incgrptemp$ethnicity,fixed = TRUE)
incgrptemp$ethnicity<-gsub("Other","Bk",incgrptemp$ethnicity,fixed = TRUE)

## Asian-Pac-Islander and White can be combined based on target proportion

incgrptemp$ethnicity<-gsub("Asian-Pac-Islander","Wh",incgrptemp$ethnicity,fixed = TRUE)
incgrptemp$ethnicity<-gsub("White","Wh",incgrptemp$ethnicity,fixed = TRUE)

table(incgrptemp$ethnicity,incgrptemp$target)

incgrptemp$ethnicity<-as.factor(incgrptemp$ethnicity) ## Changing back to Factor

incgrp<-incgrptemp ## Reassigning values

rm(incgrptemp) ## Removing temp variable

## Grouping Levels for Country ##

table(incgrp$country,incgrp$target)

incgrptemp<-incgrp ## Assigning value to a temp variable
incgrptemp$country<-as.character(incgrptemp$country) ## Converting to character values

## Very Low as per target proportion

incgrptemp$country<-gsub("Columbia","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Dominican-Republic","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("El-Salvador","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Guatemala","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Haiti","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Holand-Netherlands","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Honduras","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Jamaica","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Laos","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Mexico","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Nicaragua","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Outlying-US(Guam-USVI-etc)","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Peru","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Portugal","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Puerto-Rico","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Trinadad&Tobago","Very Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Vietnam","Very Low",incgrptemp$country,fixed = TRUE)

## Low as per target proportion

incgrptemp$country<-gsub("Ecuador","Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Hungary","Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Ireland","Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Poland","Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Scotland","Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("South","Low",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Thailand","Low",incgrptemp$country,fixed = TRUE)

## Medium as per target proportion

incgrptemp$country<-gsub("China","Medium",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Cuba","Medium",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Greece","Medium",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("United-States","Medium",incgrptemp$country,fixed = TRUE)

## High as per target proportion

incgrptemp$country<-gsub("Cambodia","High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Canada","High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("England","High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Germany","High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Hong","High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Italy","High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Japan","High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Philippines","High",incgrptemp$country,fixed = TRUE)

## Very High as per target proportion

incgrptemp$country<-gsub("France","Very High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("India","Very High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Iran","Very High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Taiwan","Very High",incgrptemp$country,fixed = TRUE)
incgrptemp$country<-gsub("Yugoslavia","Very High",incgrptemp$country,fixed = TRUE)

table(incgrptemp$country,incgrptemp$target)

incgrptemp$country<-as.factor(incgrptemp$country) ## Converting back to factor

incgrp<-incgrptemp ## Reassigning back all values

rm(incgrptemp) ## Removing temp variable

## Checking data again

str(incgrp)


summary(incgrp)

## Missing Value Imputation ##

sum(is.na(incgrp))

colSums(is.na(incgrp))

incgrptemp<-incgrp ## Assigning to temp variable

manyNAs(incgrptemp) ## Returns rows 31561, 31572

incgrptemp<-incgrptemp[-c(31561,31572),] ## Removing two rows

library(DMwR)

colSums(is.na(incgrptemp)) ## NA values in working_sector, occupation & country

incgrptemp =  knnImputation(data = incgrptemp, k=5)

colSums(is.na(incgrptemp)) ## NA values imputed

incgrp<-incgrptemp ## Reassigning values

rm(incgrptemp) ## Removing temp variable


## Train Validation Split ##

library(caret)
set.seed(6)

train_rows<-createDataPartition(incgrp$target,p = 0.7, list = F)

train_data<-incgrp[train_rows,]

validation_data<-incgrp[-train_rows,]

rm(train_rows) ## Remove the variable train_rows

rm(incgrp) ## Remove the variable incgrp since it is not required anymore

## Build a Model ##

str(train_data)
str(test_data)

target =train_data$target
str(target)
target = as.factor(target)
str(target)

train_data$target = NULL

str(train_data)
train_data = cbind(train_data,target)
str(train_data)


target =test_data$target
str(target)
target = as.factor(target)
str(target)

test_data$target = NULL

str(test_data)
test_data = cbind(test_data,target)
str(test_data)


library(randomForest)


test_data = validation_data
# Build the classification model using randomForest
model = randomForest(target ~ ., data=train_data, 
                     keep.forest=TRUE, ntree=100) 

# Print and understand the model
print(model)


model$importance  
round(importance(model), 2)   

rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]


# plot (directly prints the important attributes) 
varImpPlot(model)


# Predict on Train data 
pred_Train = predict(model, 
                     train_data[,setdiff(names(train_data), "target")],
                     type="response", 
                     norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Train = table("actual"= train_data$target, "predicted" = pred_Train);
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
accu_Train
rm(pred_Train, cm_Train)




pred_Test = predict(model, test_data[,setdiff(names(test_data),
                                              "target")],
                    type="response", 
                    norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual"=test_data$target, "predicted"=pred_Test);
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test

#####build RF using the imp attributes

top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:12])
top_Imp_Attr


set.seed(15)

# Build the classification model using randomForest
model_Imp = randomForest(target~.,
                         data=train_data[,c(top_Imp_Attr,"target")], 
                         keep.forest=TRUE,ntree=100) 

# Print and understand the model
print(model_Imp)

# Important attributes
model_Imp$importance  

# plot (directly prints the important attributes) 
varImpPlot(model_Imp)


# Predict on Train data 
pred_Train = predict(model_Imp, train_data[,top_Imp_Attr],
                     type="response", norm.votes=TRUE)


# Build confusion matrix and find accuracy   
cm_Train = table("actual" = train_data$target, 
                 "predicted" = pred_Train);
accu_Train_Imp = sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

# Predicton Test Data
pred_Test = predict(model_Imp, test_data[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual" = test_data$target, 
                "predicted" = pred_Test);
accu_Test_Imp = sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test
accu_Train_Imp
accu_Test_Imp
str(train_data)

#Select mtry value with minimum out of bag(OOB) error.
mtry <- tuneRF(train_data[-16],train_data$target, ntreeTry=100,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


#Parameters in tuneRF function
#The stepFactor specifies at each iteration, mtry is inflated (or deflated) by this value
#The improve specifies the (relative) improvement in OOB error must be by this much for the search to continue
#The trace specifies whether to print the progress of the search
#The plot specifies whether to plot the OOB error as function of mtry


#Build Model with best mtry again - 
set.seed(71)
rf <- randomForest(target~.,data=train_data, mtry=best.m, importance=TRUE,ntree=100)
print(rf)

#Evaluate variable importance
importance(rf)

# Important attributes
model$importance  
round(importance(model), 2)   

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

# Predict on Train data 
pred_Train = predict(model, 
                     train_data[,setdiff(names(train_data), "target")],
                     type="response", 
                     norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Train = table("actual"= train_data$target, "predicted" = pred_Train);
accu_Train = sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

# Predicton Test Data
pred_Test = predict(model, test_data[,setdiff(names(test_data),
                                              "target")],
                    type="response", 
                    norm.votes=TRUE)

# Build confusion matrix and find accuracy   
cm_Test = table("actual"=test_data$target, "predicted"=pred_Test);
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(cm_Test)

accu_Train
accu_Test

library(e1071)

model_svm <- svm(target~ age + working_sector + financial_weight + years_of_education + marital_status + occupation + relationship , data = train_data, kernel = "linear")

summary(model_svm)


library(caret)

sampling_strategy <- trainControl(method = "repeatedcv", number = 4, repeats = 10)

svm_rough_model_c <- train(target ~ . , train_data, method = "svmLinear",
                           tuneGrid = data.frame(.C = c(10^-4, 10^-3, 10^-2, 10^-1, 10^1, 10^2, 10^3)), trControl = sampling_strategy)

svm_rough_model_c
## Check for collinearity & removing variables

