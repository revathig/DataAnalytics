#---
#  title: "mith"
#author: Revathi
#Date: 19-05-2018
#output: html_document
---
  
  #Clear the Environment variables
  
rm(list=ls(all=TRUE))


##### Load the required Packages

###most of them are added part of the package if required add them here



library(myhelper)

library(caret)
library(DataExplorer)
library(DMwR)
library(dplyr)
library(C50)
library(rpart)
library(randomForest)
library(ROCR)
library(corrplot)
library(e1071)
library(class)
library(ada)
library(xgboost)
library(forcats)


library(MASS)


####some more functions if required add here




model_predict <- function(model, data_to_predict, type="response", isRF = FALSE, threshhold = 0)
{
  if(isRF)
  {
    pred_val = predict(model, data_to_predict, type=type, norm.votes=TRUE)
  }else
  {
    pred_val = predict(model, data_to_predict, type= type)
  }
  
  if(threshhold >0)
  {
    pred_val = ifelse(pred_val>threshhold,"non functional","functional")
  }
  
  pred_val
}

##change to return the required method
#accuracy or f1

model_analysis <- function(model, data_to_predict, y_val, type="response", isRF = FALSE, threshhold = 0)
{
  pred_val = model_predict(model,data_to_predict, type, isRF, threshhold)
  
  cm = confusionMatrix(pred_val, y_val)
  cm
  print(cm)
}

# write ti a file
## need to change here

Createoutputfile <- function(model, data_to_predict, type="response",
                             isRF = FALSE, threshhold = 0)
{
  pred_val = model_predict(model,data_to_predict, type, isRF, threshhold)
  write.csv(pred_val,file="TestPred.csv")
  
  myoutput = read.csv("TestPred.csv", header =T)
  str(myoutput)
  
  test_index = read.csv("test.csv", header= T)
  
  myoutput$index =NULL
  
  myoutput = cbind(test_index$Id, myoutput)  
  colnames(myoutput) = c("id", "status")
  
  colnames(myoutput)
  str(myoutput)
  
  write.csv(file = "Mithpredict.csv",x= myoutput,row.names = FALSE)
  
}




##Opening the file and Preprocessing




dataset = readdata("D:/Course/Rstart/Mith","train.csv")
dataset_target = readdata("D:/Course/Rstart/Mith","trainlabels.csv")
testdata = readdata("D:/Course/Rstart/Mith","test.csv")






```{r}
str(testdata)

```

### Data analysing
1.  Summary
2. structure
3. viewing
```{r}
summary(dataset)

```
```{r}
str(dataset)
```


```{r}
head(dataset)
```
```{r}
tail(dataset)
```

##viewing of the target variable

```{r}
str(dataset_target)
#view(dataset)


```

```{r}
colnames(dataset_target) = c("Id", "status")

```

```{r}

summary(dataset_target)
```
```{r}
print (levels(dataset$Village))

```

```{r}



```
###Merge the target variable with the dataset

```{r}

dataset_withtarget = merge(x = dataset, y = dataset_target,by = "Id", all = T)

summary(dataset_withtarget)
```




###Just viewing to check the data after merge

```{r}
tail(dataset_withtarget)
```

####PreProcessing


### Remove the Id and Organization from the data set
###Organization survayed is not unique we can remove 
### extarction, extarction_type,extraction_class have the same value we can have only one.
### Water quality, water_quality_group also has the similar values we can remove one of them
### Quatity and quatity_group also has the same values we can remove one of them
### Payment and payment_type also has the same value we can remove one of them
### source, source_type we can have one of them
### Region name and region code also mean the same we can remove one
### remove waterpoint name and village also

```{r}
remcols = c("Id", "Organization_surveyed", "Payment","Extraction_type","Extraction_type_group",  
            "Water_quality","Quantity_group","Source", "Regionname", "Waterpointname", "Village", "SchemeName",
            "Wardname", "Water_point_type_group", "Source_type", "Scheme_management", "Organization_funding", "Company_installed" )
valid_cols = setdiff(x=colnames(dataset_withtarget), y=remcols)
validcols_test = setdiff(x=colnames(testdata), y=remcols)

print(valid_cols)

dataset_temp = dataset_withtarget[, valid_cols]
testdata = testdata[ , validcols_test]

```

##View the new data set

```{r}
str(dataset_temp)

```
correlation between categorical attributes

```{r}

chisq.test(dataset_temp$Organization_funding, dataset_temp$Scheme_management)
chisq.test(dataset_temp$Organization_funding, dataset_temp$Management)
chisq.test(dataset_temp$Scheme_management,dataset_temp$Management )
chisq.test(dataset_temp$Company_installed, dataset_temp$Management)
chisq.test(dataset_temp$Company_installed,dataset_temp$Scheme_management)

```



#convert the logical attributes to categorical

dataset_temp$Public_meeting = as.factor(as.character(dataset_temp$Public_meeting))
#str(dataset_temp_catconvert$Public_meeting)
dataset_temp$Permit = as.factor(as.character(dataset_temp$Permit))


##Convert on the test data also

```{r}

testdata$Public_meeting = as.factor(as.character(testdata$Public_meeting))
testdata$Permit = as.factor(as.character(testdata$Permit))
nrow(testdata)



####checking for NA values 




sum(is.na(dataset_temp_catconvert))

colSums(is.na(dataset_temp_catconvert))
max(rowSums(is.na(dataset_temp_catconvert)))
manyNAs(is.na(dataset_temp_catconvert))
sum(is.na(testdata))

###Imputation fo NA's



dataset_imputed = centralImputation(dataset_temp)
testdata_imputed = centralImputation(testdata)
nrow(testdata)

###Split the data into train validation and test



set.seed(123)


train_rows <- createDataPartition(dataset_imputed$status , p = 0.8, list = F)

train_data <- dataset_imputed[train_rows, ]

test_data <- dataset_imputed[-train_rows, ]

print(nrow(train_data))
print(nrow(test_data))


##Split the train into train and validation

set.seed(10)

train_rows = createDataPartition(train_data$status, p=0.8, list = F)
new_train_data = train_data[train_rows, ]

validation_data = train_data[-train_rows, ]

train_data = new_train_data
rm(new_train_data)
print(nrow(train_data))
print(nrow(validation_data))



####strandadize the data




std_model <- preProcess(train_data[, !names(train_data) %in% c("status")], method = c("center", "scale"))


train_data[, !names(train_data) %in% c("status")] <- predict(object = std_model, newdata = train_data[, !names(train_data) %in% c("status")])

validation_data[, !names(validation_data) %in% c("status")] <- predict(object = std_model, newdata = validation_data[, !names(validation_data) %in% c("status")])

test_data[, !names(test_data) %in% c("status")] <- predict(object = std_model, newdata = test_data[, !names(test_data) %in% c("status")])




###Do on the test data

testdata_imputed <- predict(object = std_model, newdata = testdata_imputed)


nrow(testdata)
##Build the basic model 



basic_model <-glm(status~.,data=train_data,family = binomial)


summary(basic_model)


##predict on the validation data and test data
##Accuracy: 77%


cm=  model_analysis(basic_model,validation_data,validation_data$status,threshhold = 0.4)
cm=  model_analysis(basic_model,test_data,test_data$status,threshhold = 0.4)




###Trying with stepAIC to reduce the dimensions


model_aic = stepAIC(basic_model, direction="both")
summary(model_aic)

#################
###ADA Boost


train_data_wotarget = train_data[,!names(train_data) %in% c("status")]

validation_data_wotarget = validation_data[,!names(validation_data) %in% c("status")]
test_data_wotarget= test_data[,!names(validation_data) %in% c("status")]
colnames(train_data_wotarget)
colnames(test_Data_wotarget)
colnames(testdata_imputed)
#Build the Ada boost model

model = ada(x = train_data_wotarget,y = train_data$status,
            iter = 50, loss = "exponential", type="discrete", nu =0.4)
#Look at the model summary

summary(model)

ncol(train_data_wotarget)
model_analysis(model,validation_data_wotarget,validation_data$status,threshhold =0)

model_analysis(model,test_data_wotarget,test_data$status,threshhold =0)

ada_grid_params <- expand.grid("iter" = c(100, 150, 200), "nu" = c(0.3, 0.4, 0.5))
for(i in 1:nrow(ada_grid_params))
{
  iter = ada_grid_params[i, "iter"]
  nu = ada_grid_params[i, "nu"]
  ada_basic_model <- ada(x = train_data_wotarget[, 1:17], y = train_data$status, iter = iter, loss = "exponential", 
                         type= "discrete", nu = nu)
  
  model_analysis(ada_basic_model,validation_data_wotarget,validation_data$status,threshhold =0)
  
}

str(testdata_imputed)
pred_val = model_predict(model,testdata_imputed, type="vector")
write.csv(pred_val,file="TestPred.csv")

myoutput = read.csv("TestPred.csv", header =T)
str(myoutput)

test_index = read.csv("test.csv", header= T)

myoutput$X =NULL
str(myoutput)
myoutput = cbind(test_index$Id, myoutput)  
str(myoutput)
head(myoutput)
colnames(myoutput) = c("Id", "Status")

colnames(myoutput)
str(myoutput)

write.csv(file = "Mithpredictada.csv",x= myoutput,row.names = FALSE)

#Createoutputfile(model,testdata, type="vector")





ncol(train_data)
set.seed(20)
mtry <- tuneRF(train_data[, 1:19], train_data$status, ntreeTry = 200, stepFactor = 2, improve = 0.01, trace = F, plot = T)






mtry <- arrange(data.frame(mtry), OOBError)
best_m <- mtry[1, 1]

rf_basic <- randomForest(status ~ ., data = train_data, mtry = 56, importance = T, ntree = 200)
model_scores(rf_basic, x_data = train_data[, 1:22], y_data = train_data$status, type = "class", isRF = T)
model_scores(rf_basic, x_data = validation_data[, 1:22], y_data = validation_data$target, type = "class", isRF = T)
create_test_submission_file(rf_basic, x_data = test_data, file_name = "rf_basic_test", type = "class", isRF = T)
```

## Tuning Random Forest model for important attributes

```{r}
rf_importance <- importance(rf_basic)
rf_importance <- data.frame("Attributes" = row.names(rf_importance), "Importance" = rf_importance[, 4])
rf_importance <- arrange(rf_importance, desc(Importance))

# for(n in 28:35)
# {
#   top_n_attr <- as.character(rf_importance[1:n, 1])
#   rf_model_with_imp_attr <- randomForest(x = train_data[, top_n_attr], y = train_data$target, ntree = 200, mtry = n)
#   score <- model_scores(rf_model_with_imp_attr, x_data = validation_data[, top_n_attr], y_data = validation_data$target, type = "class", isRF = T)
#   print(paste0("For Top ", n, " Attributes F1 Scores is ", score[, "F1"]))
# }

```




```

