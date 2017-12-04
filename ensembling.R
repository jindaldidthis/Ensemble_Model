#Remove all exisitng variables & data frames
rm(list=ls(all=TRUE))

# Loading required libraries
if (!require("haven")) install.packages("haven");library(haven)
if (!require("vegan")) install.packages("vegan");library(vegan)
if (!require("randomForest")) install.packages("randomForest");library(randomForest)
if (!require("infotheo")) install.packages("infotheo");library(haven)library(infotheo)
if (!require("C50")) install.packages("C50");library(C50)
if (!require("rpart")) install.packages("rpart");library(rpart)
if (!require("dummies")) install.packages("dummies");library(dummies)
if (!require("e1071")) install.packages("e1071");library(e1071)
if (!require("DMwR")) install.packages("DMwR");library(DMwR)

# Set  working directory
setwd("C:\\Users\\jindal\\Desktop\\stack")


# Read the data from csv file
data = read.csv(file = "SaratogaHouses.csv", header = TRUE, col.names = TRUE)
str(data)



# Convert attributes to appropriate type  
attr <- c('price','lotsize','age','landValue','livingArea','pctCollege','bedrooms','fireplaces','bathrooms','rooms',
          'heating','fuel','sewer','waterfront','newConstruction','centralAir')
cat_Attr = c('heating','fuel','sewer','waterfront','newConstruction','centralAir')
num_Attr = setdiff(attr, cat_Attr)
num_Attr_without_target = setdiff(num_Attr,"price")

cat_Data = data.frame(sapply(data[,cat_Attr], as.factor))
num_Data = data.frame(sapply(data[,num_Attr], as.numeric))
data <- cbind(num_Data,cat_Data)
str(data)

# ind_Attr = setdiff(attr, "price")
# num_Attr = setdiff(attr,ind_Attr)

#Standardizing numeric data
data[,num_Attr_without_target] <- decostand(data[,num_Attr_without_target],'range')

#Convert all categorical attributes to numeric using dummy function
heating <- dummy(data$heating)
fuel <- dummy(data$fuel)
sewer <- dummy(data$sewer)
waterfront <- dummy(data$waterfront)
newConstruction <- dummy(data$newConstruction)
centralAir <- dummy(data$centralAir)

data = subset(data,select= -c(heating,fuel,sewer,waterfront,newConstruction,centralAir))
data <- cbind(data,heating,fuel,sewer,waterfront,newConstruction,centralAir)


#changing some column names,to make them more readable 
names(data)[c(12,13,18)]
names(data)[c(12,13,18)] <- c('heatinghot_air','heatinghot_water_steam','sewerpublic_commercial')


# #Getting Variable Importance using Random Forest and using the important variables in the model
# set.seed(123)
# rf <- randomForest(price~., data=data, ntree=500,keep.forest=T, importance=TRUE)
# round(importance(rf), 2)
# varImpPlot(rf)
# ## Look at variable importance:
# imp_attributes <- importance(rf)[,1]
# imp_attributes <- sort(imp_attributes,decreasing = T)
# imp_attributes <- as.data.frame(imp_attributes)
# imp_attributes <- row.names(imp_attributes)[1:20]
# data_imp <- data[,imp_attributes]
# data_imp <- cbind(data_imp,data$price)
# names(data_imp)[21] <- 'price'
# data <- data_imp


# Divide the data into test and train
set.seed(1234)
train_RowIDs = sample(1:nrow(data), nrow(data)*0.7)
train_Data = data[train_RowIDs,]
test_Data = data[-train_RowIDs,]
#rm(train_RowIDs)

#----------------Ensemble:Stacking-------------------- 

# Build CART model on the training dataset
cart_Model = rpart(price~., train_Data,method = "anova")
summary(cart_Model)

#Build a Random Forest model on the training dataset
rf_Model <- randomForest(price~., data=train_Data, ntree=50,keep.forest=T, importance=TRUE)
summary(rf_Model)

#Build S.V.M model for the training dataset
svm_model <- svm(train_Data[,-1],train_Data$price,type = "nu-regression")
summary(svm_model)

# #Build a linear model
# linear_model <-lm(price~.,train_Data)
# summary(linear_model)

#---------Predict on Train Data----------
# Using CART Model predict on train data
cart_Train = predict(cart_Model, train_Data, type = "vector")
regr.eval(cart_Train,train_Data$price)


#Using random forest to predict data on train dataset
rf_Train <-predict(rf_Model,train_Data)
regr.eval(rf_Train,train_Data$price)

#Using S.V.M to predict training dataset
svm_Train <- predict(svm_model,train_Data[,-1])
regr.eval(svm_Train,train_Data$price)

# #using linear regression
# linear_Train<- predict(linear_model,train_Data[,-1])
# regr.eval(linear_Train,train_Data$price)
# x = cor(train_Data)

# Combining training predictions of CART, SVM & NN together
train_Pred_All_Models = data.frame(CART = cart_Train,
                                   SVM = svm_Train,
                                   RandomForest = rf_Train)
rm(cart_Train,svm_Train,linear_Train)

# Adding the original DV to the dataframe
train_Pred_All_Models = cbind(train_Pred_All_Models, price = train_Data$price)

cor(train_Pred_All_Models)

# #Principal Component Analysis on the dataframe
# train_Pred_All_Models_Pca <- princomp(train_Pred_All_Models[,-5])
# train_Pred_All_Models_Pca <- as.data.frame(train_Pred_All_Models_Pca$scores)
# train_Pred_All_Models_Pca <- cbind(train_Pred_All_Models_Pca,train_Pred_All_Models$price)
# names(train_Pred_All_Models_Pca)[5] <- 'price'

# ensemble_Model1 = lm(price ~ ., train_Pred_All_Models_Pca)
# summary(ensemble_Model1)

#Meta-learner model
ensemble_Model = lm(price ~ ., train_Pred_All_Models)
summary(ensemble_Model)

# Check the "ensemble_Model model" on the train data
# ensemble_Train = predict(ensemble_Model, train_Pred_All_Models_Pca)
# regr.eval(ensemble_Train,train_Data$price)

ensemble_Train = predict(ensemble_Model, train_Pred_All_Models)
regr.eval(ensemble_Train,train_Data$price)

#---------Predict on Test Data----------

# Using CART Model predict on test dataset
cart_Test = predict(cart_Model, test_Data, type = "vector") 
regr.eval(cart_Test,test_Data$price)

#Using S.V.M to predict testing dataset
svm_Test<- predict(svm_model,test_Data[,-1])
regr.eval(svm_Test,test_Data$price)

#Using random forest to predict data on train dataset
rf_Test <-predict(rf_Model,test_Data)
regr.eval(rf_Test,test_Data$price)


#using linear regression
# linear_Test<- predict(linear_model,test_Data[,-1])
# regr.eval(linear_Test,test_Data$price)

# Combining test predictions of CART, C5.0 & Log Regression together
test_Pred_All_Models = data.frame(CART = cart_Test,
                                   SVM = svm_Test,
                                  RandomForest = rf_Test)
rm(cart_Test,knn_Test,svm_Test,nn_Test)

# Adding the original DV to the dataframe
test_Pred_All_Models = cbind(test_Pred_All_Models, price = test_Data$price)

# #Principal Component Analysis on the dataframe
# test_Pred_All_Models_Pca <- princomp(test_Pred_All_Models[,-5])
# test_Pred_All_Models_Pca <- as.data.frame(test_Pred_All_Models_Pca$scores)
# test_Pred_All_Models_Pca <- cbind(test_Pred_All_Models_Pca,test_Pred_All_Models$price)
# names(test_Pred_All_Models_Pca)[5] <- 'price'
# 
# ensemble_Model = lm(price ~ ., test_Pred_All_Models_Pca)
# summary(ensemble_Model)
# 


#Predict the values on test data
ensemble_Test = predict(ensemble_Model, test_Pred_All_Models)
regr.eval(ensemble_Test,test_Data$price)
