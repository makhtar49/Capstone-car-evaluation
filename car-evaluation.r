'#**************************************************
HarvardX: PH125.9x
Data Science: Capstone
Car-Evaluation by Muhammad Akhtar (Feb 2022)'
# *********************************************************
# Installing packages
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org") 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xfun)) install.packages("xfun", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(digest)) install.packages("digest", repos = "http://cran.us.r-project.org")
if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kabelExtra", repos = "http://cran.us.r-project.org")
#====================================================
# Upload library readr to run function read_csv
#====================================================
library(readr)
#=====================================================
#Downloading data from https://archive.ics.uci.edu/ml/machine-learning-databases/car
#=============================================================
#Create a local copy of the file under the name cardl
#==============================================================
cardl<-tempfile()
#URL for CAR-Evaluation dataset in csv format
url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"
cardl<-read_csv(url)

colnames(cardl)<-c("buying", "maint", "doors", "persons",
                "lug_boot","safety","class")  
#========================================================
# upload library dplyr to run function glimpse
#==============================================
library(dplyr)
glimpse(cardl)
#=================================
#saving data as cardl.RData on the disk for use in rmd file
#============================================
save(cardl,file =" cardl.RData")
#Preparing data
#======================================================
#checking and counting for missing values (NA or NULL) if any
#=================================================
cardl %>% summarize(NA_buying=sum(is.na(buying)),
                 NA_maint=sum(is.na(maint)),
                 NA_doors=sum(is.na(doors)),
                 NA_persons=sum(is.na(persons)),
                 NA_lug_boot=sum(is.na(lug_boot)),
                 NA_safety=sum(is.na(safety)),
                 NA_class=sum(is.na(class)))
#============================================
#Create a copy of cardl 
#==========================================
dl.copy<-cardl
'#======================================================
 Replace 5more with 5 in coulmn 3,
 and in column 4 replac more with 5 '
#==============================================
dl.copy<-dl.copy %>%
  mutate(doors =case_when(doors =="5more" ~ "5",
                          TRUE ~ as.character(doors)), 
         doors = as.integer(doors))%>%
  mutate(persons = case_when(persons =="more" ~ "5",
                             TRUE ~ as.character(persons)),persons = as.integer(persons)) 

'#==========================================
  dl.copy categorical data, convert string 
   categories into numric and store in car.dat as data frame'
#=======================================                               
car.mat<-data.matrix(dl.copy)
car.dat<-data.frame(car.mat)
'#================================================== 
 create correlation matrix to see any correlatio of class
 with feature '
#=============================================== 
x<-round(cor(car.mat),3)
#=======================================
# Display correlation matrix
#=======================================
x
'#============================================
#Upload the library reshape2 to convert correlation matrix into two 
two columns Var 1 and var 2 '
#=========================================
library(reshape2)
#=========================================
# Upload the library ggplot2 to run the function heatmap of correlation matrix
#====================================================
library(ggplot2)
#==================================================
cormat<-x
cormat<-round((cormat),3)
cormat[!(col(cormat)>=row(cormat))]<-NA
melted_cormat<-melt(cormat,na.rm=TRUE)
melted_cormat
ggheatmap<-ggplot(melted_cormat, aes(Var2, Var1,
                                     fill = value))+ geom_raster()
ggheatmap + geom_text(aes(Var2, Var1, label = value)
                      , color = "white", size = 2) 
#=====================================================
#Partition of dl.copy into 80% percent edx and 20% validation dataset
#=================================================================== 
'upload library caret to run createDataPartition function'
#=====================================================
library(caret)
#==============================================================
#set.seed(some random number) to ensure that we get the same sample each time
#===============================================================
set.seed(1,sample.kind="Rounding")#if usig R 3.5 or earlier, use set.seed(1)
testIndex<-createDataPartition(y=dl.copy$class, times=1, p=0.2, list=FALSE)
#===================================================
#edx has numbers, and validation has characters and integers
#=======================================================
edx<-dl.copy[-testIndex,]
validation<-dl.copy[testIndex,]
edx<-as.data.frame(data.matrix(edx))
head(edx) 
head(validation) 
#=================================================
#Methods and Analysis 
#===================================
#1. Generalized Linear Model (glm)
#========================================

#====================================================== 
#Data preparation for 
#=============================================
#===========================================
#upload dplyr library to activate %>% function
#================================================
library(dplyr)
#=====================================
#edx has integers, convert into factors
#================================================
train_set<-as.data.frame(edx)%>%
  select(buying,maint,doors,persons,lug_boot,safety,class)%>%
  mutate(buying=factor(buying),
         maint=factor(maint),
         lug_boot=factor(lug_boot),
         safety=factor(safety),
         class=factor(class))
# train_set has factors
head(train_set)
#============================================
'#Create test set and validation set by dividing
 validation dataset into 50% test set and 50% validation set using sample function
'
#==============================================================
vtestIndex = sort(sample(nrow(validation), nrow(validation)*.5))
#====================================================
#creating test data set by selecting the output row values 
#=================================================
vtest_set<-validation[vtestIndex,]
#================================================
#creating validation data set by not selecting the output row values
#==========================================================
validation_test<-validation[-vtestIndex,]
#========================================================
#convert vtest_set into integers, data frame, and then into factors
#==================================================
test_set<-as.data.frame(data.matrix(vtest_set))

test_set<-test_set%>%
  select(buying,maint,doors,persons,lug_boot,safety,class)%>%
  mutate(buying=factor(buying),
         maint=factor(maint),
         lug_boot=factor(lug_boot),
         safety=factor(safety),
         class=factor(class))
#===============================================
'#validation_set has character and integers
#Convert validation_set into factors'
#==============================================
validation_set<-as.data.frame(data.matrix(validation_test))%>%
  select(buying,maint,doors,persons,lug_boot,safety,class)%>%
  mutate(buying=factor(buying),
         maint=factor(maint),
         lug_boot=factor(lug_boot),
         safety=factor(safety),
         class=factor(class))
#===================================== 
# upload net and e1071 libraries to run multinom function
#============================================================
library(nnet) 
library(e1071)
mnm.fit<-multinom(class~.,data=train_set)
y_hat_mnm<-predict(mnm.fit,test_set)
accuracy_mnm<-confusionMatrix(y_hat_mnm,validation_set$class)$overall[["Accuracy"]]
accuracy_mnm 
accmat<-table("pred" = y_hat_mnm, "class" = factor(validation_set$class))
#Accuracy matrix
accmat
#=============================
#2.NearestNeighbor (K-NN ) Model
#============================
# use the car.dat data values
#======================================
glimpse(car.dat)
#=============================================
#separate the class outcome from features 
#==============================================
#Make sure all variables are numeric because distance is found between numeric
#=============================================
class_outcome<-car.dat%>%select(class)
car_features<-car.dat%>%select(-class)
glimpse(class_outcome)
glimpse(car_features)

#================================================
#Split car-features data into training and test sets
#===================================================
set.seed(2021)
sample_size<-floor(0.80*nrow(car_features))
train_ind<-sample(seq_len(nrow(car_features)),size = sample_size)
#================================================
# Split features into training and test set
#=============================================
class_pred_train<-car_features[train_ind,]
class_pred_test<-car_features[-train_ind,]
#=======================================================
#Split outcome variable class-outcome into traing and test sets
#=======================================================
class_outcome_train<-class_outcome[train_ind,]
class_outcome_test<-class_outcome[-train_ind,]
#===========================================
# Calculate the number of nearest neighbors k value
# using the size of training set
#========================================================
k<- floor(sqrt(sample_size))
k
#===========================================
#upload the library class 
#======================================

library(class)
class_pred_knn<-knn(train=class_pred_train,test=class_pred_test,cl=class_outcome_train, k)

#Model evaluation

accuracy_without_caret<-confusionMatrix(class_pred_knn,factor(class_outcome_test))$overall["Accuracy"]
accuracy_without_caret
#=============================================
#Dislay accuracy matrix
#====================================
accmat<-table("pred" = class_pred_knn, "class"
              = factor(class_outcome_test))
accmat 
#===================================================
# Knn_Model with caret package to pick k 
#================================================
train_knn<-train(class_pred_train,class_outcome_train, method="knn", 
                 tuneGrid=data.frame(k=seq(1, floor(sqrt(sample_size)),1))) 
k<-train_knn$bestTune

class_pred_knn<-knn(train=class_pred_train,test=class_pred_test,cl=class_outcome_train, k)
#==================
#Model Evaluation
#=======================
accuracy_with_caret<-confusionMatrix(class_pred_knn,factor(class_outcome_test))$overall["Accuracy"]
k
accuracy_with_caret
#=====================================
#Display accuracy matrix
#=====================================
accmat<-table("pred" = class_pred_knn, "class_labels" = factor(class_outcome_test))
accmat
#=======================================================
#         Random Forest Model 
#======================================== 
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
#library(caret)
library(randomForest)
#library(datasets)
set.seed(1971)
glimpse(car_features)
#===============================================
#split car_features into training set (class_predict_train) and test set 
#class_predict_test
#==================================================
sample_size<-floor(0.80*nrow(car_features))

train_ind<-sample(seq_len(nrow(car_features)),size = sample_size)
class_pred_train<-car_features[train_ind,]
class_pred_test<-car_features[-train_ind,]
#===================================================================
#Split outcome variable class-outcome into traing = class_outcome_train
# and test set = class_outcome_test
#==================================================
class_outcome_train<-class_outcome[train_ind,]
class_outcome_test<-class_outcome[-train_ind,]
#====================================================
# Fit random Forest Model
#===========================================
rf<-randomForest(factor(class_outcome_train)~.,data=class_pred_train)

#=================================
#Prediction & Confusion Matrix-test data
#===========================================
rf_class_pred<-predict(rf,class_pred_test)       
accuracy_rf<-confusionMatrix(rf_class_pred,factor(class_outcome_test))$overall["Accuracy"] 
accuracy_rf 
#======================================
#Display Accuracy Matrix
#===============================
accmat<-table("pred" =  rf_class_pred, "class_labels" = factor(class_outcome_test))
accmat
#=======================================
#Results in tabular form
#=================================
library(data.table)
 
percent_accuracy<-data.frame(Model=c("(Multinomial logistic Model","KNN Model without caret","KNN Model with Caret Package","RandomForest"), 
                             Accuracy=c(accuracy_mnm,accuracy_without_caret, 
                                        accuracy_with_caret, accuracy_rf))  
percent_accuracy 

#===============================
# The End!
#================================
