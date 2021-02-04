


### Project P34 
### Prepare model for making prediction for assigning sales targets to sales persons at the basis of their last month achievement #######

## Automated EDA Process
# Set the environment for load the dataset 
install.packages("readxl")
library(readxl)
# Load the Final Dataset
Final_Dataset <-  read_xlsx("E:\\Project_P34_ExcelR\\Final Dataset.xlsx")

Validation_Dataset <- read_xlsx("E:\\Project_P34_ExcelR\\Validation Dataset.xlsx")

# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)

#Check the all features type
introduce(Final_Dataset)

#Covert all chararacter features into numeric
Final_Dataset$PROD_CD <- as.numeric(factor(Final_Dataset$PROD_CD))
Final_Dataset$SLSMAN_CD <- as.numeric(factor(Final_Dataset$SLSMAN_CD))


#Summary of features  
summary(Final_Dataset$PROD_CD)
summary(Final_Dataset$SLSMAN_CD)
summary(Final_Dataset$PLAN_MONTH)
summary(Final_Dataset$TARGET_IN_EA)
summary(Final_Dataset$ACH_IN_EA)


##Visualiation of all features & report generation of them 
plot_intro(Final_Dataset)
plot_missing(Final_Dataset)

#Histogram
DataExplorer::plot_histogram(Final_Dataset)
#Histogram for transformed variable featuers 
DataExplorer::plot_histogram(log(Final_Dataset))

#Density plot
plot_density(Final_Dataset)
#Bar Plot 
plot_bar(Final_Dataset)

#Correlation between features 
plot_correlation(Final_Dataset, cor_args = list( 'use' = 'complete.obs'))

#Box plots by indivisual feature based against all features 
plot_boxplot(Final_Dataset, by= 'PLAN_MONTH',  ncol = 2)
plot_boxplot(Final_Dataset, by= 'PROD_CD',  ncol = 2)
plot_boxplot(Final_Dataset, by=  'SLSMAN_CD', ncol=2)
plot_boxplot(Final_Dataset, by=  'TARGET_IN_EA', ncol=2)
plot_boxplot(Final_Dataset, by=  'ACH_IN_EA', ncol=2)

# Box plots by transofmed indivisual feature based against all features 
plot_boxplot(log(Final_Dataset), by= 'PLAN_MONTH',  ncol = 2)
plot_boxplot(log(Final_Dataset), by= 'PROD_CD',  ncol = 2)
plot_boxplot(log(Final_Dataset), by=  'SLSMAN_CD', ncol=2)
plot_boxplot(log(Final_Dataset), by=  'TARGET_IN_EA', ncol=2)
plot_boxplot(log(Final_Dataset), by=  'ACH_IN_EA', ncol=2)


# Scatter plots by indivisual feature based against all features 
plot_scatterplot(Final_Dataset, by= 'PLAN_MONTH')
plot_scatterplot(Final_Dataset, by= 'PROD_CD')
plot_scatterplot(Final_Dataset, by=  'SLSMAN_CD')
plot_scatterplot(Final_Dataset, by=  'TARGET_IN_EA')
plot_scatterplot(Final_Dataset, by=  'ACH_IN_EA')


# Visualization HTML report generation in webbrowser of all features 
create_report(Final_Dataset)


## Preprocessing on "Final_Dataset" 
# Cheking the duplicacy in data 
duplicated(Final_Dataset)
# Checking the coorelation between 
cor(Final_Dataset)
# Removing the feature "PLAN_YEAR"
Final_Dataset <- Final_Dataset[,-4]


# Custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#Apply normalization to entire data frame
norm_final_data <- as.data.frame(lapply(Final_Dataset[,c(4:5)],normalize))
final_data <- cbind(Final_Dataset[1:3],norm_final_data)


## Automated EDA on Validation data set 
#Check the missing values 
sum(is.na(Validation_Dataset))

# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)

#Check the all features type
introduce(Validation_Dataset)

#Covert all chararacter features into numeric
Validation_Dataset$PROD_CD <- as.numeric(factor(Validation_Dataset$PROD_CD))
Validation_Dataset$SLSMAN_CD <- as.numeric(factor(Validation_Dataset$SLSMAN_CD))


#Summary of variable features  
summary(Validation_Dataset$PROD_CD)
summary(Validation_Dataset$SLSMAN_CD)
summary(Validation_Dataset$TARGET_IN_EA)


##Visualiation of all features & report generation of them 
plot_intro(Validation_Dataset)
plot_missing(Validation_Dataset)

#Histogram
DataExplorer::plot_histogram(Validation_Dataset)
#Transformation of all histogram 
DataExplorer::plot_histogram(log(Validation_Dataset))

#Density plot
plot_density(Validation_Dataset)
#Bar Plot 
plot_bar(Validation_Dataset)

#Correlation between features 
plot_correlation(Validation_Dataset, cor_args = list( 'use' = 'complete.obs'))

#Box plots by indivisual feature based against all variable features 
plot_boxplot(Validation_Dataset, by= 'PROD_CD',  ncol = 2)
plot_boxplot(Validation_Dataset, by=  'SLSMAN_CD', ncol=2)
plot_boxplot(Validation_Dataset, by=  'TARGET_IN_EA', ncol=2)

#Trasnformation of all features by box plot visualization 
plot_boxplot(log(Validation_Dataset), by= 'PROD_CD',  ncol = 2)
plot_boxplot(log(Validation_Dataset), by=  'SLSMAN_CD', ncol=2)
plot_boxplot(log(Validation_Dataset), by=  'TARGET_IN_EA', ncol=2)


#Scatter plots by indivisual feature based against all variable features 
plot_scatterplot(Validation_Dataset, by= 'PROD_CD')
plot_scatterplot(Validation_Dataset, by=  'SLSMAN_CD')
plot_scatterplot(Validation_Dataset, by=  'TARGET_IN_EA')

#Trasnformation of TARGET_IN_EA by Scatter plot visualization 
plot_scatterplot(log(Validation_Dataset), by=  'TARGET_IN_EA')

# Visualization HTML report generation in webbrowser of all features 
create_report(Validation_Dataset)

# Preprocessing on "Validation_dataset"
# Checking the duplicacy 
duplicated(Validation_Dataset)
# Check the correlation
cor(Validation_Dataset)

# Preprocessing of data 
Validation_Dataset <- Validation_Dataset[,-c(4,6)]


# Custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#Apply normalization to entire data frame
norm_validation_data <- as.data.frame(lapply(Validation_Dataset[,4],normalize))
validation_data <- cbind(Validation_Dataset[1:3],norm_validation_data)


# Set the environment 
install.packages("caret")
library(caret)
Control_parameters <- trainControl(method = "cv",
                                   number = 10,
                                   savePredictions = TRUE,
                                   classProbs = TRUE)



##Set the environment 
install.packages("xgboost")
library(xgboost)


# Prepare the xgBoost model 
sales_ach_model <- train(ACH_IN_EA~.,
                            data = final_data,
                            method= "xgbTree",
                            trControl= Control_parameters)




##View the forest results.
print(sales_ach_model)
#Prediction
ACH_IN_EA<- predict(sales_ach_model,validation_data)
validation_data1 <- cbind(validation_data,ACH_IN_EA)


# Set the environment
install.packages("caret")
library(caret)
Control_parameters1 <- trainControl(method = "cv",
                                   number = 10,
                                   savePredictions = TRUE,
                                   classProbs = TRUE)



##Set the environment 
install.packages("xgboost")
library(xgboost)


# Prepare the xgBoost model 
sales_target_model <- train(TARGET_IN_EA~.,
                         data = final_data,
                         method= "xgbTree",
                         trControl= Control_parameters1)




##View the forest results.
print(sales_target_model)
#Prediction
predict_target<- predict(sales_target_model,validation_data1)

# cheking testing accuracy 
rmse <- RMSE(predict_target,validation_data1$TARGET_IN_EA)
R_square <- c(1-rmse)
R_square

#Prediction of traning dataset 
predict_target1 <- predict(sales_target_model,final_data)
# Cheking the traning accuracy 
rmse1 <- RMSE(predict_target1,final_data$TARGET_IN_EA)
R_square1 <- c(1-rmse1)
R_square1

# Prepare final data 
final_target <- cbind(validation_data1,predict_target)







