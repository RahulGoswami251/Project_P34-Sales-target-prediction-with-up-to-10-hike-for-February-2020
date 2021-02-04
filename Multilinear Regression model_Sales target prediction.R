


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
# Checking the coorelation between 
cor(Final_Dataset)
# Removing the feature "PLAN_YEAR"
Final_Dataset <- Final_Dataset[,-4]


#Apply normalization to entire data frame
norm_final_data <- scale(Final_Dataset[4:5])
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

# Check the correlation
cor(Validation_Dataset)

# Preprocessing of data 
Validation_Dataset <- Validation_Dataset[,-c(4,6)]



#Apply normalization to entire data frame
norm_validation_data <- scale(Validation_Dataset[4])
validation_data <- cbind(Validation_Dataset[1:3],norm_validation_data)


# Prepare the Multilinear regression model 
set.seed(250)
Sales_ach <- lm(ACH_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+TARGET_IN_EA, data=final_data)
summary(Sales_ach)

# Multi-colinearity 
install.packages("car")
library(car)
#Variance Inflation Factor
car::vif(Sales_ach)
##Subset selection
library(MASS)
stepAIC(Sales_ach)

## Full Model Building Process
Sales_ach <- lm(ACH_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+TARGET_IN_EA, data=final_data)
summary(Sales_ach)
##Digonastic Plots
## Residual Plots, QQ-Plots, Std. Residual Vs Fitted 
plot(Sales_ach)
#Residual Plot
residualPlots(Sales_ach)
#QQ-Plots
qqPlot(Sales_ach)
#Deletion Diagnostics
influenceIndexPlot(Sales_ach)

#check outliers
outlierTest(Sales_ach)

# Iteration 1 
# Remove outliers 
final_data1 <- final_data[-c(9590,20138,20117,20091,20090,20137,5259,8833,8601,20118),] 

# Prepare model 
Sales_ach1 <- lm(ACH_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+TARGET_IN_EA, data=final_data1)
summary(Sales_ach1)


plot(Sales_ach1) 
residualPlots(Sales_ach1)
qqPlot(Sales_ach1)
influenceIndexPlot(Sales_ach1)

#check outliers
outlierTest(Sales_ach1)


# Iteration 2
# Remove outliers 
final_data2 <- final_data1[-c(5243,20477,8560,9800,9591,9801,20116,5356,9163,5369),] 

# Prepare model 
Sales_ach2 <- lm(ACH_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+TARGET_IN_EA, data=final_data2)
summary(Sales_ach2)



plot(Sales_ach2) 
residualPlots(Sales_ach2)
qqPlot(Sales_ach2)
influenceIndexPlot(Sales_ach2)

#check outliers
outlierTest(Sales_ach2)

# Iteration 3
# Remove outliers 
final_data3 <- final_data2[-c(20477,8560,9800,9801,9591,20116,5356,5369,9163,4078),] 


# Prepare model 
Sales_ach3 <- lm(ACH_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+TARGET_IN_EA, data=final_data3)
summary(Sales_ach3)

# Final Prediction targets for computer from historical data
ACH_IN_EA <- predict(Sales_ach3,validation_data)

#Prepare valiadtion data with achievement 
validation_data1 <- cbind(validation_data,ACH_IN_EA)


## Full Model Building Process
set.seed(250)
Sales_target <- lm(TARGET_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+ACH_IN_EA, data=final_data)
summary(Sales_target)
##Digonastic Plots
## Residual Plots, QQ-Plots, Std. Residual Vs Fitted 
plot(Sales_target)
#Residual Plot
residualPlots(Sales_target)
#QQ-Plots
qqPlot(Sales_target)
#Deletion Diagnostics
influenceIndexPlot(Sales_target)

#check outliers
outlierTest(Sales_target)

# Iteration 1 
# Remove outliers 
final_data4 <- final_data[-c(20091,20138,9590,20090,20117,20137,5243,9800,8560,9801),] 

# Prepare model 
Sales_target1 <- lm(TARGET_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+ACH_IN_EA, data=final_data4)
summary(Sales_target1)


plot(Sales_target1 ) 
residualPlots(Sales_target1)
qqPlot(Sales_target1 )
influenceIndexPlot(Sales_target1)

#check outliers
outlierTest(Sales_target1)

# Iteration 2 
# Remove outliers 
final_data5 <- final_data4[-c(5259,20116,8833,5369,8601,5356,20477,8789,20433,17907),] 

# Prepare model 
Sales_target2 <- lm(TARGET_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+ACH_IN_EA, data=final_data5)
summary(Sales_target2)


plot(Sales_target2 ) 
residualPlots(Sales_target2)
qqPlot(Sales_target2)
influenceIndexPlot(Sales_target2)


#check outliers
outlierTest(Sales_target2)

#Iteration 3 
# Remove outliers 
final_data6 <- final_data5[-c(5259,20116,8833,5369,8601,5356,20477,8789,20433,17907),] 

# Prepare model 
Sales_target2 <- lm(TARGET_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+ACH_IN_EA, data=final_data6)
summary(Sales_target2)


# Target Prediction for feb month 
predict_target <- predict(Sales_target2,validation_data1)
validation_data2 <- cbind(validation_data1,predict_target)

# Testing accuracy 
rmse <- RMSE(predict_target,validation_data1$TARGET_IN_EA)
R_square <- c(1-rmse)
R_square


