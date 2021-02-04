


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


##Training a model on the data ----
#Train the neuralnet model
set.seed(250)
install.packages("neuralnet")
library(neuralnet)

#Simple ANN with only a single hidden neuron
sales_ach_model <- neuralnet(ACH_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+TARGET_IN_EA,
                                data = final_data, hidden = 4, linear.output = FALSE )


#Visualize the network topology
plot(sales_ach_model)

##Evaluating model performance ----
#Obtain model results
model_results <- compute(sales_ach_model,validation_data)
#Obtain predicted achievements values
ACH_IN_EA <- model_results$net.result


# Add predicted Achievement for January month in validation dataset 
validation_data1 <- cbind(validation_data,ACH_IN_EA)


##Training a model on the data ----
#Train the neuralnet model
set.seed(250)
install.packages("neuralnet")
library(neuralnet)

#Simple ANN with only a single hidden neuron
sales_target_model <- neuralnet(TARGET_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+ACH_IN_EA,
                                data = final_data, hidden = 4, linear.output = FALSE )


#Visualize the network topology
plot(sales_target_model)

##Evaluating model performance ----
#Obtain model results
model_results <- compute(sales_target_model, validation_data1)
#Obtain predicted profit values
predicted_target <- model_results$net.result
#Examine the correlation between predicted and actual profit values
cor(predicted_target,validation_data1$TARGET_IN_EA)

#Checking the test accuracy of model 
rmse=(sum((validation_data1$TARGET_IN_EA-model_results$net.result)^2)/nrow(validation_data1))^0.5
Rsquare <- c(1-rmse)
Rsquare


##Improving model performance ----
#A more complex neural network topology with 6 hidden neurons
sales_target_model1 <- neuralnet(TARGET_IN_EA~PROD_CD+SLSMAN_CD+PLAN_MONTH+ACH_IN_EA,
                                 data = final_data, hidden = 6, linear.output = FALSE )


#Visualize the network topology
windows();
plot(sales_target_model1)

##Evaluating model performance ----
#Obtain model results
model_results1 <- compute(sales_target_model1, validation_data1)
#Obtain predicted profit values
predicted_target1 <- model_results1$net.result
#Examine the correlation between predicted and actual profit values
cor(predicted_target1,validation_data1$TARGET_IN_EA)

#Checking the testing accuracy of model 
rmse1=(sum((validation_data1$TARGET_IN_EA-model_results1$net.result)^2)/nrow(validation_data1))^0.5
Rsquare1 <- c(1-rmse1)
Rsquare1


#Obtain model results
model_results2 <- compute(sales_target_model1, final_data)
#Obtain predicted profit values
predicted_target2 <- model_results2$net.result
#Examine the correlation between predicted and actual profit values
cor(predicted_target2,final_data$TARGET_IN_EA)


#Checking the training accuracy of model 
rmse2=(sum((final_data$TARGET_IN_EA-model_results2$net.result)^2)/nrow(final_data))^0.5
Rsquare2 <- c(1-rmse2)
Rsquare2

