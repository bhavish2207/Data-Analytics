## IST 707 Data Analytics HOMEWORK 1

setwd("C:/Users/bhavi/OneDrive/Desktop/SYR ADS/Sem 2/IST_707_Data_Analytics/HW")
getwd
emp_attr <- read.csv("employee_attrition.csv")


getmode <- function(x) {       ### Writing a function to obtain mode (most frequently occuring value)
  
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]    
                      ### Match returns the first position of occurence of every element of 'x' in uniq
                      ### These position match indexes are tabulated which generates number of times each index is present
                      ### which.max provides the index of the maximum occuring match, i.e. the mode
}

emp_attr[emp_attr == ""] <-NA   ## Storing all missing values as NA

sum(is.na(emp_attr))   ## 11 NAs in the table

sum(is.na(emp_attr$Gender))   ## 1 NA in the Gender column
emp_attr$Gender[is.na(emp_attr$Gender)] <- getmode(emp_attr$Gender)    ## Replace Gender NA with Mode of the column

sum(is.na(emp_attr$OverTime))   ## 1 NA in the OverTime column
emp_attr$OverTime[is.na(emp_attr$OverTime)] <- getmode(emp_attr$OverTime)   ## Replace OverTime NA with mode

sum(is.na(emp_attr$DistanceFromHome))    ## 2 NAs in DistanceFromHome
emp_attr$DistanceFromHome[is.na(emp_attr$DistanceFromHome)] <- mean(emp_attr$DistanceFromHome, na.rm = TRUE) 
                                                                ## Replace NA with mean

sum(is.na(emp_attr$JobLevel))   ## 1 NA in JobLevel column
emp_attr$JobLevel[is.na(emp_attr$JobLevel)] <- getmode(emp_attr$JobLevel)   ## Replace with mode

sum(is.na(emp_attr$PercentSalaryHike))            ## 1 NA in percent salary hike
emp_attr$PercentSalaryHike[is.na(emp_attr$PercentSalaryHike)] <- mean(emp_attr$PercentSalaryHike, na.rm = TRUE)
                                                  ## Replace NA with mean

sum(is.na(emp_attr$PerformanceRating))          ## 1 NA in PerformanceRating
emp_attr$PerformanceRating[is.na(emp_attr$PerformanceRating)] <- getmode(emp_attr$PerformanceRating)
                                                      ### Replace NA with mode

sum(is.na(emp_attr$RelationshipSatisfaction))  ## 1 NA in RelationshipSatisfaction
emp_attr$RelationshipSatisfaction[is.na(emp_attr$RelationshipSatisfaction)] <- getmode(emp_attr$RelationshipSatisfaction)
                                                         ### Replace NA with mode

sum(is.na(emp_attr$TotalWorkingYears))       ## 2 NA in Total Working Years
emp_attr$TotalWorkingYears[is.na(emp_attr$TotalWorkingYears)] <- median(emp_attr$TotalWorkingYears, na.rm = TRUE)
                                                      ### Replace NA with median

sum(is.na(emp_attr$YearsSinceLastPromotion))    ## 1 NA in YearsSinceLastPromotion
emp_attr$YearsSinceLastPromotion[is.na(emp_attr$YearsSinceLastPromotion)] <- median(emp_attr$YearsSinceLastPromotion, na.rm = TRUE)
                                                    ### Replace NA with median


### Binning of numeric continuous variables to ordinal categorical variables for EDA & ARM

emp_attr_categorical_variables<-emp_attr


### HISTOGRAMS on numeric columns to identify Outliers and look at range/spread
  ## HISTOGRAM of AGE column. ## Mean aproximately = Median, hence normal distribution

hist(emp_attr_categorical_variables$Age)
abline(v=mean(emp_attr_categorical_variables$Age),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$Age),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))

  ## HISTOGRAM of Daily Rate column. ## Mean = Median, with Uniform distribution

hist(emp_attr_categorical_variables$DailyRate)
abline(v=mean(emp_attr_categorical_variables$DailyRate),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$DailyRate),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))

## HISTOGRAM of DistanceFromHome column. ## Mean slightly > Median, hence right skewed distribution

hist(emp_attr_categorical_variables$DistanceFromHome, breaks = 50)
abline(v=mean(emp_attr_categorical_variables$DistanceFromHome),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$DistanceFromHome),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))

## HISTOGRAM of HourlyRate column. ## Mean = Median, with uniform distribution

hist(emp_attr_categorical_variables$HourlyRate)
abline(v=mean(emp_attr_categorical_variables$HourlyRate),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$HourlyRate),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))


## HISTOGRAM of MonthlyIncome column. ## Mean > Median, Right Skewed with possibility of outliers

hist(emp_attr_categorical_variables$MonthlyIncome)
abline(v=mean(emp_attr_categorical_variables$MonthlyIncome),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$MonthlyIncome),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))


## HISTOGRAM of MonthlyRate column. Mean = Median with Uniform Distribution.

hist(emp_attr_categorical_variables$MonthlyRate)
abline(v=mean(emp_attr_categorical_variables$MonthlyRate),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$MonthlyRate),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))

## HISTOGRAM of NumCompaniesWorked column. Mean > Median with right skewed Distribution.

hist(emp_attr_categorical_variables$NumCompaniesWorked)
abline(v=mean(emp_attr_categorical_variables$NumCompaniesWorked),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$NumCompaniesWorked),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))

## Histogram of YearsAtCompany column. Mean > Median with right skewed Distribution

hist(emp_attr_categorical_variables$YearsAtCompany)
abline(v=mean(emp_attr_categorical_variables$YearsAtCompany),col = "red",lwd = 4)
abline(v=median(emp_attr_categorical_variables$YearsAtCompany),col = "blue", lwd = 2)
legend(x = "topright", 
       c("Mean", "Median"),
       col = c("red", "blue"),lwd = c(4,2))


## Creating Age Groups

library(stringr)

emp_attr_categorical_variables$age_groups <-cut(emp_attr_categorical_variables$Age, breaks = c(quantile(emp_attr_categorical_variables$Age, probs = c(0,0.25,0.5,0.75,1))),
     labels = c(str_c(quantile(emp_attr_categorical_variables$Age,probs = 0),quantile(emp_attr_categorical_variables$Age,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$Age,probs = 0.25),quantile(emp_attr_categorical_variables$Age,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$Age,probs = 0.5),quantile(emp_attr_categorical_variables$Age,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$Age,probs = 0.75),quantile(emp_attr_categorical_variables$Age,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$age_groups<-as.factor(emp_attr_categorical_variables$age_groups)


## Creating Daily Rate Groups
emp_attr_categorical_variables$DailyRate_groups <-cut(emp_attr_categorical_variables$DailyRate, breaks = c(quantile(emp_attr_categorical_variables$DailyRate, probs = c(0,0.25,0.5,0.75,1))),
  labels = c(str_c(quantile(emp_attr_categorical_variables$DailyRate,probs = 0),quantile(emp_attr_categorical_variables$DailyRate,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$DailyRate,probs = 0.25),quantile(emp_attr_categorical_variables$DailyRate,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$DailyRate,probs = 0.5),quantile(emp_attr_categorical_variables$DailyRate,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$DailyRate,probs = 0.75),quantile(emp_attr_categorical_variables$DailyRate,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$DailyRate_groups<-as.factor(emp_attr_categorical_variables$DailyRate_groups)


## Creating DistanceFromHome Groups
emp_attr_categorical_variables$DistanceFromHome_groups <-cut(emp_attr_categorical_variables$DistanceFromHome, breaks = c(quantile(emp_attr_categorical_variables$DistanceFromHome, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0),quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0.25),quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0.5),quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0.75),quantile(emp_attr_categorical_variables$DistanceFromHome,probs = 0.99),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$DistanceFromHome_groups <- as.factor(emp_attr_categorical_variables$DistanceFromHome_groups)


## Creating HourlyRate Groups
emp_attr_categorical_variables$HourlyRate_groups <- cut(emp_attr_categorical_variables$HourlyRate,breaks = c(quantile(emp_attr_categorical_variables$HourlyRate, probs = c(0,0.25,0.5,0.75,1))),
  labels = c(str_c(quantile(emp_attr_categorical_variables$HourlyRate,probs = 0),quantile(emp_attr_categorical_variables$HourlyRate,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$HourlyRate,probs = 0.25),quantile(emp_attr_categorical_variables$HourlyRate,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$HourlyRate,probs = 0.5),quantile(emp_attr_categorical_variables$HourlyRate,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$HourlyRate,probs = 0.75),quantile(emp_attr_categorical_variables$HourlyRate,probs = 1),sep = " to ")), right = FALSE,include.lowest=TRUE)
emp_attr_categorical_variables$HourlyRate_groups <- as.factor(emp_attr_categorical_variables$HourlyRate_groups)


## Creating MonthlyIncome Groups
emp_attr_categorical_variables$MonthlyIncome_groups <- cut(emp_attr_categorical_variables$MonthlyIncome,breaks = c(quantile(emp_attr_categorical_variables$MonthlyIncome, probs = c(0,0.25,0.5,0.75,1))),
  labels = c(str_c(quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 0),quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 0.25),quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 0.5),quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 0.75),quantile(emp_attr_categorical_variables$MonthlyIncome,probs = 1),sep = " to ")), right = FALSE,include.lowest=TRUE)
emp_attr_categorical_variables$MonthlyIncome_groups <- as.factor(emp_attr_categorical_variables$MonthlyIncome_groups)


## Creating MonthlyRate Groups
emp_attr_categorical_variables$MonthlyRate_groups <- cut(emp_attr_categorical_variables$MonthlyRate,breaks = c(quantile(emp_attr_categorical_variables$MonthlyRate, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$MonthlyRate,probs = 0),quantile(emp_attr_categorical_variables$MonthlyRate,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$MonthlyRate,probs = 0.25),quantile(emp_attr_categorical_variables$MonthlyRate,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$MonthlyRate,probs = 0.5),quantile(emp_attr_categorical_variables$MonthlyRate,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$MonthlyRate,probs = 0.75),quantile(emp_attr_categorical_variables$MonthlyRate,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$MonthlyRate_groups <- as.factor(emp_attr_categorical_variables$MonthlyRate_groups)


## Creating NumCompaniesWorked Groups
emp_attr_categorical_variables$NumCompaniesWorked_groups <- cut(emp_attr_categorical_variables$NumCompaniesWorked,breaks = c(quantile(emp_attr_categorical_variables$NumCompaniesWorked, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 0),quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 0.25),quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 0.5),quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 0.75),quantile(emp_attr_categorical_variables$NumCompaniesWorked,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$NumCompaniesWorked_groups <- as.factor(emp_attr_categorical_variables$NumCompaniesWorked_groups)


## Creating PercentSalaryHike Groups
emp_attr_categorical_variables$PercentSalaryHike_groups <- cut(emp_attr_categorical_variables$PercentSalaryHike,breaks = c(quantile(emp_attr_categorical_variables$PercentSalaryHike, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 0),quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 0.25),quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 0.5),quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 0.75),quantile(emp_attr_categorical_variables$PercentSalaryHike,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$PercentSalaryHike_groups <- as.factor(emp_attr_categorical_variables$PercentSalaryHike_groups)


## Creating TotalWorkingYears Groups
emp_attr_categorical_variables$TotalWorkingYears_groups <- cut(emp_attr_categorical_variables$TotalWorkingYears,breaks = c(quantile(emp_attr_categorical_variables$TotalWorkingYears, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0),quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0.25),quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0.5),quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0.75),quantile(emp_attr_categorical_variables$TotalWorkingYears,probs = 0.99),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$TotalWorkingYears_groups <- as.factor(emp_attr_categorical_variables$TotalWorkingYears_groups)


## Creating YearsAtCompany Groups
emp_attr_categorical_variables$YearsAtCompany_groups <- cut(emp_attr_categorical_variables$YearsAtCompany,breaks = c(quantile(emp_attr_categorical_variables$YearsAtCompany, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 0),quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 0.25),quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 0.5),quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 0.75),quantile(emp_attr_categorical_variables$YearsAtCompany,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$YearsAtCompany_groups <- as.factor(emp_attr_categorical_variables$YearsAtCompany_groups)


## Creating YearsInCurrentRole Groups
emp_attr_categorical_variables$YearsInCurrentRole_groups <- cut(emp_attr_categorical_variables$YearsInCurrentRole,breaks = c(quantile(emp_attr_categorical_variables$YearsInCurrentRole, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 0),quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 0.25),quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 0.5),quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 0.75),quantile(emp_attr_categorical_variables$YearsInCurrentRole,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$YearsInCurrentRole_groups <- as.factor(emp_attr_categorical_variables$YearsInCurrentRole_groups)

## Creating YearsSinceLastPromotion Groups

emp_attr_categorical_variables$YearsSinceLastPromotion_groups <- cut(emp_attr_categorical_variables$YearsSinceLastPromotion,breaks = c(quantile(emp_attr_categorical_variables$YearsSinceLastPromotion, probs = c(0,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$YearsSinceLastPromotion,probs = 0),quantile(emp_attr_categorical_variables$YearsSinceLastPromotion,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsSinceLastPromotion,probs = 0.5),quantile(emp_attr_categorical_variables$YearsSinceLastPromotion,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsSinceLastPromotion,probs = 0.75),quantile(emp_attr_categorical_variables$YearsSinceLastPromotion,probs = 1),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$YearsSinceLastPromotion_groups <- as.factor(emp_attr_categorical_variables$YearsSinceLastPromotion_groups)


## Creating YearsWithCurrManager Groups
emp_attr_categorical_variables$YearsWithCurrManager_groups <- cut(emp_attr_categorical_variables$YearsWithCurrManager,breaks = c(quantile(emp_attr_categorical_variables$YearsWithCurrManager, probs = c(0,0.25,0.5,0.75,1))),
labels = c(str_c(quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0),quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0.25),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0.25),quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0.5),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0.5),quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0.75),sep = " to "),str_c(quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0.75),quantile(emp_attr_categorical_variables$YearsWithCurrManager,probs = 0.99),sep = " to ")), right = FALSE, include.lowest=TRUE)
emp_attr_categorical_variables$YearsWithCurrManager_groups <- as.factor(emp_attr_categorical_variables$YearsWithCurrManager_groups)


### CAN EXCLUDE EmployeeNumber, EmployeeCount, Over18 & StandardHours variables as they have 0 variance
emp_attr_categorical_variables$EmployeeNumber <- NULL
emp_attr_categorical_variables$EmployeeCount <- NULL
emp_attr_categorical_variables$Over18 <- NULL
emp_attr_categorical_variables$StandardHours <- NULL

#View(emp_attr_categorical_variables)

### PERFORM EDA to identify interesting trends in data

library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)


### HYPOTHESIS
### 1. Attrition Rate is very low amongst married/divorced employees and very high amongst single employees

  df_marital_status <- emp_attr_categorical_variables %>%                 ##
  group_by(MaritalStatus,Attrition) %>%
  summarise(count = n()) %>%
    spread(Attrition,count) %>%
    ungroup %>%
    transmute(MaritalStatus=MaritalStatus,attr_rate = Yes/(Yes+No))
  df_marital_status
  
colnames(df_marital_status) <- c("Marital_Status","Attrition_Rate")
marital_status_attrition_plot <- ggplot(df_marital_status,aes(x=Marital_Status,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Marital Status")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by Marital Status")
marital_status_attrition_plot

### 2. Attrition Rate is very low amongst Non Travellers and higher amongst frequent travellers

df_travel <- emp_attr_categorical_variables %>%             
  group_by(BusinessTravel,Attrition) %>%
  summarise(count = n()) %>%
  spread(Attrition,count) %>%
  ungroup %>%
  transmute(BusinessTravel=BusinessTravel,attr_rate = Yes/(Yes+No))
df_travel
colnames(df_travel) <- c("BusinessTravel","Attrition_Rate")
business_travel_attrition_plot <- ggplot(df_travel,aes(x=BusinessTravel,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Travel Rate")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by Travel Rate")
business_travel_attrition_plot

### 3. Attrition rate is lower amongst employees living near by

df_distance <- emp_attr_categorical_variables %>%             
  group_by(DistanceFromHome_groups,Attrition) %>%
  summarise(count = n()) %>%
  spread(Attrition,count) %>%
  ungroup %>%
  transmute(DistanceFromHome_groups=DistanceFromHome_groups,attr_rate = Yes/(Yes+No))

colnames(df_distance) <- c("DistanceFromHome_groups","Attrition_Rate")
distance_attrition_plot <- ggplot(df_distance,aes(x=DistanceFromHome_groups,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Distance from Home groups")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by distance from home groups")
distance_attrition_plot

### 4. Attrition Rate is lower amongst high environment satisfaction employees

df_envt <- emp_attr_categorical_variables %>%             
  group_by(EnvironmentSatisfaction,Attrition) %>%
  summarise(count = n()) %>%
  spread(Attrition,count) %>%
  ungroup %>%
  transmute(EnvironmentSatisfaction=EnvironmentSatisfaction,attr_rate = Yes/(Yes+No))
df_envt
colnames(df_envt) <- c("EnvironmentSatisfaction","Attrition_Rate")
envt_attrition_plot <- ggplot(df_envt,aes(x=EnvironmentSatisfaction,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Environment Satisfaction")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by environment satisfaction")
envt_attrition_plot

### 5. Attrition Rate is lower amongst high MonthlyIncome_groups employees

df_monthly_income <- emp_attr_categorical_variables %>%             
  group_by(MonthlyIncome_groups,Attrition) %>%
  summarise(count = n()) %>%
  spread(Attrition,count) %>%
  ungroup %>%
  transmute(MonthlyIncome_groups=MonthlyIncome_groups,attr_rate = Yes/(Yes+No))

colnames(df_monthly_income) <- c("MonthlyIncome_groups","Attrition_Rate")
monthly_income_attrition_plot <- ggplot(df_monthly_income,aes(x=MonthlyIncome_groups,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Monthly Income Groups")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by monthly income groups")
monthly_income_attrition_plot

### 5. Attrition Rate is higher amongst employees with low years at company

df_yearsatcompany <- emp_attr_categorical_variables %>%             
  group_by(YearsAtCompany_groups,Attrition) %>%
  summarise(count = n()) %>%
  spread(Attrition,count) %>%
  ungroup %>%
  transmute(YearsAtCompany_groups=YearsAtCompany_groups,attr_rate = Yes/(Yes+No))

colnames(df_yearsatcompany) <- c("YearsAtCompany_groups","Attrition_Rate")
yearsatcompany_plot <- ggplot(df_yearsatcompany,aes(x=YearsAtCompany_groups,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Years at company Groups")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by years at company groups")
yearsatcompany_plot

### 6. Attrition Rate is higher amongst employees with lower job levels

df_joblevel <- emp_attr_categorical_variables %>%             
  group_by(JobLevel,Attrition) %>%
  summarise(count = n()) %>%
  spread(Attrition,count) %>%
  ungroup %>%
  transmute(JobLevel=JobLevel,attr_rate = Yes/(Yes+No))

colnames(df_joblevel) <- c("JobLevel","Attrition_Rate")
JobLevel_plot <- ggplot(df_joblevel,aes(x=JobLevel,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Job Level")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by Job Level")
JobLevel_plot

### 7. Attrition Rate is higher amongst lower age group employees

df_age_groups <- emp_attr_categorical_variables %>%             
  group_by(age_groups,Attrition) %>%
  summarise(count = n()) %>%
  spread(Attrition,count) %>%
  ungroup %>%
  transmute(age_groups=age_groups,attr_rate = Yes/(Yes+No))

colnames(df_age_groups) <- c("AgeGroups","Attrition_Rate")
age_group_plot <- ggplot(df_age_groups,aes(x=AgeGroups,y=Attrition_Rate))+
  geom_col(show.legend=TRUE,position = "dodge")+
  xlab("Age Groups")+ ylab("Attrition Rate")+
  ggtitle("Employee Attrition Rate by Age Groups")
age_group_plot

### Removing Continuous numeric variables for running ARM
emp_attr_arm <- emp_attr_categorical_variables
emp_attr_arm$Age<-NULL
emp_attr_arm$DailyRate<-NULL
emp_attr_arm$DistanceFromHome<-NULL
emp_attr_arm$HourlyRate<-NULL
emp_attr_arm$MonthlyIncome<-NULL
emp_attr_arm$MonthlyRate<-NULL
emp_attr_arm$NumCompaniesWorked<-NULL
emp_attr_arm$PercentSalaryHike<-NULL
emp_attr_arm$TotalWorkingYears<-NULL
emp_attr_arm$YearsAtCompany<-NULL
emp_attr_arm$YearsInCurrentRole<-NULL
emp_attr_arm$YearsSinceLastPromotion<-NULL
emp_attr_arm$YearsWithCurrManager<-NULL
emp_attr_arm$Education <-as.factor(emp_attr_arm$Education)
emp_attr_arm$EnvironmentSatisfaction <- as.factor(emp_attr_arm$EnvironmentSatisfaction)
emp_attr_arm$JobInvolvement <- as.factor(emp_attr_arm$JobInvolvement)
emp_attr_arm$JobLevel <- as.factor(emp_attr_arm$JobLevel)
emp_attr_arm$JobSatisfaction <- as.factor(emp_attr_arm$JobSatisfaction)
emp_attr_arm$PerformanceRating <- as.factor(emp_attr_arm$PerformanceRating)
emp_attr_arm$RelationshipSatisfaction <- as.factor(emp_attr_arm$RelationshipSatisfaction)
emp_attr_arm$StockOptionLevel <- as.factor(emp_attr_arm$StockOptionLevel)
emp_attr_arm$TrainingTimesLastYear <- as.factor(emp_attr_arm$TrainingTimesLastYear)
emp_attr_arm$WorkLifeBalance <- as.factor(emp_attr_arm$WorkLifeBalance)


## ARM
#install.packages("arules")
library(arules)  

#install.packages("arulesViz")
library(arulesViz)

emp_attr_armX <- as(emp_attr_arm,"transactions")

View(emp_attr_arm)

### RULES for ATTRITION "NO", i.e. employees who have not attritioned
ruleset_attrition_no <- apriori(emp_attr_armX,                                                
                   parameter = list(support = 0.1, confidence = 0.5),      
                   appearance = list(default = "lhs", rhs=("Attrition=No"))) 

### RULES for ATTRITION "Yes", i.e. employees who have attritioned
ruleset_attrition_yes <- apriori(emp_attr_armX,                                                
                                parameter = list(support = 0.1, confidence = 0.5),      
                                appearance = list(default = "lhs", rhs=("Attrition=Yes")))


### B.	Using the inspect() command, review both the "ruleset". 

inspect(ruleset)

### C.	Experiment with the interactive ruleset interface by running the inspectDT() command. 

inspectDT(ruleset_attrition_no)

inspectDT(ruleset_attrition_yes)

rsconnect::setAccountInfo(name='bhavishkumar', token='E496BCE34B1133CBF3AD74B279ADBA64', secret='Wb+XRYXSKpEGuOtB/sDQXd3VZkSVNqFZ4bttVfn0')


View(test_df)
