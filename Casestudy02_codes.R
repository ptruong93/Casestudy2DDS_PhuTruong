
#Import dependencies
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggthemes)
library(naniar)
library(class)
library(caret)
library(plotly)
library(GGally)
library(ggpubr)
library(e1071)
library(gdata)
library(readxl)



# Import data
#train <-  read.csv(file.choose(),header= TRUE)
#predict_attrition <-  read.csv(file.choose(),header= TRUE)
#Predict_Salary <- read.csv(file.choose(),header = TRUE)

train <- read.csv("C:/Users/phu truong/Documents/CaseStudy2-data.csv")
predict_attrition <- read.csv("C:/Users/phu truong/Documents/CaseStudy2CompSet No Attrition.csv")
Predict_Salary <- read_excel("C:/Users/phu truong/Documents/CaseStudy2CompSet No Salary.xlsx")

# Add attrition column to predict_attrition data set
namevector <- c("Attrition")
predict_attrition[ , namevector] <- NA

# Count missing values in each column
s = sapply(train, function(x) sum(is.na(x)))
s

gg_miss_var(train)

str(train)

#Selecting variables that affect attrition at significant level of alpha= 0.1

model_select= lm(as.numeric(train$Attrition) ~ JobSatisfaction+EnvironmentSatisfaction+
            RelationshipSatisfaction+PerformanceRating+JobInvolvement+MonthlyIncome +
              MonthlyRate+PercentSalaryHike+ as.numeric(JobRole)+
              as.numeric(EducationField)+Education, data =train)
summary(model_select)

#Fit the model with significant factors that affect Attrition

model_fit= lm(as.numeric(train$Attrition) ~ JobSatisfaction+EnvironmentSatisfaction+
            JobInvolvement+MonthlyIncome, data =train)
summary(model_fit)

#Visualize data
train %>% ggplot(aes(x = JobSatisfaction, fill = Attrition)) + geom_histogram(stat="count") + ggtitle("Histogram of Jobsatisfaction")

train %>% ggplot(aes(x = EnvironmentSatisfaction, fill = Attrition)) + geom_histogram(stat="count") + ggtitle("Histogram of EnvironmentSatisfaction")

train %>% ggplot(aes(x = JobInvolvement, fill = Attrition)) + geom_histogram(stat="count") + ggtitle("Histogram of JobInvolvement")

train %>% ggplot(aes(x = ID, y= MonthlyIncome, color = Attrition)) + geom_point() + ggtitle("Scatterplot of MonthlyIncome")

train %>% ggplot(aes(x = ID, y= MonthlyIncome, color = Attrition)) + geom_point() + ggtitle("Scatterplot of MonthlyIncome")




# train the model with naive bayes

iterations = 100

masterAcc = matrix(nrow = iterations)

splitPerc = 570/870 

for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(train)[1],round(splitPerc * dim(train)[1]))
  train1 = train[trainIndices,]
  test1 = train[-trainIndices,]
  
  model = naiveBayes(train1[,c("JobSatisfaction","EnvironmentSatisfaction","JobInvolvement","MonthlyIncome")],
                     train1$Attrition)
  table(predict(model,test1[,c("JobSatisfaction","EnvironmentSatisfaction","JobInvolvement","MonthlyIncome")]),
                test1$Attrition)
  CM = confusionMatrix(table(predict(model,test1[,c("JobSatisfaction","EnvironmentSatisfaction","JobInvolvement","MonthlyIncome")]),
                             test1$Attrition))
  masterAcc[j] = CM$overall[1]
}

MeanAcc = colMeans(masterAcc)

MeanAcc

#predict attrition with naive baye classifier
model_nb = naiveBayes(train[,c("JobSatisfaction","EnvironmentSatisfaction","JobInvolvement","MonthlyIncome")],
                      train$Attrition)
results= predict(model_nb,predict_attrition[,c("JobSatisfaction","EnvironmentSatisfaction","JobInvolvement","MonthlyIncome")])
predict_attrition$Attrition <- results
predict_attrition$Attrition
predict = predict_attrition[,c("ID","Attrition")]
summary(predict)

predict_attrition %>% ggplot(aes(x = Attrition, fill = Attrition)) + geom_histogram(stat="count") + ggtitle("Histogram of Prediction results")

#check the accuracy of prediction results 
table(results,test1$Attrition)
CM_predict= confusionMatrix(table(results,test1$Attrition))
CM_predict


# Regression model for predicting Salary
salary.lm <- lm(MonthlyIncome ~ JobLevel + TotalWorkingYears + YearsAtCompany + MonthlyRate
                , data=train) 
summary(salary.lm)

results2 = predict(salary.lm, newdata= Predict_Salary)
Predict_Salary$MonthlyIncome <- results2

# Visualize data
pairs(Predict_Salary[,c("MonthlyIncome","JobLevel","TotalWorkingYears","YearsAtCompany","MonthlyRate")])


# Jobs role specific trend
# Plot the mean values of Jobsatisfaction by each role
dfmean= train %>% 
    group_by(JobRole) %>%
    summarize(mean_Jobs = round(mean(JobSatisfaction),digits=2), mean_salary= round(mean(MonthlyIncome),digits=2), count = n())

p1 <- dfmean %>% ggplot(aes(x = JobRole)) + geom_col(aes( y = mean_Jobs, fill="red")) +
  geom_text(aes(y =mean_Jobs , label = mean_Jobs), fontface = "bold", vjust = 2.4, color = "white", size = 4) + ggtitle("Mean Jobsatisfaction by Job roles")
p1 + rotate_x_text(45)

# Plot the mean values of monthlyincome by each role
p2 <- dfmean %>% ggplot(aes(x = JobRole)) + geom_col(aes( y = mean_salary, fill="red")) +
  geom_text(aes(y =mean_salary , label = mean_salary), fontface = "bold", vjust = 2.4, color = "white", size = 4) + ggtitle("Mean MonthlyIncome by Job roles")

p2 + rotate_x_text(45)


