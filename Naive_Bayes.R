install.packages("readr")
install.packages("naivebayes")
install.packages("ggplot2")
install.packages("psych")
install.packages("dplyr")
library(readr)
library(naivebayes)
library(ggplot2)
library(psych)
library(dplyr)

#Importing the Data Set
salary_train <- read.csv("C:\\Users\\91755\\Desktop\\1Assignments\\10 - Naive Bays\\SalaryData_Train.csv")
salary_test <- read.csv("C:\\Users\\91755\\Desktop\\1Assignments\\10 - Naive Bays\\SalaryData_Test.csv")
attach(salary_train)
View(salary_train)
head(salary_train)

#EDA and Statistical Analysis
summary(salary_train)
str(salary_train)

table(salary_train$Salary)
round(prop.table(table(salary_train$Salary))*100, 1)

sum(is.na(salary_train))
sum(is.na(salary_test))

salary_train$educationno <- factor(salary_train$educationno)
salary_test$educationno <- factor(salary_test$educationno)

#Data Visualization
salary_train %>%
  ggplot(aes(x=Salary, y=age, fill=Salary))+
  geom_boxplot()+
  ggtitle("Box Plot")
plot(salary_train$workclass, salary_train$Salary)
plot(salary_train$education, salary_train$Salary)
plot(salary_train$occupation, salary_train$Salary)
plot(salary_train$maritalstatus, salary_train$Salary)

#Model Building
model <- naive_bayes(salary_train$Salary~age+occupation+relationship+capitalgain+capitalloss+hoursperweek, data = salary_train, usekernel = T)
model
plot(model)
pred <- predict(model, salary_test) #Accuarcy=79.66% 
pred
confusionMatrix(table(pred, salary_test$Salary))
table(salary_train$Salary)
