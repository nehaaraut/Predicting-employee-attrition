# Attrition and prediction

data <- read.csv("C:/Users/Nehaa/Desktop/Imarticus/DataSet/WA_Fn-UseC_-HR-Employee-Attrition.csv")
#View(data)

dim(data)
str(data)

#library(naniar)
#vis_miss(data) #no null data

summary(data)

# Dropping unnecessary variables

names(data)

library(dplyr)
data1 = select(data, -9, -10, -22, -27) #dropping employee count, employee number, over 18, standard hours

dim(data1)

#___EDA
head(data1, 10)
summary(data1)

#_______________imputation of a categorical variable 
#Barplots of categorical variables

t1 = table(data1$Attrition)
t1 
sum(t1)
barplot(t1, main = "Barplot of Attrition",
        col = c("green","red"),
        xlab = 'Attrition',
        ylab = 'Counts')

t2 = table(data1$BusinessTravel)
t2 
sum(t2)
barplot(t2, main = "Barplot of Business Travel",
        col = c("green","red","blue"),
        xlab = 'Business Travel',
        ylab = 'Counts')

t3 = table(data1$Department)
t3 
sum(t3)
barplot(t3, main = "Barplot of Department",
        col = c("green","red","blue"),
        xlab = 'Department',
        ylab = 'Counts')

t4 = table(data1$EducationField)
t4 
sum(t4)
barplot(t4, main = "Barplot of EducationField",
        col = c("green","red","blue","orange","yellow","pink"),
        xlab = 'EducationField',
        ylab = 'Counts')

t5 = table(data1$Gender)
t5 
sum(t5)
barplot(t5, main = "Barplot of Gender",
        col = c("pink","blue"),
        xlab = 'Gender',
        ylab = 'Counts')

t6 = table(data1$JobRole)
t6 
sum(t6)
barplot(t6, main = "Barplot of Job Role",
        col = c("green","red","blue","orange","yellow","pink","purple","brown","magenta"),
        xlab = 'Jobe Role',
        ylab = 'Counts')

t7 = table(data1$MaritalStatus)
t7 
sum(t7)
barplot(t7, main = "Barplot of Marital Status",
        col = c("orange","green","pink"),
        xlab = 'Marital',
        ylab = 'Counts')

t8 = table(data1$OverTime)
t8 
sum(t8)
barplot(t8, main = "Barplot of Over time",
        col = c("purple","pink"),
        xlab = 'Over time',
        ylab = 'Counts')


#_____cross tabulation
#install.packages("gmodels)
library(gmodels)

CrossTable(data1$Attrition, data1$BusinessTravel)


CrossTable(data1$Attrition, data1$Department)


CrossTable(data1$Attrition, data1$EducationField)


CrossTable(data1$Attrition, data1$Gender)


CrossTable(data1$Attrition, data1$JobRole)


CrossTable(data1$Attrition, data1$MaritalStatus)


CrossTable(data1$Attrition, data1$OverTime)


#__________continuous data

str(data1)

names(data1)
numeric <- data1[,c(1,4,6,7,9,11:13,15,17:19,21:31)]

library(psych)
describe(numeric)

nummatrix = as.matrix(numeric)

heatmap(nummatrix)
#pairs.panels(nummatrix)


#____________Independent sample t test
t.test(data1$DailyRate ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.02, 812.5 vs 750.36

t.test(data1$DistanceFromHome ~ data1$Attrition, var.equal = TRUE)
#p-value =0, 8.91 vs 10.63

t.test(data1$Education ~ data1$Attrition, var.equal = TRUE)
#p-value =0.22, 2.92 vs 2.83

t.test(data1$EnvironmentSatisfaction ~ data1$Attrition, var.equal = TRUE)
#p-value =0, 2.77 vs 2.46

t.test(data1$HourlyRate ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.7931, 65.95 vs 65.57

t.test(data1$JobInvolvement ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 2.77 vs 2.51

t.test(data1$JobLevel ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 2.14 vs 1.63

t.test(data1$JobSatisfaction ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 2.77 vs 2.46

t.test(data1$MonthlyIncome ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 6832.74 vs 4787.09

t.test(data1$MonthlyRate ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.5611, 14265.78 vs 14559.31

t.test(data1$NumCompaniesWorked ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.09, 2.64 vs 2.94

t.test(data1$PercentSalaryHike ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.6056, 15.23 vs 15.09

t.test(data1$PerformanceRating ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.9119, 3.15 vs 3.15

t.test(data1$RelationshipSatisfaction ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.0787, 2.73 vs 2.59

t.test(data1$StockOptionLevel ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 0.845 vs 0.527

t.test(data1$TotalWorkingYears ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 11.86 vs 8.24

t.test(data1$TrainingTimesLastYear ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.02, 2.83 vs 2.62

t.test(data1$WorkLifeBalance ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.01, 2.78 vs 2.65

t.test(data1$YearsAtCompany ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 7.36 vs 5.13

t.test(data1$YearsInCurrentRole ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 4.48 vs 2.90

t.test(data1$YearsSinceLastPromotion ~ data1$Attrition, var.equal = TRUE)
#p-value = 0.20, 2.23 vs 1.94

t.test(data1$YearsWithCurrManager ~ data1$Attrition, var.equal = TRUE)
#p-value = 0, 4.36 vs 2.85

#____________train test data

set.seed(123)


library(caTools)

split = sample.split(data1$Attrition, SplitRatio = 0.70)

# Create training and testing sets
train = subset(data1, split == TRUE)
test = subset(data1, split == FALSE)


prop.table(table(train$Attrition))

prop.table(table(test$Attrition))


#install.packages("C50")
library(C50)

sum(is.na(data1))

data_model <- C5.0(train[-2], train$Attrition)  # -2 represents attrition to be excluded

data_model

summary(data_model)

#______prediction on test data
data_pred <- predict(data_model, test)

library(gmodels)

CrossTable(test$Attrition,
           data_pred,
           prop.r = FALSE,
           prop.c = FALSE,
           dnn = c("Actual","Predicted"))
#  Accuracy
mean(data_pred == test$Attrition)


#______Boosting

data_boost <- C5.0(train[-2],
                   train$Attrition,
                   trials = 5)
                  

data_boost

summary(data_boost)

data_boost_pred <- predict(data_boost, test)

CrossTable(test$Attrition,
           data_boost_pred,
           prop.r = FALSE,
           prop.c = FALSE,
           dnn = c("Actual", "Predicted"))

#__Accuracy
mean(data_boost_pred == test$Attrition)

#_____________Random Forest
#install.packages("randomForest")
library(randomForest)

model1 <- randomForest(Attrition ~ ., data = train, importance = TRUE)
model1

model2 <- randomForest(Attrition ~ ., data = train, importance = TRUE)
model2


predTrain <- predict(model2, train, type = "class")

# Checking classification accuracy
table(predTrain, train$Attrition)  

# Predicting on Test set
predtest <- predict(model2, test, type = "class")

CrossTable(test$Attrition,
           predtest,
           prop.r = FALSE,
           prop.c = FALSE,
           dnn = c("Actual", "Predicted"))

# Checking classification accuracy
mean(predtest == test$Attrition)      






