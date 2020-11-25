### Data download ###
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(randomForest)) install.packages("randomForest")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(randomForest)

dl <- tempfile()
download.file("https://github.com/jdominguez-github/Capstone_CYOP_German_Credit_Risk/raw/master/german_credit_data.csv",dl)

credit <- fread(text = gsub(",", ",", readLines(dl)),
      col.names = c("Seq","Age","Sex","Job","Housing","Saving_accounts","Checking_account","Credit_amount",
      "Duration","Purpose","Risk"))

### Data pre-processing

# Search for NA values over all columns of the dataset
sapply(credit,function(x) sum(is.na(x)))

# Number of NA Values
# Saving_accounts: 183
# Checking_account: 394 

# For NA in the Saving_accounts column the most common value in the dataset, which is "little", will be assigned
table(credit$Saving_accounts)
credit[is.na(credit$Saving_accounts)]$Saving_accounts <- "little"

# For NA in the Checking_accounts column the most common value in the dataset, which is "little", will be assigned
table(credit$Checking_account)
credit[is.na(credit$Checking_account)]$Checking_account <- "little"

# Confirm that no NA values remain in the dataset
sapply(credit,function(x) sum(is.na(x)))

# Convert outcome variable "Risk" into factor
credit <- credit %>% 
  mutate(Risk = factor(Risk))

# Convert rest of categorical features into factor
credit <- credit %>% 
  mutate(Sex = factor(Sex), Job = factor(Job), Housing = factor(Housing), Saving_accounts = factor(Saving_accounts), 
         Checking_account = factor(Checking_account),Purpose = factor(Purpose))


### Create training set and test set

# Split credit data set into training set (80%) and test set (20%)
test_index <- createDataPartition(credit$Risk,1,0.2,list=FALSE)

credit_test <- credit[test_index, ]
credit_train <- credit[-test_index, ]


### credit dataset overview ### 

# General structure of the dataset
str(credit)

# Summary stats
summary(credit)

# Sample of first 10 rows
head(credit)

# Total number of rows
c("Number of projects" = nrow(credit))


### credit data exploration ### 

#Number of good / bad credit risks
cont_risk <- table(credit$Risk)
cont_risk

# Proportion of good credit risks
c("Proportion of succesful projects" = mean(credit$Risk=="good"))

# Credit risk stacked bar plot
cont_succ <- table(credit$Risk)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Risk") +
  ggtitle("Credit risk stacked bar plot")

# Sex stacked bar plot
cont_succ <- table(credit$Sex)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Sex") +
  ggtitle("Sex stacked bar plot")

# Job stacked bar plot
cont_succ <- table(credit$Job)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Job") +
  ggtitle("Job stacked bar plot")

# Housing stacked bar plot
cont_succ <- table(credit$Housing)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Housing") +
  ggtitle("Housing stacked bar plot")

# Saving_accounts stacked bar plot
cont_succ <- table(credit$Saving_accounts)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Saving accounts") +
  ggtitle("Saving_accounts stacked bar plot")

# Checking_account stacked bar plot
cont_succ <- table(credit$Checking_account)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Checking account") +
  ggtitle("Checking_account stacked bar plot")

# Purpose stacked bar plot
cont_succ <- table(credit$Purpose)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Purpose") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Purpose stacked bar plot")

# Age boxplot
credit %>% ggplot(aes(Risk,Age,fill=Risk)) + 
  geom_boxplot() +
  ggtitle("Age boxplot")

# Credit amount boxplot
credit %>% ggplot(aes(Risk,Credit_amount,fill=Risk)) + 
  geom_boxplot() +
  ggtitle("Credit amount boxplot")

# Duration boxplot
credit %>% ggplot(aes(Risk,Duration,fill=Risk)) + 
  geom_boxplot() +
  ggtitle("Duration boxplot")

# Age distribution
credit %>% ggplot(aes(Age)) + 
  geom_histogram(fill="black") +
  ggtitle("Age distribution")

# Credit amount distribution
credit %>% ggplot(aes(Credit_amount)) + 
  geom_histogram(fill="black") +
  ggtitle("Credit amount distribution")

# Duration distribution
credit %>% ggplot(aes(Duration)) + 
  geom_histogram(fill="black") +
  ggtitle("Duration distribution")

# Good risk rate per Sex
credit %>% 
  group_by(Sex) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(Sex,-avg_good),avg_good,fill=Sex)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Sex") +
  ggtitle("Good risk rate per Sex")
# Good risk rate per Job
credit %>% 
  group_by(Job) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(Job,-avg_good),avg_good,fill=Job)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Job") +
  ggtitle("Good risk rate per Job")
# Good risk rate per Housing
credit %>% 
  group_by(Housing) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(Housing,-avg_good),avg_good,fill=Housing)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Housing") +
  ggtitle("Good risk rate per Housing")
# Good risk rate per Saving_accounts
credit %>% 
  group_by(Saving_accounts) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(Saving_accounts,-avg_good),avg_good,fill=Saving_accounts)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Saving accounts") +
  ggtitle("Good risk rate per Saving_accounts")
# Good risk rate per Checking_account
credit %>% 
  group_by(Checking_account) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(Checking_account,-avg_good),avg_good,fill=Checking_account)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Checking account") +
  ggtitle("Good risk rate per Checking_account")
# Good risk rate per Credit_amount
credit %>% 
  mutate(amount_1k = round(Credit_amount/1000)) %>% 
  group_by(amount_1k) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(amount_1k,-avg_good),avg_good,fill=amount_1k)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Credit amount (in k DM)") +
  ggtitle("Good risk rate per Credit_amount")
credit %>% 
  mutate(amount_1k = round(Credit_amount/1000)) %>% 
  group_by(amount_1k) %>% 
  summarize(avg = mean(Risk=="good")) %>%
  ggplot(aes(amount_1k,avg)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Good risk rate per Credit_amount")
# Good risk rate per Duration
credit %>% 
  group_by(Duration) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(Duration,-avg_good),avg_good,fill=Duration)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Duration") +
  ggtitle("Good risk rate per Duration")
credit %>% 
  group_by(Duration) %>% 
  summarize(avg = mean(Risk=="good")) %>%
  ggplot(aes(Duration,avg)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Good risk rate per Duration")
# Good risk rate per Purpose
credit %>% 
  group_by(Purpose) %>% 
  summarize(avg_good = mean(Risk=="good")) %>% 
  ggplot(aes(reorder(Purpose,-avg_good),avg_good,fill=Purpose)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Purpose") +
  ggtitle("Good risk rate per Purpose")





# Lineas temporales: exito por años, n. proyectos por años, categorias con mas proyectos por años,
# etc.


### Model implementation ### 

#------------------------
### Logistic regresion
#------------------------

now()

trainctrl <- trainControl(verboseIter = TRUE)

# fit_glm <- train(Risk ~ .,
#                  method="glm", data = credit_train, trControl = trainctrl)

fit_glm <- train(Risk ~ Credit_amount+Duration,
                 method="glm", data = credit_train, trControl = trainctrl)

y_hat_glm <- predict(fit_glm, credit_test, type = "raw")

confusionMatrix(y_hat_glm,credit_test$Risk)$overall[["Accuracy"]]
confusionMatrix(y_hat_glm,credit_test$Risk)

now()

Accuracy : 0.69

#------------------------
### Decision tree
#------------------------
now()

fit_rpart <- train(Risk ~ ., 
                   data = credit_train,method="rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), trControl = trainctrl)

ggplot(fit_rpart)
fit_rpart$bestTune

# Predicción y evaluación
confusionMatrix(predict(fit_rpart, credit_test, type = "raw"),
                credit_test$Risk)$overall["Accuracy"]
confusionMatrix(predict(fit_rpart, credit_test, type = "raw"),
                credit_test$Risk)

# Visualización del árbol
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel, cex = 0.75)

now()

Accuracy : 0.68


#------------------------
### Random forest
#------------------------
# idx <- createDataPartition(kickstarter_train$success,1,0.2,list=FALSE)
# kick_kk <- kickstarter[idx, ]

now()

trainctrl <- trainControl(verboseIter = TRUE, number = 25)

# Ajuste de parámetros
fit_rf <- train(Risk ~ .
                , data = credit_train,method="rf",
                tuneGrid = data.frame(mtry = seq(1,15)),ntree=1000, trControl = trainctrl)

print(fit_rf)
ggplot(fit_rf)
fit_rf$bestTune

# Estimación de la importancia de las variables
imp <- varImp(fit_rf)
imp

# Predicción y evaluación
confusionMatrix(predict(fit_rf, credit_test, type = "raw"),
                credit_test$Risk)$overall["Accuracy"]
confusionMatrix(predict(fit_rf, credit_test, type = "raw"),
                credit_test$Risk)

now()

# Balanced Accuracy : 0.7277

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0  9000  3347
# 1  6049 20115
# 
# Accuracy : 0.756           
# 95% CI : (0.7517, 0.7603)
# No Information Rate : 0.6092          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4705          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.5980          
#             Specificity : 0.8573          
#          Pos Pred Value : 0.7289          
#          Neg Pred Value : 0.7688          
#              Prevalence : 0.3908          
#          Detection Rate : 0.2337          
#    Detection Prevalence : 0.3206          
#       Balanced Accuracy : 0.7277          
#                                           
#        'Positive' Class : 0   


#------------------------
### SVM
#------------------------

now()
# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=10, repeats=3, verboseIter = TRUE)

# Fit the model 
svm1 <- train(Risk ~ ., data = credit_train, 
              method = "svmLinear", trControl = train_control)
#View the model
svm1

now()




now()



control <- trainControl(method = "cv", number = 10, p = .9, verboseIter = TRUE)

train_knn_cv <- train(Risk ~ ., method = "knn", 
                      data = credit_train,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)


# Observamos cuál es el parámetro más óptimo
ggplot(train_knn_cv)
train_knn_cv$bestTune


confusionMatrix(predict(train_knn_cv, credit_test, type = "raw"),
                credit_test$Risk)$overall["Accuracy"]


now()