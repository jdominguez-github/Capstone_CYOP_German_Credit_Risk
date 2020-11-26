### Data ingestion ###
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
download.file("https://github.com/jdominguez-github/Capstone_CYOP_German_Credit_Risk/raw/master/german.data",dl)

credit <- fread(text = gsub(" ", ",", readLines(dl)),
                 col.names = c("Checking_acc_status","Duration","Credit_history","Purpose","Credit_amount","Savings_account",
                               "Current_empl_dur","Installment_rate","Personal_status_Sex","Other_debtors_guarantors",
                               "Residence_since","Property","Age","Other_installment_plans","Housing","N_credits",
                               "Job","N_dependant_people","Telephone","Foreign","Risk"))

# Split credit data set into training set (80%) and test set (20%)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(credit$Risk,1,0.2,list=FALSE)

credit_test <- credit[test_index, ]
credit_train <- credit[-test_index, ]


### Data pre-processing

# Translate original data set codes into something more descriptive for exploratory analysis

credit_trans <- credit

# Checking_acc_status
credit_trans[credit$Checking_acc_status == "A11"]$Checking_acc_status <- "0"
credit_trans[credit$Checking_acc_status == "A12"]$Checking_acc_status <- "0-200"
credit_trans[credit$Checking_acc_status == "A13"]$Checking_acc_status <- ">200"
credit_trans[credit$Checking_acc_status == "A14"]$Checking_acc_status <- "No account"

# Credit_history
credit_trans[credit$Credit_history == "A30"]$Credit_history <- "No credit/Duly paid at other banks"
credit_trans[credit$Credit_history == "A31"]$Credit_history <- "Duly paid at this bank"
credit_trans[credit$Credit_history == "A32"]$Credit_history <- "Existing credit duly paid at this bank"
credit_trans[credit$Credit_history == "A33"]$Credit_history <- "Payment delay in the past"
credit_trans[credit$Credit_history == "A34"]$Credit_history <- "Critical account/Credit at other banks"

# Purpose
credit_trans[credit$Purpose == "A40"]$Purpose <- "car (new)"
credit_trans[credit$Purpose == "A41"]$Purpose <- "car (used)"
credit_trans[credit$Purpose == "A42"]$Purpose <- "furniture/equipment"
credit_trans[credit$Purpose == "A43"]$Purpose <- "radio/television"
credit_trans[credit$Purpose == "A44"]$Purpose <- "domestic appliances"
credit_trans[credit$Purpose == "A45"]$Purpose <- "repairs"
credit_trans[credit$Purpose == "A46"]$Purpose <- "education"
credit_trans[credit$Purpose == "A47"]$Purpose <- "vacation"
credit_trans[credit$Purpose == "A48"]$Purpose <- "retraining"
credit_trans[credit$Purpose == "A49"]$Purpose <- "business"
credit_trans[credit$Purpose == "A410"]$Purpose <- "others"

# Savings_account
credit_trans[credit$Savings_account == "A61"]$Savings_account <- "<100"
credit_trans[credit$Savings_account == "A62"]$Savings_account <- "100-500"
credit_trans[credit$Savings_account == "A63"]$Savings_account <- "501-1000"
credit_trans[credit$Savings_account == "A64"]$Savings_account <- ">1000"
credit_trans[credit$Savings_account == "A65"]$Savings_account <- "Unknown/No account"

# Current_empl_dur
credit_trans[credit$Current_empl_dur == "A71"]$Current_empl_dur <- "Unemployed"
credit_trans[credit$Current_empl_dur == "A72"]$Current_empl_dur <- "<1y"
credit_trans[credit$Current_empl_dur == "A73"]$Current_empl_dur <- "1y-4y"
credit_trans[credit$Current_empl_dur == "A74"]$Current_empl_dur <- "4y-7y"
credit_trans[credit$Current_empl_dur == "A75"]$Current_empl_dur <- ">7y"

# Personal_status_Sex
# New Personal_status feature
credit_trans <- credit_trans %>% mutate(Personal_status="")
credit_trans <- credit_trans %>% mutate(Sex="")

credit_trans[credit$Personal_status_Sex == "A91"]$Personal_status <- "divorced/separated"
credit_trans[credit$Personal_status_Sex == "A92"]$Personal_status <- "divorced/separated/married"
credit_trans[credit$Personal_status_Sex == "A93"]$Personal_status <- "single"
credit_trans[credit$Personal_status_Sex == "A94"]$Personal_status <- "married/widowed"
credit_trans[credit$Personal_status_Sex == "A95"]$Personal_status <- "single"
# New Sex feature
credit_trans[credit$Personal_status_Sex == "A91"]$Sex <- "male"
credit_trans[credit$Personal_status_Sex == "A92"]$Sex <- "female"
credit_trans[credit$Personal_status_Sex == "A93"]$Sex <- "male"
credit_trans[credit$Personal_status_Sex == "A94"]$Sex <- "male"
credit_trans[credit$Personal_status_Sex == "A95"]$Sex <- "female"

credit_trans <- credit_trans %>% select(-Personal_status_Sex)

# Other_debtors_guarantors
credit_trans[credit$Other_debtors_guarantors == "A101"]$Other_debtors_guarantors <- "none"
credit_trans[credit$Other_debtors_guarantors == "A102"]$Other_debtors_guarantors <- "co-applicant"
credit_trans[credit$Other_debtors_guarantors == "A103"]$Other_debtors_guarantors <- "guarantor"

# Property
credit_trans[credit$Property == "A121"]$Property <- "real estate"
credit_trans[credit$Property == "A122"]$Property <- "building society savings agreement/life insurance"
credit_trans[credit$Property == "A123"]$Property <- "car or other"
credit_trans[credit$Property == "A124"]$Property <- "unknown / no property"

# Other_installment_plans
credit_trans[credit$Other_installment_plans == "A141"]$Other_installment_plans <- "bank"
credit_trans[credit$Other_installment_plans == "A142"]$Other_installment_plans <- "stores"
credit_trans[credit$Other_installment_plans == "A143"]$Other_installment_plans <- "none"

# Housing
credit_trans[credit$Housing == "A151"]$Housing <- "rent"
credit_trans[credit$Housing == "A152"]$Housing <- "own"
credit_trans[credit$Housing == "A153"]$Housing <- "for free"

# Job
credit_trans[credit$Job == "A171"]$Job <- "unemployed/unskilled - NR"
credit_trans[credit$Job == "A172"]$Job <- "unemployed/unskilled - R"
credit_trans[credit$Job == "A173"]$Job <- "skilled/official"
credit_trans[credit$Job == "A174"]$Job <- "management/self-employed/highly qualified/officer"

# Telephone
credit_trans[credit$Telephone == "A191"]$Telephone <- "no"
credit_trans[credit$Telephone == "A192"]$Telephone <- "yes"

# Foreign
credit_trans[credit$Foreign == "A201"]$Foreign <- "yes"
credit_trans[credit$Foreign == "A202"]$Foreign <- "no"

# Risk_profile
credit_trans <- credit_trans %>% mutate(Risk_profile="")

credit_trans[credit$Risk == "1"]$Risk_profile <- "good"
credit_trans[credit$Risk == "2"]$Risk_profile <- "bad"

# Convert categorical features into factor
credit_trans$Checking_acc_status <- factor(credit_trans$Checking_acc_status)
credit_trans$Credit_history <- factor(credit_trans$Credit_history)
credit_trans$Purpose <- factor(credit_trans$Purpose)
credit_trans$Savings_account  <- factor(credit_trans$Savings_account )
credit_trans$Current_empl_dur <- factor(credit_trans$Current_empl_dur)
credit_trans$Other_debtors_guarantors <- factor(credit_trans$Other_debtors_guarantors)
credit_trans$Property <- factor(credit_trans$Property)
credit_trans$Other_installment_plans <- factor(credit_trans$Other_installment_plans)
credit_trans$Housing <- factor(credit_trans$Housing)
credit_trans$Job <- factor(credit_trans$Job)
credit_trans$Telephone <- factor(credit_trans$Telephone)
credit_trans$Foreign <- factor(credit_trans$Foreign)
credit_trans$Personal_status <- factor(credit_trans$Personal_status)
credit_trans$Sex <- factor(credit_trans$Sex)

# Convert outcome features into factor
credit_trans$Risk_profile = factor(credit_trans$Risk_profile)
credit$Risk = factor(credit$Risk)
credit_test$Risk = factor(credit_test$Risk)
credit_train$Risk = factor(credit_train$Risk)


# Overview of original credit dataset

# General structure
str(credit)
# Summary stats
summary(credit)
# Sample of first 10 rows
head(credit)
# Total number of rows
c("Number of users" = nrow(credit))


# Overview of the dataset with translated variables

# General structure
str(credit_trans)
# Summary stats
summary(credit_trans)
# Sample of first 10 rows
head(credit_trans)

### Exploratory analysis ### 

#Number of credit risk profiles categorized as "good" / "bad"
cont_risk <- table(credit_trans$Risk_profile)
cont_risk

#### Proportion of credit risk profiles categorized as "good"
c("Proportion of \"good\" credit risk profiles" = mean(credit_trans$Risk_profile=="good"))

# Credit risk distribution
cont_succ <- table(credit_trans$Risk_profile)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Risk") +
  ggtitle("Credit risk distribution")

#### Number of male/female customers
cont_sex <- table(credit_trans$Sex)
cont_sex

# Sex distribution
cont_succ <- table(credit_trans$Sex)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Sex") +
  ggtitle("Sex distribution")

# Job distribution
cont_succ <- table(credit_trans$Job)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Job") +
  ggtitle("Job distribution") +
  theme(axis.text.x = element_text(angle = 90))

# Housing distribution
cont_succ <- table(credit_trans$Housing)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Housing") +
  ggtitle("Housing distribution")

# Savings account distribution
cont_succ <- table(credit_trans$Savings_account)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Savings account") +
  ggtitle("Savings account distribution") +
  theme(axis.text.x = element_text(angle = 90))

# Purpose distribution
cont_succ <- table(credit_trans$Purpose)
data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Purpose") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Purpose distribution")

# Age distribution
credit_trans %>% ggplot(aes(Age)) + 
  geom_histogram(aes(y=..density..),col="red",fill="blue",alpha=.2) +
  ggtitle("Age distribution") + 
  geom_vline(aes(xintercept=mean(Age)),color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.2) 

# Credit amount distribution
credit_trans %>% ggplot(aes(Credit_amount)) + 
  geom_histogram(aes(y=..density..),col="red",fill="red",alpha=.2) +
  ggtitle("Credit amount distribution") + 
  geom_vline(aes(xintercept=mean(Credit_amount)),color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.2) 

# Duration distribution
credit_trans %>% ggplot(aes(Duration)) + 
  geom_histogram(aes(y=..density..),col="red",fill="green",alpha=.2) +
  ggtitle("Duration distribution") + 
  geom_vline(aes(xintercept=mean(Duration)),color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.2) 


#### Features correlation check

# Age boxplot
credit_trans %>% ggplot(aes(Risk_profile,Age,fill=Risk_profile,alpha=.5)) + 
  geom_boxplot() +
  ggtitle("Age boxplot") +
  xlab("Risk profile")
# Quality of profiles by Age
credit_trans %>% 
  group_by(Age) %>% 
  summarize(avg = mean(Risk_profile=="good")) %>%
  ggplot(aes(Age,avg)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Quality of profiles by Age") +
  ylab("Average of \"good\" risk profiles")

# Credit amount boxplot
credit_trans %>% ggplot(aes(Risk_profile,Credit_amount,fill=Risk_profile,alpha=.5)) + 
  geom_boxplot() +
  ggtitle("Credit amount boxplot") +
  xlab("Risk profile")
# Quality of profiles by Credit_amount (bars)
credit_trans %>% 
  mutate(amount_1k = round(Credit_amount/1000)) %>% 
  group_by(amount_1k) %>% 
  summarize(avg_good = mean(Risk_profile=="good")) %>% 
  ggplot(aes(reorder(amount_1k,-avg_good),avg_good,fill=amount_1k)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Credit amount (in k DM)") +
  ggtitle("Quality of profiles by Credit_amount (bars)") +
  ylab("Average of \"good\" risk profiles")
# Quality of profiles by Credit_amount (line)
credit_trans %>% 
  mutate(amount_1k = round(Credit_amount/1000)) %>% 
  group_by(amount_1k) %>% 
  summarize(avg = mean(Risk_profile=="good")) %>%
  ggplot(aes(amount_1k,avg)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Quality of profiles by Credit_amount (line)") +
  ylab("Average of \"good\" risk profiles")

# Duration boxplot
credit_trans %>% ggplot(aes(Risk_profile,Duration,fill=Risk_profile,alpha=.5)) + 
  geom_boxplot() +
  ggtitle("Duration boxplot") +
  xlab("Risk profile")
# Quality of profiles by Duration (bars)
credit_trans %>% 
  group_by(Duration) %>% 
  summarize(avg_good = mean(Risk_profile=="good")) %>% 
  ggplot(aes(reorder(Duration,-avg_good),avg_good,fill=Duration)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Duration") +
  ggtitle("Quality of profiles by Duration (bars)") +
  ylab("Average of \"good\" risk profiles")
# Quality of profiles by Duration (lines)
credit_trans %>% 
  group_by(Duration) %>% 
  summarize(avg = mean(Risk_profile=="good")) %>%
  ggplot(aes(Duration,avg)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Quality of profiles by Duration (lines)") +
  ylab("Average of \"good\" risk profiles")

# Quality of profiles by Sex
credit_trans %>% 
  group_by(Sex) %>% 
  summarize(avg_good = mean(Risk_profile=="good")) %>% 
  ggplot(aes(reorder(Sex,-avg_good),avg_good,fill=Sex)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Sex") +
  ggtitle("Quality of profiles by Sex") +
  ylab("Average of \"good\" risk profiles")
# Quality of profiles by Job
credit_trans %>% 
  group_by(Job) %>% 
  summarize(avg_good = mean(Risk_profile=="good")) %>% 
  ggplot(aes(reorder(Job,-avg_good),avg_good,fill=Job)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Job") +
  ggtitle("Quality of profiles by Job") +
  ylab("Average of \"good\" risk profiles")
# Quality of profiles by Housing
credit_trans %>% 
  group_by(Housing) %>% 
  summarize(avg_good = mean(Risk_profile=="good")) %>% 
  ggplot(aes(reorder(Housing,-avg_good),avg_good,fill=Housing)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Housing") +
  ggtitle("Quality of profiles by Housing") +
  ylab("Average of \"good\" risk profiles")
# Quality of profiles by Savings_account
credit_trans %>% 
  group_by(Savings_account) %>% 
  summarize(avg_good = mean(Risk_profile=="good")) %>% 
  ggplot(aes(reorder(Savings_account,-avg_good),avg_good,fill=Savings_account)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Savings account") +
  ggtitle("Quality of profiles by Savings_account") +
  ylab("Average of \"good\" risk profiles")
# Quality of profiles by Purpose
credit_trans %>% 
  group_by(Purpose) %>% 
  summarize(avg_good = mean(Risk_profile=="good")) %>% 
  ggplot(aes(reorder(Purpose,-avg_good),avg_good,fill=Purpose)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Purpose") +
  ggtitle("Quality of profiles by Purpose") +
  ylab("Average of \"good\" risk profiles")


### Model implementation ### 

# perf_results will host our different models' performance, i.e. metrics for each model
perf_results <- data_frame()

# F1-score calculation
f1 <- function(y_hat,y){
  precision <- posPredValue(y_hat, y, positive="1")
  recall <- sensitivity(y_hat, y, positive="1")
  F1 <- (2 * precision * recall) / (precision + recall)
  F1
}

#------------------------
### Logistic regresion
#------------------------

# trainctrl <- trainControl(verboseIter = TRUE)

# Train model
fit_glm <- train(Risk ~ ., method="glm", data = credit_train)

# Calculate predictions using fitted model
y_hat_glm <- predict(fit_glm, credit_test, type = "raw")

# Display results
cm_glm <- confusionMatrix(y_hat_glm,credit_test$Risk)
Acc_glm <- cm_glm$overall[["Accuracy"]]

F1_glm <- f1(y_hat_glm,credit_test$Risk)

# Save first metric result in perf_results
perf_results <- data_frame(method = "Logistic regresion", Accuracy = Acc_glm, F1_score = F1_glm)
perf_results %>% knitr::kable()


#------------------------
### Decision tree
#------------------------

# Train model
fit_dt <- train(Risk ~ ., 
                   data = credit_train,method="rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), 
                trControl = trainControl(method = "cv"))

# Optimal cp parameter
ggplot(fit_dt)
fit_dt$bestTune

# Calculate predictions using fitted model and check results
y_hat_dt <- predict(fit_dt, credit_test, type = "raw")
cm_dt <- confusionMatrix(y_hat_dt, credit_test$Risk)
Acc_dt <- cm_dt$overall[["Accuracy"]]

F1_dt <- f1(y_hat_dt,credit_test$Risk)

# Save metric in perf_results
perf_results <- bind_rows(perf_results, data_frame(method="Decision tree", Accuracy = Acc_dt, F1_score = F1_dt ))
perf_results %>% knitr::kable()

# Tree visualization
plot(fit_dt$finalModel, margin = 0.1)
text(fit_dt$finalModel, cex = 0.75)

#------------------------
### Random forest
#------------------------
# Please note that this code takes a substantial amount of time to run

# trainctrl <- trainControl(number = 25)

trainctrl <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

# Parameters (mtry) fit. ntree parameter is set at a fixed value of 1000
fit_rf <- train(Risk ~ .
                , data = credit_train,method="rf",
                tuneGrid = data.frame(mtry = seq(1,15)),ntree=1000, trControl = trainctrl)

# Optimal mtry parameter
print(fit_rf)
ggplot(fit_rf)
fit_rf$bestTune

# Feature importance analysis
imp <- varImp(fit_rf)
imp

# Calculate predictions using fitted model and check results
y_hat_rf <- predict(fit_rf, credit_test, type = "raw")
cm_rf <- confusionMatrix(y_hat_rf,credit_test$Risk)
Acc_rf <- cm_rf$overall[["Accuracy"]]
F1_rf <- f1(y_hat_rf,credit_test$Risk)

# Save metric in perf_results
perf_results <- bind_rows(perf_results, data_frame(method="Random forest", Accuracy = Acc_rf, F1_score = F1_rf ))
perf_results %>% knitr::kable()


#------------------------
### SVM with Linear Kernel
#------------------------

# Set up Repeated k-fold Cross Validation
train_control <- trainControl(method="repeatedcv", number=25, repeats=3)

# Fit the model 
svm <- train(Risk ~ ., data = credit_train, 
              method = "svmLinear", trControl = train_control)
#View the model
svm

# Calculate predictions using fitted model and check results
y_hat_svm <- predict(svm, credit_test, type = "raw")
cm_svm <- confusionMatrix(y_hat_svm, credit_test$Risk)
Acc_svm <- cm_svm$overall[["Accuracy"]]
F1_svm <- f1(y_hat_svm,credit_test$Risk)

# Save metric in perf_results
perf_results <- bind_rows(perf_results, data_frame(method="SVM with Linear Kernel", Accuracy = Acc_svm, F1_score = F1_svm ))
perf_results %>% knitr::kable()

#------------------------
### kNN
#------------------------

control <- trainControl(method = "cv", number = 10, p = .9)

# Fit the model 
fit_knn <- train(Risk ~ ., method = "knn", 
                      data = credit_train,
                      tuneGrid = data.frame(k = seq(3, 61, 2)),
                      trControl = control)

# Optimal K parameter
ggplot(fit_knn)
fit_knn$bestTune

# Calculate predictions using fitted model and check results
y_hat_knn <- predict(fit_knn, credit_test, type = "raw")
cm_knn <- confusionMatrix(y_hat_knn, credit_test$Risk)
Acc_knn <- cm_knn$overall[["Accuracy"]]
F1_knn <- f1(y_hat_knn,credit_test$Risk)

# Save metric in perf_results
perf_results <- bind_rows(perf_results, data_frame(method="kNN", Accuracy = Acc_knn, F1_score = F1_knn ))
perf_results %>% knitr::kable()




