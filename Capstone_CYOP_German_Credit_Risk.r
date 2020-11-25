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
      "Duration","Purpose"))

### Data pre-processing

# Search for NA values over all columns of the dataset
sapply(credit,function(x) sum(is.na(x)))

# There are no NA values

# Convert outcome variable "status" into factor
kickstarter_ini <- kickstarter_ini %>% 
  mutate(status = factor(status))

# Create "year" variable with the year the project was launched in
kickstarter_ini <- kickstarter_ini %>% 
  mutate(year = year(launched_at))

# Create binary "success" variable based on the status variable
kickstarter_ini <- kickstarter_ini %>% 
  mutate(success = ifelse(status=="successful",1,0)) %>%
  mutate(success = factor(success))

# Drop variables which are directly related to the outcome (backers, pledged, etc.)
kickstarter <- kickstarter_ini %>% select(main_category,country,city,goal_usd,success,year,duration,
                                          blurb_length,name_length,start_month,end_month,start_Q,end_Q)

### Create training set, test set and validation set (final hold-out test set)

# Split kickstarter data set into training set (80%) and test set (20%)
test_index <- createDataPartition(kickstarter$success,1,0.2,list=FALSE)

kickstarter_test <- kickstarter[test_index, ]
kickstarter_train <- kickstarter[-test_index, ]


### kickstarter dataset overview ### 

# General structure of the dataset
str(kickstarter_ini)

# Summary stats
summary(kickstarter_ini)

# Sample of first 10 rows
head(kickstarter_ini)

# Total number of rows
c("Number of projects" = nrow(kickstarter_ini))


### kickstarter data exploration ### 

# Total number of countries
c("Number of countries" = length(unique(kickstarter_ini$country)))

# Total number of main categories
c("Number of main categories" = length(unique(kickstarter_ini$main_category)))

# Total number of sub-categories
c("Number of sub-categories" = length(unique(kickstarter_ini$sub_category)))

# Average goal amount in USD
c("Average goal amount in USD" = mean(kickstarter_ini$goal_usd))

# Average pledged amount in USD
c("Average pledged amount in USD" = mean(kickstarter_ini$usd_pledged))

#Number of non-successful (0) / successful projects (1)
cont_succ <- table(kickstarter_ini$success)
cont_succ

# Proportion of succesful projects
c("Proportion of succesful projects" = mean(kickstarter_ini$success==1))

# Average goal amount in USD for successful projects
kick_succ <- kickstarter_ini %>% filter(success==1)
c("Average goal amount in USD" = mean(kick_succ$goal_usd))

# Average pledged amount in USD for successful projects
c("Average pledged amount in USD" = mean(kick_succ$usd_pledged))

# Success stacked bar plot
cont_succ <- table(kickstarter_ini$success)

data.frame(cont_succ) %>% 
  ggplot(aes(x= reorder(Var1,-Freq),Freq,fill=Var1)) +
  geom_bar(stat ="identity") +
  xlab("Success")

# Category distribution (top 20 main categories)
kickstarter_ini %>% 
  group_by(main_category) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(reorder(main_category,-n),n,fill=main_category)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Categories")

# Country distribution
kickstarter_ini %>% 
  group_by(country) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>%
  ggplot(aes(reorder(country,-n),n,fill=country)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_log10() +
  xlab("Countries")

# Success rate per category
kickstarter_ini %>% 
  group_by(main_category) %>% 
  summarize(avg_suc = mean(success==1)) %>% 
  arrange(desc(avg_suc)) %>%
  top_n(20) %>% 
  ggplot(aes(reorder(main_category,-avg_suc),avg_suc,fill=main_category)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Categories")

# Success rate per country
kickstarter_ini %>% 
  group_by(country) %>% 
  summarize(avg_suc = mean(success==1)) %>% 
  arrange(desc(avg_suc)) %>% 
  ggplot(aes(reorder(country,-avg_suc),avg_suc,fill=country)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Countries")

# Goal distribution (goals which comprise >= 1000 projects)
kickstarter_ini %>% 
  mutate(goal_10k = round(goal_usd/10000)) %>% 
  group_by(goal_10k) %>% 
  summarize(n=n()) %>% 
  filter(n>=1000) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(goal_10k,n,fill=goal_10k)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))
xlab("Goals")

# Success rate per goal amount
kickstarter_ini %>% 
  mutate(goal_10k = round(goal_usd/10000)) %>% 
  group_by(goal_10k) %>% 
  summarize(avg = mean(success==1)) %>%
  ggplot(aes(goal_10k,avg)) +
  scale_x_log10() +
  geom_point() +
  geom_smooth()

# Success rate per duration (in months)
kickstarter_ini %>% 
  mutate(dur_months = round(duration/30)) %>% 
  group_by(dur_months) %>% 
  summarize(avg = mean(success==1)) %>%
  ggplot(aes(dur_months,avg)) +
  geom_point() +
  geom_smooth()

# Success rate per year
kickstarter_ini %>% 
  group_by(year) %>% 
  summarize(avg = mean(success==1)) %>%
  ggplot(aes(year,avg)) +
  geom_point() +
  geom_smooth()

# Success rate per project name complexity
kickstarter_ini %>% 
  group_by(name_length) %>% 
  summarize(avg = mean(success==1)) %>%
  ggplot(aes(name_length,avg)) +
  geom_point() +
  geom_smooth()


# Lineas temporales: exito por años, n. proyectos por años, categorias con mas proyectos por años,
# etc.


### Model implementation ### 

#------------------------
### Logistic regresion
#------------------------

now()

trainctrl <- trainControl(verboseIter = TRUE)
# fit_glm <- train(success ~ main_category+country+goal_th+duration_months+name_complexity, 
#                  method="glm", data = kickstarter_train, trControl = trainctrl)

fit_glm <- train(success ~ main_category+country+goal_usd+year+duration+blurb_length +
                   name_length+start_month+end_month+start_Q+end_Q,
                 method="glm", data = kickstarter_train, trControl = trainctrl)

y_hat_glm <- predict(fit_glm, kickstarter_test, type = "raw")

confusionMatrix(y_hat_glm,kickstarter_test$success)$overall[["Accuracy"]]
confusionMatrix(y_hat_glm,kickstarter_test$success)

now()

Accuracy : 0.6917

#------------------------
### Decision tree
#------------------------
now()

fit_rpart <- train(success ~ main_category+country+goal_usd+year+duration+blurb_length +
                     name_length+start_month+end_month+start_Q+end_Q, 
                   data = kickstarter_train,method="rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), trControl = trainctrl)

ggplot(fit_rpart)
fit_rpart$bestTune

# Predicción y evaluación
confusionMatrix(predict(fit_rpart, kickstarter_test, type = "raw"),
                kickstarter_test$success)$overall["Accuracy"]
confusionMatrix(predict(fit_rpart, kickstarter_test, type = "raw"),
                kickstarter_test$success)

# Visualización del árbol
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel, cex = 0.75)

now()

Accuracy : 0.7013


#------------------------
### Random forest
#------------------------
# idx <- createDataPartition(kickstarter_train$success,1,0.2,list=FALSE)
# kick_kk <- kickstarter[idx, ]

now()

trainctrl <- trainControl(verboseIter = TRUE, number = 10)

# Limpieza
rm(dl,kickstarter_ini,kickstarter_test)

# Ajuste de parámetros
fit_rf <- train(success ~ main_category+country+goal_usd+year+duration+blurb_length +
                  name_length+start_month+end_month+start_Q+end_Q
                , data = kickstarter_train,method="rf",
                tuneGrid = data.frame(mtry = seq(1,15)),ntree=200, trControl = trainctrl)

print(fit_rf)
ggplot(fit_rf)
fit_rf$bestTune

kickstarter_test <- kickstarter[test_index, ]

# Estimación de la importancia de las variables
imp <- varImp(fit_rf)
imp

# Predicción y evaluación
confusionMatrix(predict(fit_rf, kickstarter_test, type = "raw"),
                kickstarter_test$success)$overall["Accuracy"]
confusionMatrix(predict(fit_rf, kickstarter_test, type = "raw"),
                kickstarter_test$success)

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
train_control <- trainControl(method="repeatedcv", number=1, repeats=3, verboseIter = TRUE)

# Fit the model 
svm1 <- train(success ~ main_category+country+goal_usd+year+duration+blurb_length +
                name_length+start_month+end_month+start_Q+end_Q, data = kickstarter_train, 
              method = "svmLinear", trControl = train_control))
#View the model
svm1

now()