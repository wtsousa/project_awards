#######################
## Loading libraries ##
#######################

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

########################
## Reading Input Data ##
########################

df_path <- 'Data Science/Projeto Tree/Tree_Editing.csv'
Awards <- read.csv(file=df_path, header=TRUE, sep=",")
head(Awards)
str(Awards)
summary(Awards)

## Columns with NA data



####################
## Data Wrangling ##
####################

#Removing columns with predition power:
# - X (Film Title)
# - IMDB (ID)
# - Names


#Awards_Tree <- Awards %>% select(-c(X,IMDB,Names))
Awards_Tree <- Awards %>% select(-c(TITLE_ID))



####################
## DECISION TREES ##
####################

#Report - NOMINEES
#METHOD       DELTA   TEST_YEAR SENSITIVITY(TP) SPECIFICITY(TN) 

#SIMPLE RPART 15      2020      80%             90.91%
#SIMPLE RPART 15      2019      40%             81.25%
#SIMPLE RPART 15      2018      60%             87.50%
#SIMPLE RPART 15      2017      80%             100%
#SIMPLE RPART 15      2016      80%             93.75%

#SIMPLE RPART 12      2020      100%            72.73%
#SIMPLE RPART 12      2019      40%             81.25%
#SIMPLE RPART 12      2018      60%             87.50%
#SIMPLE RPART 12      2017      80%             95.65%
#SIMPLE RPART 12      2016      60%             100%

#SIMPLE RPART 10      2020      80%             72.73%
#SIMPLE RPART 10      2019      40%             93.75%
#SIMPLE RPART 10      2018      60%             87.50%
#SIMPLE RPART 10      2017      80%             100%
#SIMPLE RPART 10      2016      80%             100%

#SIMPLE RPART 8       2020      60%             81.82%
#SIMPLE RPART 8       2019      40%             93.75%
#SIMPLE RPART 8       2018      60%             87.50%
#SIMPLE RPART 8       2017      80%             95.65%
#SIMPLE RPART 8       2016      80%             100%

#SIMPLE RPART 6       2020      100%            90.91%
#SIMPLE RPART 6       2019      20%             93.75%
#SIMPLE RPART 6       2018      60%             87.50%
#SIMPLE RPART 6       2017      80%             95.65%
#SIMPLE RPART 6       2016      80%             87.50%

#RANDOM TREES 15      2020      100%            90.91%
#RANDOM TREES 15      2019      80%             83.33%
#RANDOM TREES 15      2018      100%            94.44%
#RANDOM TREES 15      2017      60%             100%
#RANDOM TREES 15      2016      80%             90%

#RANDOM TREES 12      2020      100%            90.91% 
#RANDOM TREES 12      2019      60%             83.33%
#RANDOM TREES 12      2018      80%             84.44%
#RANDOM TREES 12      2017      80%             100%
#RANDOM TREES 12      2016      100%            90%

#RANDOM TREES 10      2020      100%            90.91% 
#RANDOM TREES 10      2019      60%             83.33%
#RANDOM TREES 10      2018      100%            94.44%
#RANDOM TREES 10      2017      80%             96.43%
#RANDOM TREES 10      2016      80%             90%

#RANDOM TREES 8       2020      100%            100% 
#RANDOM TREES 8       2019      40%             88.88%
#RANDOM TREES 8       2018      100%            94.44%
#RANDOM TREES 8       2017      100%            94.44%
#RANDOM TREES 8       2016      100%            95%

#RANDOM TREES 6       2020      100%            100%
#RANDOM TREES 6       2019      20%             88.88%
#RANDOM TREES 6       2018      60%             94.44%
#RANDOM TREES 6       2017      80%             100%
#RANDOM TREES 6       2016      100%            90%

#BAGGING      15      2020      100%            90.91%
#BAGGING      15      2019      80%             83.33%
#BAGGING      15      2018      100%            94.44%
#BAGGING      15      2017      60%             100%
#BAGGING      15      2016      80%             90%

#BAGGING      12      2020      100%            90.91%
#BAGGING      12      2019      60%             83.33%
#BAGGING      12      2018      80%             94.44%
#BAGGING      12      2017      80%             100%
#BAGGING      12      2016      100%            90%

#BAGGING      10      2020      100%            90.91%
#BAGGING      10      2019      60%             83.33%
#BAGGING      10      2018      100%            94.44%
#BAGGING      10      2017      80%             96.43%
#BAGGING      10      2016      80%             90%

#BAGGING      8       2020      100%            100%
#BAGGING      8       2019      40%             88.88%
#BAGGING      8       2018      100%            94.44%
#BAGGING      8       2017      100%            94.44%
#BAGGING      8       2016      100%            95%

#BAGGING      6       2020      100%            100%
#BAGGING      6       2019      20%             88.88%
#BAGGING      6       2018      60%             94.44%
#BAGGING      6       2017      80%             100%
#BAGGING      6       2016      100%            90%

#BOOSTING     10      2020      80%             100%
#BOOSTING     10      2019      60%             83.33%      

#BOOSTING     8       2019      40%             88.88%

# Train and Test Sets
delta = 15 #The number of immediate past years the model will consider
test_year = 2020 #The prediction year

Awards_Test <- Awards %>% filter(YEAR<=test_year+1,YEAR>=test_year-4) %>% select(-c(YEAR,TITLE_ID))
Awards_Train <- Awards %>% filter(YEAR<test_year-4,YEAR>(test_year-delta)) %>% select(-c(YEAR,TITLE_ID))

target_variable <- "X...Academy.Awards....Oscar....BEST.FILM.EDITING.....W.."
model_formula <- as.formula(paste(target_variable, " ~ ."))

#Transforming All Variables from Numeric to Factor
#Awards_Test[] <- lapply(Awards_Test[], as.factor)
#Awards_Train[] <- lapply(Awards_Train[], as.factor)

# Create the model
Awards_model <- rpart(formula = model_formula, 
                      data = Awards_Train, 
                      method = "class")
Awards_model

# Display the results 
rpart.plot(x = Awards_model, yesno = 2, type = 0, extra = 0)

# Predicted Class
class_prediction <- predict(object = Awards_model,  
                            newdata = Awards_Test,  
                            type = "class") 

# Calculate the confusion matrix for the test set
test_set.factor <- as.factor(Awards_Test$X...Academy.Awards....Oscar....BEST.FILM.EDITING.....W..)
confusionMatrix(data = class_prediction,         
                reference = test_set.factor,
                positive = '1')

##################################
### ALTERNATIVE MODELS - CARET ###
##################################

#Imputing 0 instead of NA so that the CART methods from the caret library can work
Awards_Train_noNA <- Awards_Train
Awards_Train_noNA[is.na(Awards_Train_noNA)] <- 0
Awards_Test_noNA <- Awards_Test
Awards_Test_noNA[is.na(Awards_Test_noNA)] <- 0

######################
### RANDOM FORESTS ###
######################

Awards_model_RF <- train(model_formula, 
                                  data = Awards_Train_noNA, 
                                  method = "rf")

# Importância das Variáveis Preditoras
#imp <- varImp(Awards_model_RF, scale=FALSE)

# Gerando Matriz de Confusão
pred_rf <- predict(Awards_model_RF ,newdata=Awards_Test_noNA)
pred_rf.factor <- as.factor(ifelse(pred_rf>0.5,1,0))
test_set.factor <- as.factor(Awards_Test_noNA$X...Academy.Awards....Oscar....BEST.FILM.EDITING.....W..)
confusionMatrix(data=pred_rf.factor, test_set.factor,positive = '1')



###############
### BAGGING ###
###############

Awards_model_bagging <- train(model_formula, 
                        data = Awards_Train_noNA, 
                        method = "treebag")

# Importância das Variáveis Preditoras
#imp <- varImp(Awards_model_bagging, scale=FALSE)

# Gerando Matriz de Confusão
pred_bag <- predict(Awards_model_bagging ,newdata=Awards_Test_noNA)
pred_bag.factor <- as.factor(ifelse(pred_rf>0.5,1,0))
test_set.factor <- as.factor(Awards_Test_noNA$X...Academy.Awards....Oscar....BEST.FILM.EDITING.....W..)
confusionMatrix(data=pred_bag.factor, test_set.factor, positive = '1')

########################
###     BOOSTING     ###
### TAKES 10 MINUTES ###
########################

Awards_model_boosting <- train(model_formula, 
                              data = Awards_Train_noNA, 
                              method = "xgbTree")

Awards_model_boosting

# Importância das Variáveis Preditoras
imp <- varImp(Awards_model_boosting, scale=FALSE)
imp
plot(imp)

# Gerando Matriz de Confusão
pred_boost <- predict(Awards_model_boosting ,newdata=Awards_Test_noNA)
pred_boost.factor <- as.factor(ifelse(pred_rf>0.5,1,0))
test_set.factor <- as.factor(Awards_Test_noNA$X...Academy.Awards....Oscar....BEST.FILM.EDITING.....W..)

confusionMatrix(data=pred_boost.factor, test_set.factor, positive = '1')


#####################
## HYPERPARAMETERS ##
#####################

#Generate a grid of hyperparameter values

# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(1, 4, 1)
maxdepth <- seq(1, 6, 1)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)

# Check out the grid
head(hyper_grid)

# Print the number of grid combinations
nrow(hyper_grid)

# Number of potential models in the grid
num_models <- nrow(hyper_grid)

# Create an empty list to store models
grade_models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:num_models) {
  
  # Get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # Train a model and store in the list
  grade_models[[i]] <- rpart(formula = final_grade ~ ., 
                             data = grade_train, 
                             method = "anova",
                             minsplit = minsplit,
                             maxdepth = maxdepth)
}

# Number of potential models in the grid
num_models <- length(grade_models)

# Create an empty vector to store RMSE values
rmse_values <- c()

# Write a loop over the models to compute validation RMSE
for (i in 1:num_models) {
  
  # Retrieve the i^th model from the list
  model <- grade_models[[i]]
  
  # Generate predictions on grade_valid 
  pred <- predict(object = model,
                  newdata = grade_valid)
  
  # Compute validation RMSE and add to the 
  rmse_values[i] <- rmse(actual = grade_valid$final_grade, 
                         predicted = pred)
}

# Identify the model with smallest validation set RMSE
best_model <- grade_models[[which.min(rmse_values)]]

# Print the model paramters of the best model
best_model$control

# Compute test set RMSE on best_model
pred <- predict(object = best_model,
                newdata = grade_test)
rmse(actual = grade_test$final_grade, 
     predicted = pred)