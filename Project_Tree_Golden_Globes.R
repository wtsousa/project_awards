#######################
## Loading libraries ##
#######################

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(ipred)
library(Metrics)

################
## LOOP SETUP ##
################

#target_award <- "X...Academy.Awards....Oscar...."
target_award <- "X...GoldenGlobes....Golden.Globe...."

#category_list <- c('BEST.FILM','BEST.DIRECTOR','BEST.COSTUME','BEST.CINEMATOGRAPHY','BEST.SONG','BEST.MUSIC',
#                   'BEST.PRODUCTION.DESIGN', 'BEST.MAKEUP.AND.HAIR','BEST.FILM.EDITING','BEST.FOREIGN.FILM',
#                   'BEST.LEAD.ACTOR','BEST.LEAD.ACTRESS','BEST.SUPPORTING.ACTOR','BEST.SUPPORTING.ACTRESS',
#                   'BEST.VFX','BEST.DOCUMENTARY.SHORT','BEST.ANIMATION','BEST.DOCUMENTARY','BEST.ANIMATED.SHORT',
#                   "BEST.ORIGINAL.SCRIPT","BEST.ADAPTED.SCRIPT", "BEST.SOUND.MIXING", "BEST.SOUND.EDITING",
#                   'BEST.SHORT')
category_list <- c('BEST.SUPPORTING.ACTOR')
file_category_list <- c('Best_Supporting_Actor')
#target_type_list <- c('N','W')
target_type_list <- c('W')
delta_list <- c(6,8,10,12,14) #The number of immediate past years the model will consider for train
test_range_list <- c(1,2,3) #The number of years the model will consider for test
test_year <- 2020 #The prediction year

#Best Short funciona melhor com o Best Animated Short Test que com o Doc Short Test
#Best Animated Short só funciona com o Animated Test. Best Short idem.
#Best Doc Short precisa ser avaliado melhor, mas na rodada inteira vamos usar o Doc Short Test

#category_list <- c('BEST.CINEMATOGRAPHY')
#file_category_list <- c('Cinematography')
#delta_list <- c(10)
#test_range_list <- c(2)
#target_type_list <- c('W')

#Depois testar DocShort com DocShort+Test, (OS,AS,Sound, Doc, Foreign, Editing) + Test 
# Film com Foreign Test, Film com Editing Test & Geral com as Main Categories (Film, Lead, Direct, Script, Editing)

#Empty Data Frames for Results Data
bagging_summary <- data.frame()
awards_importance <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(awards_importance) <- c("Award_Category_Alias")

for (category_index in seq(1,length(category_list),1)) { 
for (target_type in target_type_list) { 
  chosen_file_category <- file_category_list[category_index] 
  chosen_category <- category_list[category_index]
  if (target_type == 'N') { 
    pred_range = 5 
    if (chosen_category == 'BEST.FILM') {
      pred_range = 10
    } 
  } else { pred_range = 1 }

########################
## Reading Input Data ##
########################



# Arquivo com Indicados & Vencedores
df_path <- paste('Data Science/Projeto Tree/Golden Globes/Tree_GG_',chosen_file_category,'_All_Results.csv',sep="")

if (target_type == 'N') {
  # Arquivo só com Indicados
  df_path <- paste('Data Science/Projeto Tree/Golden Globes/Tree_GG_',chosen_file_category,'_Nominees.csv',sep="")
}

Awards <- read.csv(file=df_path, header=TRUE, sep=",")
#head(Awards)
#str(Awards)
#summary(Awards)
dim(Awards)
names(Awards)
#unique(Awards$YEAR)
#length(unique(Awards$YEAR))
#max(unique(Awards$YEAR))

#X films with Y features

####################
## Data Wrangling ##
####################

#Imputing 0 when there's NA
Awards[is.na(Awards)] <- 0

#Checking for duplicate rows
#Awards_Titles <- Awards %>% select(TITLE_ID,TITLE,YEAR) %>% group_by(TITLE_ID) %>% count(YEAR)

#Preparing the model formula
target_variable <- paste(target_award,chosen_category,".....",target_type,"..",sep="")
features_to_be_removed <- c(target_variable,'TITLE_ID','TITLE','YEAR')
model_features <- setdiff(names(Awards), features_to_be_removed)
model_formula <- as.formula(paste(paste(target_variable, " ~ "),paste(model_features, collapse="+")))


##############################################
##############################################
############## MODELLING LOOP ################
##############################################
##############################################

# Prediction Data Set
Awards_Pred <- Awards %>% filter(YEAR==2021)

# Train and Test Sets
for (delta in delta_list){
for (test_range in test_range_list) {

Awards_Test <- Awards %>% filter(YEAR<=test_year,YEAR>(test_year-test_range))
Awards_Train <- Awards %>% filter(YEAR<=(test_year-test_range),YEAR>((test_year-test_range)-delta))


##############
## BAGGING ###
##############

print(paste(chosen_category,target_type))
print(paste("Delta:",delta))
print(paste("Test Range:",test_range))
print("Running...")

#BAGGING
set.seed(123)
Awards_model_bagging <- train(model_formula, 
                              data = Awards_Train, 
                              method = "treebag")


imp <- varImp(Awards_model_bagging, scale=FALSE)
new_imp_df <- cbind(Award_Category_Alias = rownames(imp$importance), imp$importance)
rownames(new_imp_df) <- NULL
new_imp_df$Category_Alias <- chosen_category
new_imp_df$target_type <- target_type
new_imp_df$Delta <- delta
new_imp_df$Test_Range <- test_range

awards_importance <- rbind(awards_importance, new_imp_df)

# Print the model
#print(Awards_model_bagging)

# Generate predicted classes using the model object
prob_prediction <- predict(object = Awards_model_bagging, 
                            newdata = Awards_Test)
# Print the predicted classes
cutoff_threshold <- 0.5
class_prediction <- ifelse(prob_prediction > cutoff_threshold,1,0)
reference_factor <- Awards_Test[target_variable][,]

Awards_Test$prob <- prob_prediction
Awards_Test$class <- class_prediction
Awards_Test$target <- reference_factor

# Calculate the confusion matrix for the test set
conf_matrix <- confusionMatrix(data = as.factor(class_prediction),         
                reference = as.factor(reference_factor),
                positive = '1')

Sensitivity <- conf_matrix$byClass['Sensitivity']
Specificity <- conf_matrix$byClass['Specificity']

Acuracia <- conf_matrix$overall['Accuracy']

# Compute the AUC (`actual` must be a binary vector)
AUC_G <- auc(actual = reference_factor, predicted = prob_prediction)
AUC_F <- auc(actual = as.factor(reference_factor), predicted = as.factor(class_prediction)) 

# Calculate Probabilities using 2021 as the Prediction Year
prob_prediction_2021 <- predict(object = Awards_model_bagging, 
                           newdata = Awards_Pred)
Awards_Pred$prob <- prob_prediction_2021
print(Awards_Pred %>% select(c(TITLE,prob)) %>% arrange(desc(prob)) %>% head(12))

# Compute Oscar prediction metric
Prediction_Index_List <- c()
for (pred_year in seq(test_year-test_range+1,test_year,1)) {
  #Retrieve all predictions
  Oscar_Predictions <- Awards_Test %>% filter(YEAR==pred_year) %>% 
    select(c(TITLE,prob,target,class)) %>% arrange(desc(prob)) 
  #Identify the target rows for comparison
  Oscar_Targets <- Oscar_Predictions[Oscar_Predictions$target == 1,]
  n_targets <- nrow(Oscar_Targets) #It will always be 1 when predicting Winners
  
  #Subset only the n-first rows in the given prediction range
  Oscar_Predictions <- Oscar_Predictions %>% head(pred_range)
  Pred_Index <- sum(Oscar_Predictions$target)/n_targets
  Prediction_Index_List <- append(Prediction_Index_List,Pred_Index)
}
Pred_Index0 <- mean(Prediction_Index_List)

Prediction_Index_List <- c()
for (pred_year in seq(test_year-test_range+1,test_year,1)) {
  #Retrieve all predictions
  Oscar_Predictions <- Awards_Test %>% filter(YEAR==pred_year) %>% 
    select(c(TITLE,prob,target,class)) %>% arrange(desc(prob)) 
  #Identify the target rows for comparison
  Oscar_Targets <- Oscar_Predictions[Oscar_Predictions$target == 1,]
  n_targets <- nrow(Oscar_Targets) #It will always be 1 when predicting Winners
  
  #Subset only the n-first rows in the given prediction range
  Oscar_Predictions <- Oscar_Predictions %>% head(pred_range+1)
  Pred_Index <- sum(Oscar_Predictions$target)/n_targets
  Prediction_Index_List <- append(Prediction_Index_List,Pred_Index)
}
Pred_Index1 <- mean(Prediction_Index_List)

Prediction_Index_List <- c()
for (pred_year in seq(test_year-test_range+1,test_year,1)) {
  #Retrieve all predictions
  Oscar_Predictions <- Awards_Test %>% filter(YEAR==pred_year) %>% 
    select(c(TITLE,prob,target,class)) %>% arrange(desc(prob)) 
  #Identify the target rows for comparison
  Oscar_Targets <- Oscar_Predictions[Oscar_Predictions$target == 1,]
  n_targets <- nrow(Oscar_Targets) #It will always be 1 when predicting Winners
  
  #Subset only the n-first rows in the given prediction range
  Oscar_Predictions <- Oscar_Predictions %>% head(pred_range+2)
  Pred_Index <- sum(Oscar_Predictions$target)/n_targets
  Prediction_Index_List <- append(Prediction_Index_List,Pred_Index)
}
Pred_Index2 <- mean(Prediction_Index_List)

Prediction_Index_List <- c()
for (pred_year in seq(test_year-test_range+1,test_year,1)) {
  #Retrieve all predictions
  Oscar_Predictions <- Awards_Test %>% filter(YEAR==pred_year) %>% 
    select(c(TITLE,prob,target,class)) %>% arrange(desc(prob)) 
  #Identify the target rows for comparison
  Oscar_Targets <- Oscar_Predictions[Oscar_Predictions$target == 1,]
  n_targets <- nrow(Oscar_Targets) #It will always be 1 when predicting Winners
  
  #Subset only the n-first rows in the given prediction range
  Oscar_Predictions <- Oscar_Predictions %>% head(pred_range+3)
  Pred_Index <- sum(Oscar_Predictions$target)/n_targets
  Prediction_Index_List <- append(Prediction_Index_List,Pred_Index)
}
Pred_Index3 <- mean(Prediction_Index_List)

Prediction_Index_List <- c()
for (pred_year in seq(test_year-test_range+1,test_year,1)) {
  #Retrieve all predictions
  Oscar_Predictions <- Awards_Test %>% filter(YEAR==pred_year) %>% 
    select(c(TITLE,prob,target,class)) %>% arrange(desc(prob)) 
  #Identify the target rows for comparison
  Oscar_Targets <- Oscar_Predictions[Oscar_Predictions$target == 1,]
  n_targets <- nrow(Oscar_Targets) #It will always be 1 when predicting Winners
  
  #Subset only the n-first rows in the given prediction range
  Oscar_Predictions <- Oscar_Predictions %>% head(pred_range+4)
  Pred_Index <- sum(Oscar_Predictions$target)/n_targets
  Prediction_Index_List <- append(Prediction_Index_List,Pred_Index)
}
Pred_Index4 <- mean(Prediction_Index_List)

#if (target_type == 'W'){
#  Oscar_Winner_Prediction <- Oscar_Predictions[1,]
#  Oscar_Winner <- Oscar_Predictions[Oscar_Predictions$target == 1,]
#  Pred_Index <- rownames(Oscar_Winner)
#}


#bagging_result <- data.frame(chosen_category,target_type,delta,test_range,test_year,AUC_G,AUC_F,Sensitivity,Specificity,Acuracia,Pred_Index)
#bagging_result <- data.frame(chosen_category,target_type,pred_range,delta,test_range,AUC_G,AUC_F,Pred_Index0,Pred_Index1,Pred_Index2)
bagging_result <- data.frame(chosen_category,target_type,pred_range,delta,test_range,Pred_Index0,Pred_Index1,Pred_Index2,Pred_Index3,Pred_Index4)
rownames(bagging_result) <- NULL
bagging_summary <- rbind(bagging_summary,bagging_result)

}}

}}

##############################################
##############################################
##############   END OF LOOP  ################
##############################################
##############################################



#bagging_summary

write.csv(bagging_summary,"Data Science/Projeto Tree/bagging_summary.csv", row.names = FALSE)
write.csv(awards_importance,"Data Science/Projeto Tree/awards_importance.csv", row.names = FALSE)

#dim(bagging_summary)
#names(bagging_summary)

(bagging_summary %>% group_by(chosen_category,target_type,pred_range,delta, test_range) %>% 
  summarise(mean_p0 = mean(Pred_Index0),mean_p1 = mean(Pred_Index1),mean_p2 = mean(Pred_Index2),
            mean_p3 = mean(Pred_Index3),mean_p4 = mean(Pred_Index4)) %>% 
  mutate(mean_pX = (mean_p0 + mean_p1 + mean_p2 + mean_p3 + mean_p4)/5) %>% arrange(desc(mean_pX)))

bagging_group <- bagging_summary %>% group_by(chosen_category,target_type,pred_range,delta) %>% 
  summarise(mean_p0 = mean(Pred_Index0),mean_p1 = mean(Pred_Index1),mean_p2 = mean(Pred_Index2),
            mean_p3 = mean(Pred_Index3),mean_p4 = mean(Pred_Index4))

bagging_group <- bagging_group %>% mutate(mean_pX = (mean_p0 + mean_p1 + mean_p2 + mean_p3 + mean_p4)/5)

bagging_group <- bagging_group %>% group_by(chosen_category,target_type) %>% 
  summarise(mean_delta = mean(mean_pX)) %>% arrange(desc(mean_delta))

awards_group <- awards_importance %>% group_by(Award_Category_Alias,Category_Alias,target_type) %>%
  summarise(mean_Overall = mean(Overall))
#awards_group %>% arrange(desc(mean_Overall))
#bagging_summary