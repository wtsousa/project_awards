#######################
## Loading libraries ##
#######################

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(ipred)
library(Metrics)

########################
## Reading Input Data ##
########################

# Arquivo com Indicados & Vencedores
df_path <- 'Data Science/Projeto Tree/Tree_Editing.csv'
# Arquivo só com Indicados
#df_path <- 'Data Science/Projeto Tree/Tree_Editing_Nominees.csv'

Awards <- read.csv(file=df_path, header=TRUE, sep=",")
head(Awards)
str(Awards)
summary(Awards)
dim(Awards)
names(Awards)
unique(Awards$YEAR)
length(unique(Awards$YEAR))
max(unique(Awards$YEAR))

#427 films with 62 features
#Editing Awards (Nominees and Winners) from 2001 to 2020


####################
## Data Wrangling ##
####################

#Imputing 0 when there's NA
Awards[is.na(Awards)] <- 0

#Removing columns with no variance/one factor
#dim(Awards)
#Awards <- Awards[, sapply(Awards, function(col) length(unique(col))) > 1]
#dim(Filter(function(x) length(unique(x)) > 1, Awards))

#Preparing the model formula
target_variable <- "X...Academy.Awards....Oscar....BEST.FILM.EDITING.....W.."
features_to_be_removed <- c(target_variable,'TITLE_ID','TITLE','YEAR')
model_features <- setdiff(names(Awards), features_to_be_removed)
model_formula <- as.formula(paste(paste(target_variable, " ~ "),paste(model_features, collapse="+")))

delta_list <- c(10,15) #The number of immediate past years the model will consider for train
test_range_list <- c(1,3,5) #The number of years the model will consider for test
test_year <- 2020 #The prediction year

#Empty Data Frame for Results Data
boosting_summary <- data.frame()

##############################################
##############################################
############## MODELLING LOOP ################
##############################################
##############################################

# Train and Test Sets
for (delta in delta_list){
for (test_range in test_range_list) {

Awards_Test <- Awards %>% filter(YEAR<=test_year,YEAR>(test_year-test_range))
Awards_Train <- Awards %>% filter(YEAR<=(test_year-test_range),YEAR>((test_year-test_range)-delta))

#Awards_Train <- lapply(Awards_Train, as.factor)
#Awards_Test <- lapply(Awards_Test, as.factor)


##############
## BOOSTING ##
##############


print(paste("Delta:",delta))
print(paste("Test Range:",test_range))
print("Running...")

set.seed(123)
Awards_model_boosting <- train(model_formula, 
                              data = Awards_Train, 
                              method = "xgbTree")

# Print the model
print(Awards_model_boosting)
imp <- varImp(Awards_model_boosting, scale=FALSE)
print(imp)

# Generate predicted classes using the model object
prob_prediction <- predict(object = Awards_model_boosting, 
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

boosting_result <- data.frame(delta,test_range,test_year,AUC_G,AUC_F,Sensitivity,Specificity,Acuracia)
rownames(boosting_result) <- NULL
boosting_summary <- rbind(boosting_summary,boosting_result)

}}

##############################################
##############################################
##############   END OF LOOP  ################
##############################################
##############################################

#> boosting_summary #Resultado prevendo indicados/Nominees considerando dataset só com Indicados(N)
#delta test_range test_year     AUC_G     AUC_F Sensitivity Specificity  Acuracia
#1    10          1      2020 0.9909091 0.9000000   0.8000000   1.0000000 0.9629630
#2    10          3      2020 0.9400000 0.7277778   0.5333333   0.9222222 0.8666667
#3    10          5      2020 0.9770621 0.8287006   0.6800000   0.9774011 0.9405941
#4    15          1      2020 0.9909091 0.8000000   0.6000000   1.0000000 0.9259259
#5    15          3      2020 0.9507407 0.7722222   0.6000000   0.9444444 0.8952381
#6    15          5      2020 0.9823729 0.8458757   0.7200000   0.9717514 0.9405941

#> boosting_summary #Resultado prevendo vencedores/Winners considerando dataset completo (N+W)
#delta test_range test_year     AUC_G     AUC_F Sensitivity Specificity  Acuracia
#1    10          1      2020 0.9615385 0.5000000   0.0000000   1.0000000 0.9629630
#2    10          3      2020 0.9460784 0.6568627   0.3333333   0.9803922 0.9619048
#3    10          5      2020 0.9791878 0.6923858   0.4000000   0.9847716 0.9702970
#4    15          1      2020 0.9615385 0.5000000   0.0000000   1.0000000 0.9629630
#5    15          3      2020 0.9787582 0.6666667   0.3333333   1.0000000 0.9809524
#6    15          5      2020 0.9730964 0.6949239   0.4000000   0.9898477 0.9752475

#Nota-se que para prever os vencedores/Winners, o treino com delta alto (15) aumenta os indicadores
#De qualquer forma, para este target o método Bagging obteve melhor performance

###### CONCLUSIONS OSCAR NOMINEES ######

Awards_Test %>% filter(YEAR==2020) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                    Ford v Ferrari  0.980617762      1     1
#2                             Joker  0.759473503      1     1
#3                          Parasite  0.660445094      1     1
#4  Once Upon a Time... in Hollywood  0.445598304      0     0
#5                       Jojo Rabbit  0.443716586      1     0
#6                      The Irishman  0.298700273      1     0
#7                              1917  0.286591321      0     0
#8                    Marriage Story  0.272834808      0     0
#9               Dolemite Is My Name  0.244409233      0     0
#10                     The Farewell  0.244409233      0     0

Awards_Test %>% filter(YEAR==2019) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                                               BlacKkKlansman  0.672173679      1     1
#2                                               A Star Is Born  0.628497064      0     1
#3                                                    First Man  0.586031497      0     1
#4                                                         Roma  0.586031497      0     1
#5                                                The Favourite  0.557605863      1     1
#6                                                   Green Book  0.392050326      1     0
#7                                            Bohemian Rhapsody  0.296075463      1     0
#8                                                         Vice  0.296075463      1     0
#9                                            Crazy Rich Asians  0.244409233      0     0
#10                                                  Deadpool 2  0.244409233      0     0

Awards_Test %>% filter(YEAR==2018) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                                Baby Driver  0.832976639      1     1
#2  Three Billboards Outside Ebbing, Missouri  0.611832321      1     1
#3                                    Dunkirk  0.592895806      1     1
#4                          Blade Runner 2049  0.557605863      0     1
#5                                   I, Tonya  0.531740427      1     1
#6                         The Shape of Water  0.298700273      1     0
#7                                   The Post  0.297918439      0     0
#8                               Molly's Game  0.244409233      0     0
#9                                  Lady Bird  0.244409233      0     0
#10                                   Get Out  0.165099144      0     0

Awards_Test %>% filter(YEAR==2017) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                              Hacksaw Ridge  0.987888575      1     1
#2                                 La La Land  0.832976639      1     1
#3                                    Arrival  0.811415315      1     1
#4                                  Moonlight  0.788174689      1     1
#5                          Nocturnal Animals  0.407254219      0     0
#6                      Manchester by the Sea  0.366688222      0     0
#7                         Hell or High Water  0.297918439      1     0
#8                              Hail, Caesar!  0.244409233      0     0
#9                                   Deadpool  0.244409233      0     0
#10                           The Jungle Book  0.244409233      0     0

Awards_Test %>% filter(YEAR==2016) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                          Mad Max: Fury Road  0.811415315      1     1
#2                                The Revenant  0.705247045      1     1
#3  Star Wars: Episode VII - The Force Awakens  0.660851598      1     1
#4                                     Sicario  0.636477292      0     1
#5                                   Spotlight  0.560718775      1     1
#6                               The Big Short  0.514329374      1     1
#7                                     Ant-Man  0.244409233      0     0
#8                                         Joy  0.244409233      0     0
#9              Me and Earl and the Dying Girl  0.244409233      0     0
#10                                 Trainwreck  0.244409233      0     0

###### CONCLUSIONS OSCAR WINNERS ######

Awards_Test %>% filter(YEAR==2020) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                          Parasite  0.543778419      0     1
#2                       Jojo Rabbit  0.239837736      0     0
#3                    Ford v Ferrari  0.150523454      1     0
#4                              1917  0.055738658      0     0
#5                      Darkest Hour -0.002111435      0     0

Awards_Test %>% filter(YEAR==2019) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE        prob target class
#1                                               A Star Is Born  0.304544270      0     0
#2                                            Bohemian Rhapsody  0.239837736      1     0
#3                                                    First Man  0.135023445      0     0
#4                                   The Other Side of the Wind  0.090703249      0     0
#5                             Once Upon a Time... in Hollywood  0.083593130      0     0

Awards_Test %>% filter(YEAR==2018) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE        prob target class
#1                                    Dunkirk  0.764330626      1     1
#2                                  Good Time  0.269456625      0     0
#3                                   I, Tonya  0.250234783      0     0
#4                                Baby Driver  0.040669054      0     0
#5                       Call Me by Your Name  0.008285552      0     0   

Awards_Test %>% filter(YEAR==2017) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                                 La La Land  0.556046188      0     1
#2                                    Arrival  0.372774750      0     0
#3                                       Elle  0.259059519      0     0
#4                              Hacksaw Ridge  0.213451624      1     0
#5                                  Moonlight  0.106586128      0     0

###### DRAFT SECTION #####

plot(sort(prob_prediction))
plot(sort(prob_prediction), type="o", col="blue", pch="o", lty=1)
points(sort(reference_factor), col="red", pch="*")
lines(sort(reference_factor), col="red",lty=2)
points(sort(class_prediction), col="dark red",pch="+")
lines(sort(class_prediction), col="dark red", lty=3)

boosting_summary

