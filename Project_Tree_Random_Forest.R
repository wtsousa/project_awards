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

delta_list <- c(3,5,10,15) #The number of immediate past years the model will consider for train
test_range_list <- c(1,3,5) #The number of years the model will consider for test
test_year <- 2020 #The prediction year

#Empty Data Frame for Results Data
bagging_summary <- data.frame()

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
## BAGGING ###
##############


print(paste("Delta:",delta))
print(paste("Test Range:",test_range))
print("Running...")

set.seed(123)
Awards_model_bagging <- train(model_formula, 
                              data = Awards_Train, 
                              method = "rf")

#'ipred' library bagging
#Awards_model_bagging <- bagging(formula = model_formula, 
#                        data = Awards,
#                        coob = TRUE)

# Print the model
print(Awards_model_bagging)


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

bagging_result <- data.frame(delta,test_range,test_year,AUC_G,AUC_F,Sensitivity,Specificity,Acuracia)
rownames(bagging_result) <- NULL
bagging_summary <- rbind(bagging_summary,bagging_result)

}}

##############################################
##############################################
##############   END OF LOOP  ################
##############################################
##############################################

#> bagging_summary #Resultado prevendo indicados/Nominees considerando dataset só com Indicados(N)
#delta test_range test_year     AUC_G     AUC_F Sensitivity Specificity  Acuracia
#1      3          1      2020 0.9818182 0.7000000   0.4000000   1.0000000 0.8888889
#2      3          3      2020 0.9385185 0.6722222   0.4000000   0.9444444 0.8666667
#3      3          5      2020 0.9731073 0.7971751   0.6000000   0.9943503 0.9455446
#4      5          1      2020 0.9727273 0.7772727   0.6000000   0.9545455 0.8888889
#5      5          3      2020 0.9370370 0.7000000   0.4666667   0.9333333 0.8666667
#6      5          5      2020 0.9780791 0.8574011   0.7600000   0.9548023 0.9306931
#7     10          1      2020 0.9818182 0.9772727   1.0000000   0.9545455 0.9629630
#8     10          3      2020 0.9577778 0.8666667   0.8000000   0.9333333 0.9142857
#9     10          5      2020 0.9740113 0.7543503   0.5200000   0.9887006 0.9306931
#10    15          1      2020 0.9727273 0.8772727   0.8000000   0.9545455 0.9259259
#11    15          3      2020 0.9525926 0.7000000   0.4666667   0.9333333 0.8666667
#12    15          5      2020 0.9744633 0.7858757   0.6000000   0.9717514 0.9257426

#Aparentemente, quanto maior o número de dados analisados (range), melhor é o
#desempenho do random forest (indicadores aumentam), mas os dados de dez anos parece
#ser melhor que os de quinze.

#> bagging_summary #Resultado prevendo vencedores/Winners considerando dataset completo (N+W)
#delta test_range test_year     AUC_G     AUC_F Sensitivity Specificity  Acuracia
#1      3          1      2020 0.8846154 0.5000000   0.0000000   1.0000000 0.9629630
#2      3          3      2020 0.7026144 0.6568627   0.3333333   0.9803922 0.9619048
#3      3          5      2020 0.9492386 0.6949239   0.4000000   0.9898477 0.9752475
#4      5          1      2020 0.9230769 0.5000000   0.0000000   1.0000000 0.9629630
#5      5          3      2020 0.9052288 0.6666667   0.3333333   1.0000000 0.9809524
#6      5          5      2020 0.9269036 0.6949239   0.4000000   0.9898477 0.9752475
#7     10          1      2020 0.9615385 0.5000000   0.0000000   1.0000000 0.9629630
#8     10          3      2020 0.7287582 0.6617647   0.3333333   0.9901961 0.9714286
#9     10          5      2020 0.9659898 0.6949239   0.4000000   0.9898477 0.9752475
#10    15          1      2020 0.9230769 0.5000000   0.0000000   1.0000000 0.9629630
#11    15          3      2020 0.9477124 0.6666667   0.3333333   1.0000000 0.9809524
#12    15          5      2020 0.9776650 0.6000000   0.2000000   1.0000000 0.9801980

#Nota-se que para prever os vencedores/Winners, o treino com delta alto (15) aumenta os indicadores
#De qualquer forma, para este target o método Bagging obteve melhor performance

###### CONCLUSIONS OSCAR NOMINEES ######

Awards_Test %>% filter(YEAR==2020) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                    Ford v Ferrari 0.70219165      1     1
#2                          Parasite 0.69191158      1     1
#3                      The Irishman 0.56506646      1     1
#4  Once Upon a Time... in Hollywood 0.46288005      0     0
#5                             Joker 0.44080206      1     0
#6                              1917 0.41605417      0     0
#7                       Jojo Rabbit 0.27765046      1     0
#8                    Marriage Story 0.20075219      0     0
#9                      Little Women 0.14737467      0     0
#10              Dolemite Is My Name 0.14213604      0     0

Awards_Test %>% filter(YEAR==2019) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                                                The Favourite 0.61234013      1     1
#2                                                         Roma 0.60155687      0     1
#3                                                    First Man 0.56065613      0     1
#4                                               A Star Is Born 0.53164555      0     1
#5                                               BlacKkKlansman 0.41168023      1     0
#6                                                         Vice 0.24105428      1     0
#7                                            Bohemian Rhapsody 0.21178673      1     0
#8                                                   Green Book 0.19120476      1     0
#9                                            Crazy Rich Asians 0.14213604      0     0
#10                                                  Deadpool 2 0.14213604      0     0

Awards_Test %>% filter(YEAR==2018) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                                    Dunkirk 0.67612205      1     1
#2                                Baby Driver 0.62647719      1     1
#3                          Blade Runner 2049 0.52047653      0     1
#4                         The Shape of Water 0.48120785      1     0
#5  Three Billboards Outside Ebbing, Missouri 0.39562646      1     0
#6                                   I, Tonya 0.37399249      1     0
#7                                    Get Out 0.34181449      0     0
#8                                   The Post 0.22918765      0     0
#9                                  Lady Bird 0.15686503      0     0
#10                              Molly's Game 0.14213604      0     0

Awards_Test %>% filter(YEAR==2017) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                                 La La Land 0.71096625      1     1
#2                                    Arrival 0.70200915      1     1
#3                              Hacksaw Ridge 0.67136784      1     1
#4                                  Moonlight 0.65842573      1     1
#5                          Nocturnal Animals 0.36770706      0     0
#6                      Manchester by the Sea 0.31111777      0     0
#7                                     Jackie 0.25624231      0     0
#8                         Hell or High Water 0.16027926      1     0
#9                               Cameraperson 0.14706400      0     0
#10                             Hail, Caesar! 0.14213604      0     0

Awards_Test %>% filter(YEAR==2016) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                          Mad Max: Fury Road 0.75441407      1     1
#2                                The Revenant 0.59362193      1     1
#3                                   Spotlight 0.52543364      1     1
#4                                     Sicario 0.51670805      0     1
#5                               The Big Short 0.50716897      1     1
#6  Star Wars: Episode VII - The Force Awakens 0.50300729      1     1
#7                                 The Martian 0.40563859      0     0
#8                                     Ant-Man 0.14213604      0     0
#9                                         Joy 0.14213604      0     0
#10             Me and Earl and the Dying Girl 0.14213604      0     0

###### CONCLUSIONS OSCAR WINNERS ######

Awards_Test %>% filter(YEAR==2020) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                          Parasite 0.326098589      0     0
#2                    Ford v Ferrari 0.226847726      1     0
#3                      The Irishman 0.125668590      0     0
#4                              1917 0.081666346      0     0
#5                       Jojo Rabbit 0.078256625      0     0

Awards_Test %>% filter(YEAR==2019) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE        prob target class
#1                                                The Favourite 0.173622875      0     0
#2                                                    First Man 0.141884934      0     0
#3                                               A Star Is Born 0.105529582      0     0
#4                                            Bohemian Rhapsody 0.082457973      1     0
#5                                                         Roma 0.082306737      0     0

Awards_Test %>% filter(YEAR==2018) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE        prob target class
#1                                    Dunkirk 0.360240091      1     0
#2                                Baby Driver 0.279270879      0     0
#3                                   I, Tonya 0.115213492      0     0
#4                                  Good Time 0.083685210      0     0
#5                         The Shape of Water 0.070468555      0     0   

Awards_Test %>% filter(YEAR==2017) %>% select(c(TITLE,prob,target,class)) %>% arrange(desc(prob))
#TITLE       prob target class
#1                                 La La Land 0.361756934      0     0
#2                                    Arrival 0.252033008      0     0
#3                              Hacksaw Ridge 0.215337648      1     0
#4                                  Moonlight 0.206651423      0     0
#5                                       Elle 0.070846267      0     0

###### DRAFT SECTION #####

plot(sort(prob_prediction))
plot(sort(prob_prediction), type="o", col="blue", pch="o", lty=1)
points(sort(reference_factor), col="red", pch="*")
lines(sort(reference_factor), col="red",lty=2)
points(sort(class_prediction), col="dark red",pch="+")
lines(sort(class_prediction), col="dark red", lty=3)

bagging_summary

