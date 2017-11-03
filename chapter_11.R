library(dplyr)
library(tidyr)


#random forest
rf.form <- as.formula(paste("as.factor(default_numeric)", 
                            paste(colnames(training_data)[-c(4,11,12)], 
                                  sep = " + ",collapse = " + "), sep = " ~ "))
training_data %>% 
  mutate(commercial_portfolio = as.factor(commercial_portfolio),
         business_unit = as.factor(business_unit))-> training_data_factor
set.seed(11)
random_forest <- randomForest::randomForest(formula = as.factor(default_numeric)~., 
                                            data = training_data_factor, ntree = 200, importance = TRUE) # ok ma attenzione che qui ci mancano le variabili categoriche
