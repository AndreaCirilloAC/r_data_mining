library(dplyr)
library(tidyr)
library(caret)

#random forest

training_data %>% 
  mutate(commercial_portfolio = as.factor(commercial_portfolio),
         business_unit = as.factor(business_unit))-> training_data_factor

set.seed(11)
random_forest <- randomForest::randomForest(formula = as.factor(default_numeric)~., 
                                            data = training_data_factor,
                                            ntree = 400, 
                                            importance = TRUE)

random_forest

plot(random_forest)

random_forest$err.rate %>% tail()

varImpPlot(random_forest)

## ensemble learning and confusion matrix

