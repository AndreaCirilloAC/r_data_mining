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

#logistic regression

logistic_df <- data.frame(y = logistic$y, fitted_values = logistic$fitted.values)

logistic_df %>% 
  mutate(default_threshold = case_when(as.numeric(fitted_values)>0.5 ~ 1,
                                       TRUE ~ 0)) %>% 
  dplyr::select(y, default_threshold)-> logistic_table


confusionMatrix(as.factor(logistic_table$default_threshold), as.factor(logistic_table$y))

# support vector machine

support_vector_data <- data.frame(predicted = support_vector_machine_linear$fitted,
                                  truth = as.numeric(training_data$default_numeric))

support_vector_data %>% 
  mutate(predicted_treshold = case_when(as.numeric(predicted)>0.5 ~ 1,
                                        TRUE ~ 0))-> support_vector_table
confusionMatrix(as.factor(support_vector_table$predicted_treshold), 
                as.factor(support_vector_table$truth))

# random forest

random_forest

confusionMatrix(as.factor(random_forest$predicted), as.factor(random_forest$y))


## comparing precision metric

data <- data.frame(model = c("logistic",
                             "support_vector",
                             "random_forest"), 
                   precision = c(494/(494+2513),
                                 487/(487+2520),
                                 1951/(1951+1056)))
                   
ggplot(data = data, aes(x = model,y = precision, label = round(precision,2)))+
  geom_bar(stat = 'identity')+
  geom_text()

# ensemble learning

ensemble_dataset <- data.frame(svm = (support_vector_table$predicted_treshold),
                               logistic = (logistic_table$default_threshold),
                               random_forest = as.numeric(as.character(random_forest$predicted)),
                               observed = as.numeric(training_data$default_numeric))

ensemble_dataset %>% 
  mutate(majority = case_when(svm + logistic + random_forest >= 2~ 1,TRUE ~ 0))-> ensemble_dataframe

confusionMatrix(as.factor(ensemble_dataframe$majority), as.factor(ensemble_dataframe$observed))



