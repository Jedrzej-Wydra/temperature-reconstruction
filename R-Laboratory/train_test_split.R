library(tidyverse)
library(fda)
library(lubridate)

dataset_august_pila <- dataset_august[,c(1,4)]

#dataset_december_pila <- dataset_december[,c(2,5)]

data <- 
  loc_data_august %>% 
  #loc_data_december %>%
  left_join(dataset_august_pila, by = 'date') %>%
  #left_join(dataset_december_pila, by = 'date') %>%
  rename(Y = temp, X = temp_230)

train_sets <- list()
test_sets <- list()

for(loc in unique(data$location))
{
  temp <- 
    data %>%
    filter(location == loc) %>%
    dplyr::select(-location) %>% 
    transmute(rep = ifelse(str_length(as.character(day(date) - 1)) == 1, paste0('day_0', day(date) - 1), paste0('day_', day(date) - 1)),
    #transmute(rep = ifelse(str_length(as.character(day(date) - 4)) == 1, paste0('day_0', day(date) - 4), paste0('day_', day(date) - 4)),
              
              value = Y,
              hour = hour(date)) %>% 
    filter(hour != 0) %>%
    #mutate(hour = ifelse(hour < 5, hour + 23, hour)) %>% 
    #arrange(rep, hour) %>% 
    pivot_wider(names_from = rep, values_from = value) %>%
    dplyr::select(-hour, -day_16) %>%
    as.matrix()
  
  weather <- 
    data %>%
    filter(location == loc) %>%
    dplyr::select(-location) %>%
    transmute(rep = ifelse(str_length(as.character(day(date) - 1)) == 1, paste0('day_0', day(date) - 1), paste0('day_', day(date) - 1)),
    #transmute(rep = ifelse(str_length(as.character(day(date) - 4)) == 1, paste0('day_0', day(date) - 4), paste0('day_', day(date) - 4)),
              value = X,
              hour = hour(date)) %>% 
    filter(hour != 0) %>%
    #mutate(hour = ifelse(hour < 5, hour + 23, hour)) %>% 
    #arrange(rep, hour) %>% 
    pivot_wider(names_from = rep, values_from = value) %>%
    dplyr::select(-hour, -day_16) %>%
    as.matrix()
  
  train_sets[[loc]] <- list(X = weather[,4:15], Y = temp[,4:15])
  test_sets[[loc]] <- list(X = weather[,1:3], Y = temp[,1:3])
}

validation_train_sets <- list()
validation_test_sets <- list()

set.seed(2024)

for(loc in unique(data$location))
{
  train <- sort(sample(1:12, 9))
  test <- setdiff(1:12, train)
  X_valid_train <- train_sets[[loc]]$X[,train]
  Y_valid_train <- train_sets[[loc]]$Y[,train]
  X_valid_test <- train_sets[[loc]]$X[,test]
  Y_valid_test <- train_sets[[loc]]$Y[,test]
  validation_train_sets[[loc]] <- list(X = X_valid_train, Y = Y_valid_train)
  validation_test_sets[[loc]] <- list(X = X_valid_test, Y = Y_valid_test)
}
