library(dplyr)

# Functions to predict X1m.mortality.
## Use 'dots_arguments' which is a list of arguments entered via an ellipsis in the main function.

get_continuous_prediction_mortality <- function(data, dots_arguments){
  predict(dots_arguments[["model"]], 
          as.matrix(data %>% 
                      select(all_of(dots_arguments[["model_variables"]])) %>% 
                      select(-all_of(dots_arguments[["model_outcome"]]))))
}


get_categorical_prediction_mortality <- function(data, dots_arguments){
  as.numeric(data[["continuous_prediction"]]) > dots_arguments[["model_cut_off"]]
}