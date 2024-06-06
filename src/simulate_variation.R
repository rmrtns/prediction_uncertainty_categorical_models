library(dplyr)
library(MASS, exclude = "select")

simulate_variation_in_database <- function(data, identifier, variables, correlations, coefficients_of_variation, number_of_samples){
  list_of_simulated_data <- list()
  for (row in 1:nrow(data)){
    constant_data <- create_constant_data(data, identifier, data[[identifier]][row], variables, number_of_samples)
    simulated_data <- simulate_measurements_per_record(data, identifier, variables, correlations, coefficients_of_variation, number_of_samples, row)
    combined_data <- left_join(constant_data, simulated_data, by = c(identifier, "simulation_index"))
    list_of_simulated_data[[row]] <- combined_data
  }
  return(bind_rows(list_of_simulated_data))
}


create_constant_data <- function(data, identifier, index, variables, number_of_samples){
  constant_data <- data %>%
    select(-all_of(variables)) %>%
    filter(!!sym(identifier) == index) %>%
    slice(rep(1:n(), number_of_samples)) %>%
    mutate(simulation_index = row_number()) %>%
    ungroup()
}


simulate_measurements_per_record <- function(data, identifier, variables, correlations, coefficients_of_variation, number_of_samples, row){
  simulated_data_per_record <- data.frame(
    identifier = data[[identifier]][row],
    simulation_index = 1:number_of_samples
  )
  simulated_data_per_record <- simulated_data_per_record %>% rename(!!sym(identifier) := identifier)
  if (any(is.na(correlations))){
    matrix_of_probabilities <- NA
  } else {
    multivariate_random_numbers <- get_multivariate_random_numbers(variables, correlations, number_of_samples)
    matrix_of_probabilities <- get_matrix_of_probabilities(multivariate_random_numbers)
  }
  for (variable_index in 1:length(variables)){
    if (any(is.na(correlations))){
      simulated_measurements <- simulate_uncorrelated_measurements_gaussian(number_of_samples, data[[variables[variable_index]]][row], (coefficients_of_variation[variable_index] / 100 * data[[variables[variable_index]]][row]))
    } else {
      simulated_measurements <- simulate_correlated_measurements_gaussian(matrix_of_probabilities[, variable_index], data[[variables[variable_index]]][row], (coefficients_of_variation[variable_index] / 100 * data[[variables[variable_index]]][row]))
    }
    simulated_data_per_record <- simulated_data_per_record %>% 
      mutate(!!sym(variables[variable_index]) := simulated_measurements)
  }
  return(simulated_data_per_record)
}


get_multivariate_random_numbers <- function(variables, correlations, number_of_samples){
  variables <- as.vector(variables)
  number_of_variables <- length(variables)
  return(mvrnorm(number_of_samples, mu = rep(0, number_of_variables), Sigma = correlations))
}


get_matrix_of_probabilities <- function(matrix_of_quantiles){
  return(pnorm(matrix_of_quantiles))
}


simulate_uncorrelated_measurements_gaussian <- function(number_of_samples, mean, standard_deviation){
  return(rnorm(number_of_samples, mean, standard_deviation))
}


simulate_correlated_measurements_gaussian <- function(probabilities, mean, standard_deviation){
  return(qnorm(probabilities, mean, standard_deviation))
}