library(tidyr)
library(xgboost)
library(boot)
library(stringr)
# library(broom)
library(dplyr) # load after xgboost to prevent masking slice function from dplyr
library(purrr)

source('src/simulate_variation.R', local = variation <- new.env())
source('src/save_patient.R', local = save_patient <- new.env())

simulate_variation_induced_discordance <-
  function(data, identifier, variables, correlations, coefficients_of_variation, number_of_samples, continuous_prediction_function, categorical_prediction_function, ...){ 
    is_prediction_function_entered(continuous_prediction_function, categorical_prediction_function)
    predict_continuous <- create_pointer_to_prediction_function(continuous_prediction_function)
    predict_categorical <- create_pointer_to_prediction_function(categorical_prediction_function)
    dots_arguments <- list(...)
    reference_predictions <- get_reference_predictions(data, identifier, predict_continuous, predict_categorical, dots_arguments)
    list_of_simulated_discordances <- list()
    for (row in 1:nrow(data)){
      constant_data <- variation$create_constant_data(data, identifier, data[[identifier]][row], variables, number_of_samples)
      simulated_data <- variation$simulate_measurements_per_record(data, identifier, variables, correlations, coefficients_of_variation, number_of_samples, row) 
      combined_data <- left_join(constant_data, simulated_data, by = c(identifier, "simulation_index"))
      predicted_outcomes <- get_predictions(combined_data, identifier, predict_continuous, predict_categorical, dots_arguments)
      # save_patient$save_patient_data(data[[identifier]][row], predicted_outcomes)
      discordance_status_per_record <- get_summary_measures(predicted_outcomes, identifier, reference_predictions[["categorical_reference"]][reference_predictions[[identifier]] == data[[identifier]][row]])
      list_of_simulated_discordances[[row]] <- discordance_status_per_record
    }
    simulated_discordances <- bind_rows(list_of_simulated_discordances)
    simulated_discordances_with_reference <- left_join(simulated_discordances, reference_predictions, by = identifier) %>%
      relocate(any_of(c("continuous_reference", "categorical_reference", "percentage_discordant")), .after = last_col())
    simulated_discordances_with_reference <- recode_zero_percent_per_prediction_level(simulated_discordances_with_reference)
    return(simulated_discordances_with_reference)
  }


select_input_cv <- function(biological_cv, analytical_cv){
  if (any(is.na(biological_cv)) & any(is.na(analytical_cv))){
    stop("Please enter an_cv and/or bio_cv", call. = FALSE)
  } else if (!any(is.na(biological_cv)) & any(is.na(analytical_cv))){
    return(biological_cv)
  } else if (any(is.na(biological_cv)) & !any(is.na(analytical_cv))){
    return(analytical_cv)
  } else {
    return(sqrt(biological_cv**2 + analytical_cv**2))
  }
}


is_prediction_function_entered <- function(continuous_prediction_function, categorical_prediction_function){
  if (typeof(continuous_prediction_function) != "closure" & typeof(categorical_prediction_function) != "closure"){
    stop("Please enter function for continuous and/or categorical predictions", call. = FALSE)
  }
}


create_pointer_to_prediction_function <- function(prediction_function){
  if (typeof(prediction_function) != "closure"){
    return(match.fun(dummy_predict))
  } else {
    return(match.fun(prediction_function))
  }
}


dummy_predict <- function(data, dots_arguments){
  return(NA)
}


get_reference_predictions <- function(data, identifier, predict_continuous, predict_categorical, dots_arguments){
  get_predictions(data, identifier, predict_continuous, predict_categorical, dots_arguments) %>%
    rename(
      continuous_reference = continuous_prediction,
      categorical_reference = categorical_prediction
    ) %>%
    select(all_of(identifier), continuous_reference, categorical_reference)
}


get_predictions <- function(data, identifier, predict_continuous, predict_categorical, dots_arguments){
  data %>%
    mutate(continuous_prediction = predict_continuous(., dots_arguments)) %>%
    mutate(categorical_prediction = predict_categorical(., dots_arguments))
}


get_summary_measures <- function(data, identifier, categorical_reference){
  if (any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["continuous_prediction"]]))){
    stop("No continuous and/or categorical predictions could be calculated", call. = FALSE)
  } else if (!any(is.na(data[["continuous_prediction"]])) & any(is.na(data[["categorical_prediction"]]))){
    continuous_prediction_descriptives <- get_continuous_prediction_descriptives(data, identifier)
  } else if (any(is.na(data[["continuous_prediction"]])) & !any(is.na(data[["categorical_prediction"]]))){
    categorical_prediction_descriptives <- get_categorical_prediction_descriptives(data, identifier, categorical_reference)
  } else {
    continuous_prediction_descriptives <- get_continuous_prediction_descriptives(data, identifier)
    categorical_prediction_descriptives <- get_categorical_prediction_descriptives(data, identifier, categorical_reference)
    list(continuous_prediction_descriptives, categorical_prediction_descriptives) %>%
      reduce(left_join, by = identifier)
  }
}


get_continuous_prediction_descriptives <- function(data, identifier){
  data %>%
    group_by(!!sym(identifier)) %>%
    summarise(
      continuous_prediction_mean = mean(continuous_prediction),
      continuous_prediction_sd = sd(continuous_prediction),
      continuous_prediction_cv = sd(continuous_prediction) / mean(continuous_prediction) * 100,
      continuous_prediction_min = min(continuous_prediction),
      continuous_prediction_p2.5 = quantile(continuous_prediction, 0.025),
      continuous_prediction_p25 = quantile(continuous_prediction, 0.25),
      continuous_prediction_median = median(continuous_prediction),
      continuous_prediction_p75 = quantile(continuous_prediction, 0.75),
      continuous_prediction_p97.5 = quantile(continuous_prediction, 0.975),
      continuous_prediction_max = max(continuous_prediction)
    ) %>%
    ungroup()
}


get_categorical_prediction_descriptives <- function(data, identifier, categorical_reference){
  percentage_per_prediction_level <- get_percentage_per_prediction_level(data, identifier)
  percentage_discordant <-  get_percentage_discordant(percentage_per_prediction_level, identifier, categorical_reference)
  left_join(percentage_per_prediction_level, percentage_discordant, by = identifier)
}


get_percentage_per_prediction_level <- function(data, identifier) {
  data %>%
    group_by(!!sym(identifier), categorical_prediction) %>%
    summarise(percentage = n() / nrow(data) * 100, .groups = "keep") %>%
    ungroup() %>%
    pivot_wider(
      names_from = categorical_prediction,
      names_prefix = "percentage_",
      values_from = percentage
    )
}


get_percentage_discordant <- function(data, identifier, categorical_reference){
  data %>%
    mutate(percentage_discordant = 100 - !!sym(paste0("percentage_", categorical_reference))) %>%
    select(all_of(identifier), percentage_discordant)
}


recode_zero_percent_per_prediction_level <- function(data){
  data %>%
    mutate_at(
      vars(starts_with("percentage_")),
      ~case_when(
        is.na(.) ~ 0,
        TRUE ~ .)
    )
}


plot_distribution <- function(data, variable){
  reference <- median(data[[variable]])
  histogram <- plot_distribution_histogram(data, variable, reference)
  qqplot <- plot_distribution_qqplot(data, variable)
  plot(ggarrange(histogram, qqplot))
}


plot_distribution_histogram <- function(data, variable, reference){
  ggplot(data = data, aes(x = !!sym(variable), y = after_stat(density))) +
    geom_histogram(bins = ceiling(length(data[[variable]])/25), color = "black", fill = "white") +
    geom_vline(xintercept = reference, color = "black", linetype = "dashed") +
    labs(title = paste0("Histogram ", variable),
         x = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_distribution_qqplot <- function(data, variable){
  ggplot(data = data, aes(sample = !!sym(variable))) +
    geom_qq() +
    geom_qq_line() +
    labs(title = paste0("QQ plot ", variable),
         x = "Theoretical",
         y = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}


plot_distribution_boxplot <- function(data, variable){
  ggplot(data = data, aes(y = !!sym(variable))) +
    geom_boxplot(color = "black", fill = "white") +
    labs(title = paste0("Boxplot ", variable),
         x = variable) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank())
}


get_percentages_discordant_approximated_ci <- function(data) {
  observations  <- nrow(data)
  percentage_discordant_above_00 <- get_percentage_discordant_cutoff(data, 0)
  percentage_discordant_above_00_with_ci <- get_approximated_ci(percentage_discordant_above_00, observations) %>%
    mutate(discordance_status = "discordant_above_00") %>%
    relocate(discordance_status)
  percentage_discordant_above_01 <- get_percentage_discordant_cutoff(data, "discordant_01")
  percentage_discordant_above_01_with_ci <- get_approximated_ci(percentage_discordant_above_01, observations) %>%
    mutate(discordance_status = "discordant_above_01") %>%
    relocate(discordance_status)
  percentage_discordant_above_05 <- get_percentage_discordant_cutoff(data, 5)
  percentage_discordant_above_05_with_ci <- get_approximated_ci(percentage_discordant_above_05, observations) %>%
    mutate(discordance_status = "discordant_above_05") %>%
    relocate(discordance_status)
  percentage_discordant_above_10 <- get_percentage_discordant_cutoff(data, 10)
  percentage_discordant_above_10_with_ci <- get_approximated_ci(percentage_discordant_above_10, observations) %>%
    mutate(discordance_status = "discordant_above_10") %>%
    relocate(discordance_status)
  return(bind_rows(percentage_discordant_05_with_ci,percentage_discordant_01_with_ci))
}


get_percentage_discordant_cutoff <- function(data, cut_off_discordant) {
  data %>%
    group_by(percentage_discordant > cut_off_discordant) %>%
    summarise(percentage = n() / nrow(data) * 100) %>%
    ungroup() %>%
    filter(`percentage_discordant > cut_off_discordant` == TRUE) %>%
    select(percentage)
}


get_approximated_ci <- function(summary, observations) {
  approximated_ci <- summary %>%
    mutate(
      lower_bound = ((percentage / 100) + qnorm(0.025) * sqrt(((percentage / 100) * (1 - (percentage / 100))) / observations)) * 100,
      upper_bound = ((percentage / 100) + qnorm(0.975) * sqrt(((percentage / 100) * (1 - (percentage / 100))) / observations)) * 100
    )
}


get_summary_of_continuous_predictions_as_vector <- function(data, indices, distribution, variable, group){
  data_sample <- data[indices,]
  if (distribution == "gaussian"){
    return(unlist(summarise_continuous_predictions_gaussian(data_sample, variable, group), use.names = FALSE))
  } else if (distribution == "non_gaussian"){
    return(unlist(summarise_continuous_predictions_non_gaussian_median(data_sample, variable, group), use.names = FALSE))
  }
}


summarise_continuous_predictions_gaussian <- function(data, group, variable){
  group <- if (missing(group)){NULL} else {sym(group)}
  data %>%
    group_by(!!group) %>%
    summarise(
      continuous_prediction_population_mean <- mean(!!sym(variable)),
      continuous_prediction_population_sd <- sd(!!sym(variable)),
      continuous_prediction_population_sem <- sd(!!sym(variable)) / sqrt(nrow(data))
    )
}


summarise_continuous_predictions_non_gaussian <- function(data, variable, group){
  group <- if (missing(group)){NULL} else {sym(group)}
  data %>%
    group_by(!!group) %>%
    summarise(
      min = min(!!sym(variable)),
      p2.5 = quantile(!!sym(variable), 0.025),
      p25 = quantile(!!sym(variable), 0.25),
      median = median(!!sym(variable)),
      p75 = quantile(!!sym(variable), 0.75),
      p97.5 = quantile(!!sym(variable), 0.975),
      max = max(!!sym(variable))
    )
}

summarise_continuous_predictions_non_gaussian_median <- function(data, variable, group){
  group <- if (missing(group)){NULL} else {sym(group)}
  data %>%
    group_by(!!group) %>%
    summarise(
      median = median(!!sym(variable))
    )
}


get_variable_names_from_summary_of_continuous_predictions_as_vector <- function(data, distribution, variable, group){
  if (distribution == "gaussian"){
    return(unlist(list(names(summarise_continuous_predictions_gaussian(data, variable, group)))))
  } else if (distribution == "non_gaussian"){
    return(unlist(list(names(summarise_continuous_predictions_non_gaussian_median(data, variable, group)))))
  }
}


get_summary_of_discordant_predictions_as_vector <- function(data, indices) {
  data_sample <- data[indices,]
  summary <- list(
    data_sample_summary_distribution = c(summarise_distribution_percentage_discordant(data_sample) %>% select(-dataset)),
    percentage_discordant_above_00 = c(get_percentage_discordant_cutoff(data_sample, 0)),
    percentage_discordant_above_01 = c(get_percentage_discordant_cutoff(data_sample, 1)),
    percentage_discordant_above_05 = c(get_percentage_discordant_cutoff(data_sample, 5)),
    percentage_discordant_above_10 = c(get_percentage_discordant_cutoff(data_sample, 10)),
    probability_discordant = c(mean(data_sample[["percentage_discordant"]])),
    probability_of_ref_prediction = c(get_probability_of_ref_prediction_wide(data_sample)),
    probability_discordant_per_ref_prediction = c(get_probability_discordant_per_ref_prediction_wide(data_sample)),
    probability_discordant_and_ref_prediction = c(get_probability_discordant_and_ref_prediction(data_sample))
  )
  return(unlist(summary, use.names = FALSE))
}


get_variable_names_from_summary_discordant_predictions_as_vector <- function(data){
  variable_names_of_summary <- list(
    names(summarise_distribution_percentage_discordant(data) %>% select(-dataset)),
    "percentage_discordant_above_00",
    "percentage_discordant_above_01",
    "percentage_discordant_above_05",
    "percentage_discordant_above_10",
    "probability_discordant",
    names(get_probability_of_ref_prediction_wide(data)),
    names(get_probability_discordant_per_ref_prediction_wide(data)),
    names(get_probability_discordant_and_ref_prediction(data))
  )
  return(unlist(variable_names_of_summary))
}


summarise_distribution_percentage_discordant <- function(data, group){
  name_of_dataset <- as.character(deparse(substitute(data)))
  group <- if (missing(group)){NULL} else {sym(group)}
  data %>%
    group_by(!!group) %>%
    summarise(
      percentage_discordant_min = min(percentage_discordant),
      percentage_discordant_p2.5 = quantile(percentage_discordant, 0.025),
      percentage_discordant_p25 = quantile(percentage_discordant, 0.25),
      percentage_discordant_median = median(percentage_discordant),
      percentage_discordant_p75 = quantile(percentage_discordant, 0.75),
      percentage_discordant_p97.5 = quantile(percentage_discordant, 0.975),
      percentage_discordant_max = max(percentage_discordant)
    ) %>%
    mutate(dataset = name_of_dataset) %>%
    relocate(dataset)
}


get_probability_discordant_and_ref_prediction <- function(data){
  probability_of_ref_prediction <- get_probability_of_ref_prediction_long(data)
  probability_discordant_per_ref_prediction <- get_probability_discordant_per_ref_prediction_long(data)
  combined <- left_join(probability_of_ref_prediction, probability_discordant_per_ref_prediction, by = "categorical_reference")
  combined %>%
    mutate(probability_discordant_and_ref_prediction = 
             (percentage_per_category / 100) * 
             (probability_discordant / 100) *
             100) %>%
    select(categorical_reference, probability_discordant_and_ref_prediction) %>%
    pivot_wider(
      names_from = "categorical_reference",
      names_prefix = "probability_discordant_and_ref_",
      values_from = "probability_discordant_and_ref_prediction"
    )
}


get_probability_of_ref_prediction_long <- function(data){
  data %>%
    group_by(categorical_reference) %>%
    summarise(percentage_per_category = n() / nrow(data) * 100) %>%
    ungroup()
}


get_probability_of_ref_prediction_wide <- function(data){
  data %>%
    group_by(categorical_reference) %>%
    summarise(percentage_per_category = n() / nrow(data) * 100) %>%
    pivot_wider(
      names_from = "categorical_reference",
      names_prefix = "probability_ref_",
      values_from = "percentage_per_category"
    ) %>%
    ungroup()
}


get_probability_discordant_per_ref_prediction_long <- function(data){
  data %>%
    group_by(categorical_reference) %>%
    summarise(probability_discordant = mean(percentage_discordant)) %>%
    ungroup()
}


get_probability_discordant_per_ref_prediction_wide <- function(data){
  data %>%
    group_by(categorical_reference) %>%
    summarise(probability_discordant = mean(percentage_discordant)) %>%
    pivot_wider(
      names_from = "categorical_reference",
      names_prefix = "probability_discordant_if_ref_",
      values_from = "probability_discordant"
    ) %>%
    ungroup()
}