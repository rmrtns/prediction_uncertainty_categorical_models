library(caret)
library(xgboost)
library(pROC)
library(broom)
library(tidyr)
library(dplyr)

source('src/simulate_variation.R', local = variation <- new.env())

# Function to calculate confusion matrix.

calculate_confusion_matrix <- function(data, outcome, predictions, cut_off){
  tidy(confusionMatrix(
    as.factor(as.numeric(data[[predictions]] > cut_off)),
    as.factor(data[[outcome]]),
    positive = '1')) %>%
    mutate(estimate = case_when(term == "mcnemar" ~ p.value,
                                TRUE ~ estimate)) %>%
    select(term, estimate) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )
}
# Functions to create and summarise results of confusion matrices based on aggregate results of discordance status.

get_summmary_performance_measures_as_vector <- function(data, indices, mean_continuous_prediction, outcome, outcome_class_TRUE, prediction_class_TRUE){
  data_sample <- data[indices,]
  summary <- list(c(get_summmary_performance_measures(data_sample, mean_continuous_prediction, outcome, outcome_class_TRUE, prediction_class_TRUE)))
  return(unlist(summary, use.names = FALSE))
}


get_variable_names_from_summmary_performance_measures_as_vector <- function(data, mean_continuous_prediction, outcome, outcome_class_TRUE, prediction_class_TRUE){
  summary <- list(names(get_summmary_performance_measures(data, mean_continuous_prediction, outcome, outcome_class_TRUE, prediction_class_TRUE)))
  return(unlist(summary))
}


get_summmary_performance_measures <- function(data, mean_continuous_prediction, outcome, outcome_class_TRUE, prediction_class_TRUE){
  auroc <- calculate_auroc_from_mean_predictions(data, mean_continuous_prediction, outcome)
  confusion_matrix <- calculate_average_confusion_matrix_from_aggregate_results(data, prediction_class_TRUE, outcome, outcome_class_TRUE)
  confusion_matrix_derived_variables <- calculate_confusion_matrix_derived_measures(confusion_matrix, outcome_class_TRUE)
  combined <- bind_rows(auroc, confusion_matrix_derived_variables)
  return(bind_cols(auroc, confusion_matrix_derived_variables))
}


calculate_auroc_from_mean_predictions <- function(data, mean_continuous_prediction, outcome){
  return(data.frame(auroc = as.numeric(auc(roc(data[[outcome]], as.numeric(data[[mean_continuous_prediction]]))))))
}


calculate_average_confusion_matrix_from_aggregate_results <- function(data, prediction_class_TRUE, outcome, outcome_class_TRUE){
  sample_size = nrow(data)
  percentage_outcome_TRUE <- calculate_percentages_outcome_TRUE(data, outcome, outcome_class_TRUE)
  percentage_prediction_TRUE <- calculate_percentage_prediction_TRUE(data, prediction_class_TRUE)
  percentage_prediction_TRUE_for_outcome_TRUE <- calculate_mean_percentage_prediction_TRUE_for_outcome_TRUE(data, prediction_class_TRUE, outcome, outcome_class_TRUE)
  number_outcome_TRUE <- (percentage_outcome_TRUE / 100) * sample_size
  number_outcome_FALSE <- sample_size - number_outcome_TRUE
  number_prediction_TRUE <- (percentage_prediction_TRUE / 100) * sample_size
  number_prediction_FALSE <- sample_size - number_prediction_TRUE
  true_positives <- (percentage_outcome_TRUE / 100) * (percentage_prediction_TRUE_for_outcome_TRUE / 100) * sample_size
  false_negatives <- number_outcome_TRUE - true_positives
  false_positives <- number_prediction_TRUE - true_positives
  true_negatives <- number_outcome_FALSE - false_positives
  matrix <- matrix(
    c(true_positives, false_positives, false_negatives, true_negatives),
    nrow = 2,
    ncol = 2,
    byrow = TRUE,
    dimnames = list("prediction" = c("1", "0"), "reference" = c("1", "0"))
  )
} 


calculate_percentages_outcome_TRUE <- function(data, outcome, outcome_class_TRUE){
  as.numeric(
    data %>%
      group_by(!!sym(outcome)) %>%
      summarise(percentage_outcome = n() / nrow(data) * 100) %>%
      filter(!!sym(outcome) == outcome_class_TRUE) %>%
      select(percentage_outcome)
  )
}


calculate_percentage_prediction_TRUE <- function(data, categorical_prediction_TRUE){
  mean(data[[categorical_prediction_TRUE]])
}


calculate_mean_percentage_prediction_TRUE_for_outcome_TRUE <- function(data, prediction_class_TRUE, outcome, outcome_TRUE){
  as.numeric(
    data %>%
      group_by(!!sym(outcome)) %>%
      summarise(percentage_prediction = mean(!!sym(prediction_class_TRUE))) %>%
      filter(!!sym(outcome) == outcome_TRUE) %>%
      select(percentage_prediction)
  )
}


calculate_confusion_matrix_derived_measures <- function(confusion_matrix, positive_outcome){
  tidy(
    confusionMatrix(
      confusion_matrix,
      positive = positive_outcome)
  ) %>%
    mutate(estimate = case_when(
      term == "mcnemar" ~ p.value,
      TRUE ~ estimate)
    ) %>%
    filter(term != "mcnemar") %>%
    select(term, estimate) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )
}

# Functions to assess 'crossing-over' in confusion matrices based on aggregate results of discordance status.

calculate_crossover_within_confusion_matrix_aggregate_results <- function(data, outcome){
  data_with_recoded_categorical_reference <- recode_categorical_reference(data)
  data_with_confusion_matrix <- calculate_confusion_matrix_reference_results(data_with_recoded_categorical_reference, outcome)
  percentage_per_category <- calculate_percentage_per_category(data_with_confusion_matrix, "confusion_matrix")
  mean_percentage_discordant_per_category <- calculate_mean_percentage_discordant_per_category(data_with_confusion_matrix, "confusion_matrix")
  combined <- left_join(percentage_per_category, mean_percentage_discordant_per_category, by = "confusion_matrix")
  percentage_discordant_per_simulation <- calculate_percentage_discordant_per_simulation(combined)
  summary <- summarise_crossover_aggregate_results(percentage_discordant_per_simulation)
  return(summary)
}


recode_categorical_reference <- function(data){
  data %>%
    mutate(
      categorical_reference = as.numeric(categorical_reference)
    )
}


calculate_confusion_matrix_reference_results <- function(data, outcome){
  data %>%
    mutate(
      confusion_matrix = case_when(
        categorical_reference == 1 & categorical_reference == !!sym(outcome) ~ "true_positive",
        categorical_reference == 1 & categorical_reference != !!sym(outcome) ~ "false_positive",
        categorical_reference == 0 & categorical_reference == !!sym(outcome) ~ "true_negative",
        categorical_reference == 0 & categorical_reference != !!sym(outcome) ~ "false_negative"
      )
    )
}


calculate_percentage_per_category <- function(data, category){
  data %>%
    group_by(!!sym(category)) %>%
    summarise(percentage_per_category = n() / nrow(data) * 100) %>%
    ungroup()
}


calculate_mean_percentage_discordant_per_category <- function(data, category){
  data %>%
    group_by(!!sym(category)) %>%
    summarise(mean_percentage_discordant = mean(percentage_discordant)) %>%
    ungroup()
}


calculate_percentage_discordant_per_simulation <- function(data){
  data <- data %>%
    mutate(percentage_discordant_per_simulation = 
             (percentage_per_category / 100) * 
             (mean_percentage_discordant / 100) *
             100)
}


summarise_crossover_aggregate_results <- function(data){
  data <- data %>% 
    select(confusion_matrix, percentage_discordant_per_simulation) %>%
    rename(percentage_discordant = percentage_discordant_per_simulation) %>%
    pivot_wider(
      names_from = confusion_matrix,
      values_from = percentage_discordant,
      names_prefix = "percentage_discordant_"
    ) %>%
    mutate(fn_minus_tp_percentage_point_diff = percentage_discordant_false_negative - percentage_discordant_true_positive,
           fp_minus_tn_percentage_point_diff = percentage_discordant_false_positive - percentage_discordant_true_negative,
           percentage_discordant_sum = percentage_discordant_false_negative + percentage_discordant_true_positive + percentage_discordant_false_positive + percentage_discordant_true_negative)
}


get_crossover_summary_as_vector <- function(data, indices, outcome) {
  data_sample <- data[indices,]
  data_sample_summary_crossover <- calculate_crossover_within_confusion_matrix_aggregate_results(data_sample, outcome)
  return(unlist(data_sample_summary_crossover, use.names = FALSE))
}


get_variables_names_from_crossover_summary_as_vector <- function(data, outcome){
  summary <- list(names(calculate_crossover_within_confusion_matrix_aggregate_results(data, outcome)))
  return(unlist(summary))
}
