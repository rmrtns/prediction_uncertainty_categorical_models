library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggsci)

plot_normal_distribution <- function(){
  ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
    stat_function(fun = dnorm, geom = "area") + 
    scale_x_continuous(
      breaks = c(-3, -2, -1, 0, 1, 2, 3),
      labels = c("-3sd", "-2sd", "-1sd", "mean", "+1sd", "+2sd", "+3sd")) +
    scale_y_continuous(breaks = NULL) +
    theme_classic() +
    theme(
      axis.line.y = element_blank(),
      axis.title = element_blank()
    )
}


read_files <- function(directory){
  file <- read.csv(directory)
  outcome <- file %>% 
    mutate(
      variation = str_extract(
        directory, pattern = "[a-z]+_cv"),
      cv_level = paste0(
        str_sub(
          str_extract(
            directory, pattern = "cv_[:alnum:]+"
          ),
          start = 4L,
          end = -1L
        ),
        "x"
      )
    )%>%
    relocate(variation, cv_level)
}


tidy_data <- function(data){
  data %>%  
    mutate(
      variation = case_when(
        variation == "bio_cv" ~ "biological",
        variation == "an_cv" ~ "analytical",
        variation == "tot_cv" ~ "total"
      ),
      cv_level = case_when(
        cv_level == "basex" ~ "base",
        cv_level == "1dot5x" ~ "1.5x",
        TRUE ~ cv_level
      )
    ) %>%
    mutate(
      variation = factor(
        variation,
        levels = c("biological", "analytical", "total")
      ),
      cv_level = factor(
        cv_level, 
        levels = c("base", "1.5x", "2x", "4x", "6x"))
    )
}


combine_stat_with_ci <- function(data, lower, upper, decimal_places){
  data %>%
    mutate(
      across(where(is.numeric), \(x) replace_na(x, 0)),
      across(where(is.numeric), \(x) format(
        round(x, decimal_places), nsmall = decimal_places)
      ),
      cv = paste0(
        cv_level, " ", variation
      ),
      stat_with_ci = paste0(
        original, " (", !!sym(lower), "; ", !!sym(upper), ")"
      )
    ) %>%
    select(variable, cv, stat_with_ci) %>%
    pivot_wider(
      names_from = cv,
      values_from = stat_with_ci
    ) %>%
    select(
      variable, 
      `base biological`, 
      `base analytical`, `1.5x analytical`, `2x analytical`, `4x analytical`, `6x analytical`, 
      `base total`, `1.5x total`, `2x total`, `4x total`, `6x total`
    )
}


get_plot_by_cv_level <- function(data, plot_variable, y_label, y_limits){
  ggplot(
    data = data %>%
      filter(variable == plot_variable),
    aes(
      x = cv_level, 
      group = variation, shape = variation, color = variation)
  ) + 
    geom_point(
      aes(y = original),
      size = 2,
      position = position_dodge(0.4)
    ) +
    geom_errorbar(
      aes(ymin = bca_conf_low, ymax = bca_conf_high), 
      linewidth = 0.7,
      width = 0.2,
      position = position_dodge(0.4)
    ) +
    coord_cartesian(
      ylim = y_limits
    ) +
    labs(
      x = "Multiples of analytical variation",
      y = y_label
    ) + 
    scale_color_nejm() + 
    theme_bw() + 
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      panel.grid.major = element_line(color = "lightgrey")
    )
}