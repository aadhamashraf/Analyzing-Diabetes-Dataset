library(dplyr)
library(tidyverse) # metapackage of all tidyverse packages
library(car) # for the vif function to check for multicollinearity
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
library(readr) # for data import
library(tidyr) # for data tidying
library(broom) # for tidy statistical summaries
library(stringr) # for string manipulation



## Functions for Part 3
create_summary_stats <- function(data, group_col, value_col) {
  data %>%
    group_by(!!sym(group_col)) %>%
    summarise(
      n = n(),
      mean = mean(!!sym(value_col)),
      sd = sd(!!sym(value_col)),
      se = sd / sqrt(n)
    )
}
print_results <- function(t_test_result, bp_t_test) {
  cat("\n=== BMI and Glucose Level Analysis ===\n")
  print(t_test_result)

  cat("\n=== Blood Pressure and Diabetes Status Analysis ===\n")
  print(bp_t_test)
}
create_boxplot <- function(data, group_col, value_col, title) {
  ggplot(data, aes(x = .data[[group_col]], y = .data[[value_col]])) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.3) +
    theme_minimal() +
    labs(
      title = title,
      x = stringr::str_to_title(stringr::str_replace_all(group_col, "_", " ")),
      y = stringr::str_to_title(stringr::str_replace_all(value_col, "_", " "))
    )
}
create_qq_plot <- function(data, value_col, group_col) {
  ggplot(data, aes(sample = .data[[value_col]])) +
    geom_qq() +
    geom_qq_line() +
    facet_wrap(~ .data[[group_col]]) +
    theme_minimal() +
    labs(title = "Normal Q-Q Plot by Group")
}
interpret_hypothesis_test <- function(test_result, alpha = 0.05) {
  conclusion <- if (test_result$p.value < alpha) {
    "Reject the null hypothesis"
  } else {
    "Fail to reject the null hypothesis"
  }

  list(
    test_statistic = test_result$statistic,
    p_value = test_result$p.value,
    confidence_interval = test_result$conf.int,
    conclusion = conclusion
  )
}
create_report <- function(
  bmi_glucose_interpretation, bp_diabetes_interpretation
) {
  cat("\n=== Hypothesis Testing Results ===\n\n")

  cat("1. BMI and Glucose Levels:\n")
  cat("   - ", bmi_glucose_interpretation$conclusion, "\n")
  cat("   - P-value:", format.pval(bmi_glucose_interpretation$p_value), "\n")
  cat("   - 95% CI:", paste(
    round(bmi_glucose_interpretation$confidence_interval, 2), collapse = " to "
  ), "\n\n")

  cat("2. Blood Pressure and Diabetes Status:\n")
  cat("   - ", bp_diabetes_interpretation$conclusion, "\n")
  cat("   - P-value:", format.pval(bp_diabetes_interpretation$p_value), "\n")
  cat("   - 95% CI:", paste(
    round(bp_diabetes_interpretation$confidence_interval, 2), collapse = " to "
  ), "\n")
}