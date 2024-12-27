library(dplyr)
library(tidyverse) # metapackage of all tidyverse packages
library(car) # for the vif function to check for multicollinearity
library(ggplot2) # for data visualization
library(readr) # for data import
library(tidyr) # for data tidying
library(broom) # for tidy statistical summaries
library(stringr) # for string manipulation
library(rlang)

handle_outliers <- function(data, column_name) {
  ggplot(data, aes(x = "", y = !!sym(column_name))) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", column_name), y = column_name)

  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  outliers <- data[[column_name]][data[[column_name]] < lower_bound | data[[column_name]] > upper_bound]
  print(paste("Outliers in", column_name, ":", paste(outliers, collapse = ", ")))

  data[[column_name]][data[[column_name]] < lower_bound] <- lower_bound
  data[[column_name]][data[[column_name]] > upper_bound] <- upper_bound

  return(data)
}

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
  cat("\n=== diabetes and Glucose Level Analysis ===\n")
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
      x = str_to_title(str_replace_all(group_col, "_", " ")),
      y = str_to_title(str_replace_all(value_col, "_", " "))
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

create_report <- function(diabetes_glucose_interpretation, bp_diabetes_interpretation) {
  cat("\n=== Hypothesis Testing Results ===\n\n")

  cat("1. diabetic and Glucose Levels:\n")
  cat("   - ", diabetes_glucose_interpretation$conclusion, "\n")
  cat("   - P-value:", format.pval(diabetes_glucose_interpretation$p_value), "\n")
  cat("   - 95% CI:", paste(
    round(diabetes_glucose_interpretation$confidence_interval, 2), collapse = " to "
  ), "\n\n")

  cat("2. Blood Pressure and Diabetes Status:\n")
  cat("   - ", bp_diabetes_interpretation$conclusion, "\n")
  cat("   - P-value:", format.pval(bp_diabetes_interpretation$p_value), "\n")
  cat("   - 95% CI:", paste(
    round(bp_diabetes_interpretation$confidence_interval, 2), collapse = " to "
  ), "\n")
}

generate_random_samples <- function(num_of_samples, sample_size, data) {
  samples_list <- vector("list", num_of_samples)
  for (i in seq_len(num_of_samples)) {
    samples_list[[i]] <- sample(data, sample_size)
  }
  return(samples_list)
}

calculate_stats <- function(samples_list) {
  lapply(samples_list, function(sample) {
    list(MEAN = mean(sample), STD = sd(sample))
  })
}

calculate_confidence_intervals <- function(samples_stats, n, confidence_percentage) {
  alpha <- (100 - confidence_percentage) / 100 / 2
  quartile <- confidence_percentage / 100 + alpha
  t_critical <- qt(quartile, df = n - 1)
  lapply(samples_stats, function(stats) {
    x_bar <- stats$MEAN
    standard_deviation <- stats$STD
    margin <- t_critical * standard_deviation / sqrt(n)
    list(LOW = x_bar - margin, HIGH = x_bar + margin)
  })
}

calculate_CIs_proportion <- function(CIs, value) {
  cat("The given value is ", value, "\n")
  contained_count <- sum(sapply(CIs, function(CI) {
    contained_flag <- (value >= CI$LOW && value <= CI$HIGH)
    cat("Confidence Interval -> Lower bound: ", CI$LOW, ", Upper bound: ", CI$HIGH, ", Contain the value: ", contained_flag, "\n")
    contained_flag
  }))
  cat("Number of confidence intervals that contain the true population mean: ", contained_count, "\n")
  return(contained_count / length(CIs))
}

calculate_CI_widths <- function(CIs) {
  sapply(CIs, function(CI) CI$HIGH - CI$LOW)
}
