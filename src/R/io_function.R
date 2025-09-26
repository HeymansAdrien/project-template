
########################################
# Script Title
# Subtitle
# Author of the file:
# Date: 
#
# Function in io_function
########################################


library(tidyverse)
library(data.table)
`%!in%` <- compose(`!`, `%in%`)



#' Simple analysis with boxplot, HSD, and Tukey test
#'
#' This function performs a simple analysis of group differences:
#' - summarises data by group and computes maximum values
#' - runs an ANOVA followed by Tukey and HSD post-hoc tests
#' - generates a boxplot annotated with HSD group letters
#' - generates a Tukey confidence interval plot
#'
#' @param path_to_csv Character string. Path to the CSV file containing the dataset.
#' @param Y Character string. Name of the numeric response variable column in the dataset (default = `"value"`).
#' @param X Character string. Name of the grouping variable column in the dataset (default = `"genotype"`).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{summary} Data frame with group-wise maximum values and HSD group letters.
#'   \item \code{hsd} Data frame of HSD test group assignments.
#'   \item \code{tukey} Data frame of Tukey test results with confidence intervals.
#'   \item \code{plot_box} ggplot object for the annotated boxplot.
#'   \item \code{plot_tukey} ggplot object for the Tukey confidence interval plot.
#' }
#'
#' @examples
#' \dontrun{
#' # Example with a CSV file containing columns "genotype" and "value"
#' results <- make_simple_analysis("data/my_experiment.csv",
#'                                 Y = "value",
#'                                 X = "genotype")
#'
#' # View summary
#' results$summary
#'
#' # Show boxplot
#' print(results$plot_box)
#'
#' # Show Tukey confidence intervals
#' print(results$plot_tukey)
#' }
#'
#' @import tidyverse agricolae viridis
#' @export
make_simple_analysis <- function(path_to_csv, Y = "value", X = "genotype") {
  # Load necessary libraries
  library(tidyverse)
  library(agricolae)
  library(viridis)
  
  # read data
  data <- read.csv(path_to_csv)
  
  # summarize by group
  data.summarized <- data %>%
    group_by(.data[[X]]) %>%
    summarize(Max.value = max(.data[[Y]]), .groups = "drop")
  
  # run HSD test
  formula <- as.formula(paste(Y, "~", X))
  hsd <- agricolae::HSD.test(aov(formula, data = data), X, group = TRUE)
  
  # format HSD results
  hsd_df <- data.frame(hsd$groups) %>%
    mutate(!!X := row.names(hsd$groups)) %>%
    select(-value)
  
  # merge summary + HSD
  data.summarized <- left_join(data.summarized, hsd_df, by = X)
  
  ##################
  ### main graph ###
  ##################
  p1 <- ggplot(data, aes(x = reorder(.data[[X]], .data[[Y]], FUN = median),
                         y = .data[[Y]])) +
    geom_boxplot(aes(fill = .data[[X]]), alpha = 0.6) +
    geom_text(data = data.summarized,
              aes(x = .data[[X]],
                  y = 0.2 + Max.value,
                  label = groups),
              vjust = 0) +
    theme_classic() +
    viridis::scale_fill_viridis(discrete = TRUE)
  
  # tukey HSD
  tuk <- TukeyHSD(aov(formula, data = data))
  tuk_df <- data.frame(tuk[[X]]) %>%
    mutate(lab = row.names(tuk[[X]]))
  
  # tukey plot
  p2 <- ggplot(tuk_df) +
    geom_segment(aes(x = lab, xend = lab, y = lwr, yend = upr)) +
    geom_hline(yintercept = 0) +
    coord_flip()
  
  # return outputs
  list(
    summary = data.summarized,
    hsd = hsd_df,
    tukey = tuk_df,
    plot_box = p1,
    plot_tukey = p2
  )
}
