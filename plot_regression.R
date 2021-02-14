#' Plot the dataset, the OLS lines, and the residual analysis plots
#' 
#' @param df a data frame containing x and y values for regression
#' @param user_intercept an numeric value, intercept of line chosen by the user
#' @param user_slope an numeric value, slope of line chosen by the user
#' @param x_axis_label a character value, label for x-axis
#' @param y_axis_label a character value, label for y-axis
#' @param show_residual_analysis a logical value, 
#' If `TRUE`, the output plots contain residual analysis.
#' @param show_ols_line a logical value, 
#' If `TRUE`, the output plot(s) contains the OLS lines
#'
#' @return a ggplot or ggarrange object
#' @export
#'
#' @examples
plot_regression <- function(df,
                            user_intercept = 1,
                            user_slope = 1,
                            x_axis_label = "x",
                            y_axis_label = "y",
                            show_user_line = TRUE,
                            show_residual_analysis = FALSE,
                            show_ols_line = FALSE) {

  # Fitted values and residuals based on user input -------------------------
  df <- df %>% 
    mutate(y_hat = user_intercept + user_slope * x,
           residual = y - y_hat)
  
  # Scatter plot with the regression line defined by the user ---------------
  data_user_line_fig <- df %>% 
    ggplot(aes(x = x, y = y)) +
    # Scatter plot of data
    geom_point(color = "#5287B9") +
    # Regression line defined by the user
    geom_abline(aes(intercept = user_intercept, 
                    slope = user_slope),
                color = "#C15F56",
                size = 1.5) +
    # y-axis limits adapted to user choices
    ylim(min(user_intercept + user_slope * df$x, min(df$y)), 
         max(user_intercept + user_slope * df$x, max(df$y))) +
    labs(title = paste0(y_axis_label, " vs. ", x_axis_label),
         x = x_axis_label, 
         y = y_axis_label) + 
    theme_bw() +
    theme(plot.title = element_text(face = "bold", 
                                    size = 20))

  # QQ Plot -----------------------------------------------------------------
  qq_fig <- df %>% 
    ggplot(aes(sample = scale(residual))) +
    # qq line
    geom_qq_line(linetype = "dashed",
                 color = "gray",
                 size = 1.2) +
    # qq points
    geom_qq(color = "#5287B9") +
    labs(title = "Normal Quantile-Quantile Plot",
         x = "Theoretical Quantiles", 
         y = "Standardized Residuals") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", 
                                    size = 20))
  
  # Density of Standardized Residuals Plot ----------------------------------
  residual_density_fig <- df %>%
    ggplot(aes(x = residual)) +
    # Density of Residuals
    geom_density(color = "#5287B9",
                 size = 1.5,
                 adjust = 2) +
    # Density of std. normal
    geom_line(data = tibble(x = seq(-3, 3, by = 0.2),
                            y = dnorm(x)),
              aes(x = x, y = y),
              color = "#C15F56",
              linetype = "dashed") +
    # label for residual density
    geom_label(data = tibble(x = mean(df$residual), y = 0.1),
               aes(x = x, y = y),
               label = "Residuals",
               color = "#5287B9") +
    # label for std. normal density
    geom_label(data = tibble(x = 0, y = 0.3),
               aes(x = x, y = y),
               label = "Std. Normal",
               color = "#C15F56") +
    labs(title = "Density of Residuals",
         x = "Residuals",
         y = "Density") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold",
                                    size = 20))

  # Residuals vs. Fitted values ---------------------------------------------
  if (show_residual_analysis == TRUE) {
    residual_fitted_values_fig <- df %>% 
      ggplot(aes(x = y_hat, y = residual)) +
      # horizontal line at zero and mean of residuals
      geom_hline(yintercept = c(0, mean(df$residual)), 
                 linetype = "dashed",
                 color = c("gray", "#8db9e3"),
                 size = 1.2) +
      # residuals
      geom_point(color = "#5287B9") +
      # label for mean residual
      geom_label(data = tibble(x = quantile(df$y_hat, probs = 0.8), 
                               y = mean(df$residual) + 1),
                 aes(x = x, y = y),
                 label = paste("Residual Mean: ", 
                               round(mean(df$residual), 3),
                               "\n Residual Variance" , 
                               round(var(df$residual), 3)),
                 color = "#5287B9",
                 size = 4) +
      labs(title = "Residuals vs. Fitted Values",
           x = "Fitted Values", 
           y = "Residuals") + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold", 
                                      size = 20))
  }
  
  # Show both OLS line and residuals ----------------------------------------
  if (show_ols_line == TRUE & show_residual_analysis == TRUE) {
    data_user_line_ols_residual_fig <- data_user_line_fig +
      geom_segment(aes(xend = x, yend = y_hat), 
                   linetype = "dashed", 
                   color = "grey") +
      geom_smooth(formula = "y ~ x",
                  method = "lm",
                  se = FALSE,
                  color = "#189B41",
                  size = 1.5)
    
    ggarrange(plotlist = list(data_user_line_ols_residual_fig, 
                              residual_fitted_values_fig, 
                              residual_density_fig, 
                              qq_fig),
              nrow = 2, ncol = 2)

  # Show OLS but not the residuals ------------------------------------------
  } else if (show_ols_line == TRUE & show_residual_analysis == FALSE) {
    data_user_line_fig +
      geom_smooth(formula = "y ~ x",
                  method = "lm",
                  se = FALSE,
                  color = "#189B41",
                  size = 1.5)
  
  # Show residuals but not the OLS line -------------------------------------
  } else if (show_ols_line == FALSE & show_residual_analysis == TRUE) {
    data_user_line_residual_fig <- data_user_line_fig +
      geom_segment(aes(xend = x, yend = y_hat), 
                   linetype = "dashed", 
                   color = "grey")
    
    ggarrange(plotlist = list(data_user_line_residual_fig, 
                              residual_fitted_values_fig, 
                              residual_density_fig, 
                              qq_fig),
              nrow = 2, ncol = 2)
  
  # Do not show residuals and the OLS line  ---------------------------------
  } else {
    data_user_line_fig
  }
}