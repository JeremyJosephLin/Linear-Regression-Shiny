library(dplyr)
library(ggplot2)
library(ggpubr)

source("plot_regression.R")

# Simulate data -----------------------------------------------------------
n <- 100
beta_0 <- -2
beta_1 <- 1
sigma <- 1 
x <- seq(0, 2, length.out = n)
y <- rnorm( n,mean = beta_0 + beta_1 * x, sd = sigma)

# Test plot_regression ----------------------------------------------------
df <- tibble(x = x, y = y)

# Show no residuals and no best fit line
plot_regression(df, 
                user_intercept = 1, 
                user_slope = 1, 
                x_axis_label = "x axis",
                y_axis_label = "y axis",
                show_residual_analysis = FALSE,
                show_ols_line = FALSE)

# Show residuals but not best fit line
plot_regression(df, 
                user_intercept = 1, 
                user_slope = 1, 
                x_axis_label = "x axis",
                y_axis_label = "y axis",
                show_residual_analysis = TRUE,
                show_ols_line = FALSE)

# Show best fit line but not residuals
plot_regression(df, 
                user_intercept = 1, 
                user_slope = 1, 
                x_axis_label = "x axis",
                y_axis_label = "y axis",
                show_residual_analysis = FALSE,
                show_ols_line = TRUE)

# Show both best fit line and residuals
plot_regression(df, 
                user_intercept = 1, 
                user_slope = 1, 
                x_axis_label = "x axis",
                y_axis_label = "y axis",
                show_residual_analysis = TRUE,
                show_ols_line = TRUE)