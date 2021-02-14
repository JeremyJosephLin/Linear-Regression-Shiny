library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggpubr)

source("plot_regression.R")

# function to select intercept range 
intercept_range <- function(data){
  if(data == "iris"){choices = c(-20:20)}
  if(data == "mtcars"){choices = c(-100:600)}
  if(data == "trees"){choices = c(-200:200)}
  return(choices)  
}
# function to select slope range
slope_range <- function(data){
  if(data == "iris"){choices = c(-20:20)}
  if(data == "mtcars"){choices = c(-100:100)}
  if(data == "trees"){choices = c(-20:20)}
  return(choices)  
}

# UI ----------------------------------------------------------------------
ui <- dashboardPage(skin = "blue",

  dashboardHeader(title = "Linear Regression"
                  # tags$img(src = "uci.png",
                  #          width = "50%", height = "50%")
                  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    # First row -------------------------------------------------------------
    # contains dataset selection and  x/y variable selection in the left column
    # and scatter plot in the right column
    
  
    fluidRow(
      column(
        width = 3,
        
        fluidRow(box(width = 12,
          selectInput(
            inputId = "data",
            label = "Choose a dataset",
            choices = c("iris", "mtcars", "trees")
          )
        )), # End first sub-row
      
        fluidRow(
          # Change drop-down menus based on datasets ----
          # iris ----
          conditionalPanel(condition = "input.data == 'iris'",
                           box(width = 12,
                             # Select explanatory variable
                             selectInput(
                               inputId = "iris_x_var",
                               label = "Choose the explanatory variable",
                               choices = names(iris[, 1:4])
                             ),
                             
                             # Select response variable
                             selectInput(
                               inputId = "iris_y_var",
                               label = "Choose the response variable",
                               choices = names(iris[, 1:4]),
                               selected = names(iris[, 1:4])[2]
                             )
                           )
          ), # End conditional panel for iris
        
          # mtcars ----
          conditionalPanel(condition = "input.data == 'mtcars'",
                           box(width = 12,
                             # Select explanatory variable
                             selectInput(
                               inputId = "mtcars_x_var",
                               label = "Choose the explanatory variable",
                               choices = names(mtcars[, c(1, 3:7)])
                             ),
                             
                             selectInput(
                               inputId = "mtcars_y_var",
                               label = "Choose the response variable",
                               choices = names(mtcars[, c(1, 3:7)]),
                               selected = names(mtcars[, c(1, 3:7)])[2]
                             )
                           )
          ), # End conditional panel for mtcars
        
        # trees ----
        conditionalPanel(condition = "input.data == 'trees'",
                         box(width = 12,
                           # Select explanatory variable
                           selectInput(
                             inputId = "trees_x_var",
                             label = "Choose the explanatory variable",
                             choices = names(trees)
                           ),
                           
                           selectInput(
                             inputId = "trees_y_var",
                             label = "Choose the response variable",
                             choices = names(trees),
                             selected = names(trees)[2]
                           )
                         )
        ) # End conditional panel for trees
        ), # End second sub-row
        
        # fluidRow(
        #   box(width = 12,
        #       # Intercept
        #       sliderInput(
        #         inputId = "intercept",
        #         label = "Choose an intercept",
        #         value = 0,
        #         min = -10,
        #         max = 10,
        #         step = 0.01
        #       ),
        #       
        #       # Slope
        #       sliderInput(
        #         inputId = "slope",
        #         label = "Choose a slope",
        #         value = 0,
        #         min = -10,
        #         max = 10,
        #         step = 0.01
        #       )
        #   )
        # ), # End third sub-row
        
      ), # End first column
    
      column(width = 9, align = "center",
             # plotOutput("scatter")
             # column width needs to be 8
             box(width = 12, plotOutput("scatter"))
      ), # End second column
      
    ), # End first row
    
    
    # Second Row ------------------------------------------------------------
    # contains 'show residuals' and 'show best fit line' buttons and the
    # user regression summary in the left column, and 
    fluidRow(
      # Start Left column
      column(width = 3),
      
      # Start Left column
      column(
        width = 3,
        box(width = 12,
          # Show residuals
          checkboxInput(
            inputId = "show_residuals",
            label = "Show residuals",
            value = FALSE
          ),
          
          # Show OLS line
          checkboxInput(
            inputId = "show_best_fit",
            label = "Show best fit line",
            value = FALSE
          ),
          
          # Table of estimated coefficients and MSE of the user input
          tableOutput("user_regression_summary"),
          
          # Prediction 
          numericInput(
            inputId = "new_x",
            label = "Show best fit line",
            value = 0
          )
        )
      ), # End left column
      
      # Start center column
      column(width = 3,
             
             box(width = 12,
               # Intercept
               uiOutput(outputId = "intercept_sliders"),
               
               # Slope
               # sliderInput(
               #   inputId = "slope",
               #   label = "Choose a slope",
               #   value = 0,
               #   min = -10,
               #   max = 10,
               #   step = 0.01
               # )
               uiOutput(outputId = "slope_sliders")
             )
      ), # End center column
      
      # Start right column
      column(width = 3,
             box(width = 12,
                 conditionalPanel(
                   condition = "input.show_best_fit == 1",
                    # Table of estimated coefficients and MSE of the OLS
                    tableOutput(outputId = "ols_regression_summary")
                  )#end conditional
                )# end box
      ) # End right column
    ) # End second row
    
  )
  
)# End ui


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  vals <- reactiveValues(count = -1)
  observeEvent(input$range, vals$count <- vals$count + 1)
  
  # adaptive slider ---------------------------------------------------------
  # data frame for slider position 
  slider_data <- reactive({
    if (input$data == 'iris') {
      # iris dataset ------------------------------------------------------
      # x, y, y_hat, x and y axis labels
      tibble(x = iris[, input$iris_x_var],
             y = iris[, input$iris_y_var])
    } else if (input$data == 'mtcars') {
      # mtcars dataset ----------------------------------------------------
      # x, y, y_hat, x and y axis labels
      tibble(x = mtcars[, input$mtcars_x_var],
             y = mtcars[, input$mtcars_y_var])
    } else if (input$data == 'trees') {
      # trees dataset ----------------------------------------------------
      # x, y, y_hat, x and y axis labels
      tibble(x = trees[, input$trees_x_var],
             y = trees[, input$trees_y_var])
    }
  })
  
  # Create adaptive slider ui 
  output$intercept_sliders <- renderUI({
    sliderInput(
      inputId = "intercept",
      label = "Choose an intercept",
      min = min(intercept_range(input$data)),
      max = max(intercept_range(input$data)),
      value = (min(slider_data()$y) + max(slider_data()$y))/2,
      step =1)
  })
  
  output$slope_sliders <- renderUI({
    sliderInput(
      inputId = "slope",
      label = "Choose a slope",
      min = min(intercept_range(input$data)),
      max = max(intercept_range(input$data)),
      value = 0,
      step =.001)
  })
  observeEvent(vals$count > 2,{
  # Choose Dataset --------------------------------------------------------
  df <- reactive({
    if (input$data == 'iris') {
      # iris dataset ------------------------------------------------------
      # x, y, y_hat, x and y axis labels
      list(data = tibble(x = iris[, input$iris_x_var],
                         y = iris[, input$iris_y_var],
                         y_hat = input$intercept + input$slope * x),
           x_name = input$iris_x_var,
           y_name = input$iris_y_var)
    } else if (input$data == 'mtcars') {
      # mtcars dataset ----------------------------------------------------
      # x, y, y_hat, x and y axis labels
      list(data = tibble(x = mtcars[, input$mtcars_x_var],
                         y = mtcars[, input$mtcars_y_var],
                         y_hat = input$intercept + input$slope * x),
           x_name = input$mtcars_x_var,
           y_name = input$mtcars_y_var)
    } else if (input$data == 'trees') {
      # trees dataset ----------------------------------------------------
      # x, y, y_hat, x and y axis labels
      list(data = tibble(x = trees[, input$trees_x_var],
                         y = trees[, input$trees_y_var],
                         y_hat = input$intercept + input$slope * x),
           x_name = input$trees_x_var,
           y_name = input$trees_y_var)
    }
  })
  
  # Scatter Plot ----------------------------------------------------------
  output$scatter <- renderPlot({
    plot_regression(
      df()$data,
      user_slope = input$slope,
      user_intercept = input$intercept,
      x_axis_label = df()$x_name,
      y_axis_label = df()$y_name,
      show_residual_analysis = input$show_residuals,
      show_ols_line = input$show_best_fit
    )
  })
  
  # User Regression Summary -----------------------------------------------
  user_regression_summary <-  reactive({
    user_mse <- sum((df()$data$y_hat - df()$data$y)^2) / (length(df()$data$x) - 2)
    
    tibble(
      Quantity = c("Intercept",
                   "Slope",
                   "MSE"),
      Value = as.character(c(input$intercept,
                             input$slope,
                             round(user_mse, 3))))
  })
  
  # Show data frame in an HTML table
  output$user_regression_summary <- renderTable(
    expr = user_regression_summary(),
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    caption = "Regression Summary of Line Defined by the User"
  )
  
  # OLS Regression Summary ------------------------------------------------
  ols_regression_summary <- reactive({
    ols_fit <- lm(df()$data$y ~ df()$data$x)
    ols_mse <- sum(ols_fit$residuals^2) / ols_fit$df.residual
    
    tibble(
      Quantity = c("Intercept",
                   "Slope",
                   "MSE"),
      Value = as.character(c(round(ols_fit$coefficients[1], 3),
                             round(ols_fit$coefficients[2], 3),
                             round(ols_mse, 3))))
  })
  
  # Show data frame in an HTML table
  output$ols_regression_summary <- renderTable(
    expr = ols_regression_summary(),
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    caption = "OLS Regression Summary"
  )
  })  
}

# ShinyApp ----------------------------------------------------------------
shinyApp(ui = ui, server = server)



























