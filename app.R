# Libraries and Functions -------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggpubr)

source("plot_regression.R")

# ui ----------------------------------------------------------------------
ui <- dashboardPage(
  # _Header ----
  dashboardHeader(
    title = "Simple Linear Regression", 
    titleWidth = 300
  ),
  
  # _Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      
      # __Dashoboard menu ----
      menuItem(
        text = "Dashboard", 
        tabName = "dashboard", 
        icon = icon("dashboard")
      ),
      
      # __Data Summary menu ----
      menuItem(
        text = "Data Summary", 
        tabName = "raw_data", 
        icon = icon("table")
      ),
      
      # __Name of developers ----
      menuItem(
        text = "Developers", 
        tabName = "developers", 
        icon = icon("user", lib = "glyphicon")
      )
    ) # End sidebarMenu
  ), # End dashboardSidebar
  
  # _Body ----
  dashboardBody(
    tabItems(
      # __Dashboard tab in the sidebar ----
      tabItem(
        tabName = "dashboard",
        
        # First row of the dashboard
        # contains:
        #   - data selection drop down menu 
        #   - x and y variable drop down menu
        #   - regression plots
        fluidRow(
          
          # ___Data and variable selection box ----
          box(
            title = "Data Selection", 
            status = "primary",
            solidHeader = TRUE,
            width = 4,
          
            # ____Dataset selection ----
            selectInput(
              inputId = "data",
              label = "Choose a dataset",
              choices = c("iris", "mtcars", "trees")
            ),
            
            # ____Variable Selection ----
            # iris
            conditionalPanel(
              condition = "input.data == 'iris'",
              
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
            ), # End conditionalPanel for iris
            
            # mtcars
            conditionalPanel(
              condition = "input.data == 'mtcars'",
              
              # Select explanatory variable
              selectInput(
                inputId = "mtcars_x_var",
                label = "Choose the explanatory variable",
                choices = names(mtcars[, c(1, 3:7)])
              ),
              
              # Select response variable
              selectInput(
                inputId = "mtcars_y_var",
                label = "Choose the response variable",
                choices = names(mtcars[, c(1, 3:7)]),
                selected = names(mtcars[, c(1, 3:7)])[2]
              )
            ), # End conditionalPanel for mtcars
            
            # trees
            conditionalPanel(
              condition = "input.data == 'trees'",
              
              # Select explanatory variable
              selectInput(
                inputId = "trees_x_var",
                label = "Choose the explanatory variable",
                choices = names(trees)
              ),
              
              # Select response variable
              selectInput(
                inputId = "trees_y_var",
                label = "Choose the response variable",
                choices = names(trees),
                selected = names(trees)[2]
              )
            ) # End conditionalPanel for trees
          ), # End dataset and variable selection box
          
          # ___Regression plots ----
          box(
            title = "Regression Plots", 
            status = "info",
            solidHeader = TRUE,
            width = 8,
            
            # Show residual analysis
            checkboxInput(
              inputId = "show_residual_analysis",
              label = "Show Residuals",
              value = FALSE
            ),
            
            # Regression plots
            plotOutput("scatter")
          )
        ), # End First row
        
        # Second row of the dashboard
        # contains:
        #   - sliders for intercept and slope
        #   - regression summaries and prediction
        fluidRow(
          
          # ___Sliders ----
          box(
            title = "User Regression Input", 
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            
            # ____iris ----
            conditionalPanel(
              condition = "input.data == 'iris'",
              # Intercept
              sliderInput(
                inputId = "intercept_iris",
                label = "Choose an intercept:",
                min = -20,
                max = 20,
                value = 0,
                step = 0.01
              ),
              
              # Slope
              sliderInput(
                inputId = "slope_iris",
                label = "Chosse a slope:",
                min = -20,
                max = 20,
                value = 0,
                step = 0.01
              )
            ), # End conditionalPanel for iris
            
            # ____mtcars ----
            conditionalPanel(
              condition = "input.data == 'mtcars'",
              # Intercept
              sliderInput(
                inputId = "intercept_mtcars",
                label = "Choose an intercept:",
                min = -100,
                max = 600,
                value = 0,
                step = 0.01
              ),
              
              # Slope
              sliderInput(
                inputId = "slope_mtcars",
                label = "Chosse a slope:",
                min = -20,
                max = 20,
                value = 0,
                step = 0.01
              )
            ), # End conditionalPanel for mtcars
            
            # ____trees ----
            conditionalPanel(
              condition = "input.data == 'trees'",
              # Intercept
              sliderInput(
                inputId = "intercept_trees",
                label = "Choose an intercept:",
                min = -200,
                max = 200,
                value = 0,
                step = 0.01
              ),
              
              # Slope
              sliderInput(
                inputId = "slope_trees",
                label = "Chosse a slope:",
                min = -20,
                max = 20,
                value = 0,
                step = 0.01
              )
            ), # End conditionalPanel for trees
          ), # End Sliders box
          
          # ___User Regression Summary ----
          box(
            title = "Regression Summary from User Input", 
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            tableOutput(outputId = "user_reg_summary")
          ),
          
          # ___OLS Summary and Prediction ----
          box(
            title = "OLS Regression Summary", 
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            # Show OLS line
            checkboxInput(
              inputId = "show_ols_line",
              label = "Show OLS Regression line",
              value = FALSE
            ),
            
            # show OLS Summary and prediction
            conditionalPanel(
              condition = "input.show_ols_line == 1",
              # ____OLS summary ----
              tableOutput(outputId = "ols_reg_summary"),
              
              # ____Prediction ----
              box(
                title = "Prediction",
                status = "info",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                
                # x value for prediction
                numericInput(
                  inputId = "prediction_x",
                  label = "x-value for prediction",
                  value = 10, step = 0.1
                ),
                
                # Prediction corresponding to prediction_x
                tableOutput(outputId = "prediction_y"),
                
                # Plot predicted value on the main plot
                checkboxInput(
                  inputId = "show_prediction",
                  label = "Show Prediction"
                ),
                
                # ____Extrapolation ----
                textOutput(outputId = "extrapolation")
              ) # End prediction
            ) # End OLS regression summary and prediction
          ) # End OLS regression summary
        ) # End second row
      ), # End dashboard tab in the sidebar
      
      # __rawdata tab in the sidebar ----
      tabItem(
        tabName = "raw_data",
        
        fluidRow(
          # Show dataset table
          box(
            title = "Raw Data",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            dataTableOutput("raw_data_table")
          ) # End show dataset
        ), # End fluidRow for show dataset table
        
        fluidRow(
          # Show dataset 5-number summary
          box(
            title = "Five Number summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            dataTableOutput("raw_data_summary")
          ) # end 5-number summary
        ) # End fluidRow for 5-number summary
      ), # End raw Data tab in the sidebar
      
    # __developers tab in the sidebar ----
      tabItem(
        tabName = "developers",
        
        box(
          title = "Developers",
          status = "success",
          solidHeader = TRUE,
          width = 4,
          
          tableOutput("developers")
        )
      ) # End developers tab
    ) # End TabItems
  ) # End dashboardBody
) # End ui

# server ------------------------------------------------------------------
server <- function(input, output) {
 # User intercept input ----
   intercept_input <- reactive({
    # iris 
    if (input$data == 'iris') {
      input$intercept_iris
    # mtcars
    } else if (input$data == 'mtcars') {
      input$intercept_mtcars
    # trees
    } else if (input$data == 'trees') {
      input$intercept_trees
    }
  })
  
  # User slope input ----
  slope_input <- reactive({
    if (input$data == 'iris') {
      # iris 
      input$slope_iris
    } else if (input$data == 'mtcars') {
      # mtcars
      input$slope_mtcars
    } else if (input$data == 'trees') {
      # trees
      input$slope_trees
    }
  })
  
  # Prepare dataset corresponding to user choice ----
  plot_data <- reactive({
    if (input$data == 'iris') {
      # iris 
      list(
        data = tibble(x = iris[, input$iris_x_var],
                      y = iris[, input$iris_y_var],
                      y_hat = intercept_input() + slope_input() * x,
                      residual = y - y_hat),
        n = nrow(iris),
        x_axis_label = input$iris_x_var,
        y_axis_label = input$iris_y_var
      )
    } else if (input$data == 'mtcars') {
      # mtcars
      list(
        data = tibble(x = mtcars[, input$mtcars_x_var],
                      y = mtcars[, input$mtcars_y_var],
                      y_hat = intercept_input() + slope_input() * x,
                      residual = y - y_hat),
        n = nrow(mtcars),
        x_axis_label = input$mtcars_x_var,
        y_axis_label = input$mtcars_y_var
      )
    } else if (input$data == 'trees') {
      # trees
      list(
        data = tibble(x = trees[, input$trees_x_var],
                      y = trees[, input$trees_y_var],
                      y_hat = intercept_input() + slope_input() * x,
                      residual = y - y_hat),
        n = nrow(trees),
        x_axis_label = input$trees_x_var,
        y_axis_label = input$trees_y_var
      )
    }
  }) # End plot_data
  
  # OLS fit of plot_data ----
  ols_fit <- reactive({
    lm(plot_data()$data$y ~ plot_data()$data$x)
  })
  
  # Regression plots ----
  output$scatter <- renderPlot({
    if (input$show_ols_line == FALSE) {
      
      plot_regression(
        plot_data()$data,
        user_intercept = intercept_input(),
        user_slope = slope_input(),
        x_axis_label = plot_data()$x_axis_label,
        y_axis_label = plot_data()$y_axis_label,
        show_residual_analysis = input$show_residual_analysis,
        show_ols_line = FALSE)
    } else if (input$show_prediction == TRUE & input$show_ols_line == TRUE) {
      
      prediction_y <- ols_fit()$coefficients[1] + 
        ols_fit()$coefficients[2] * input$prediction_x
      
      plot_regression(
        plot_data()$data,
        user_intercept = intercept_input(),
        user_slope = slope_input(),
        x_axis_label = plot_data()$x_axis_label,
        y_axis_label = plot_data()$y_axis_label,
        show_residual_analysis = FALSE,
        show_ols_line = TRUE) +
        geom_point(data = tibble(x = input$prediction_x,
                                 y = prediction_y),
                   aes(x = x, y = y), 
                   color = "#C15F56", 
                   size = 5)
    } else if (input$show_prediction == FALSE & input$show_ols_line == TRUE) {
      
      plot_regression(
        plot_data()$data,
        user_intercept = intercept_input(),
        user_slope = slope_input(),
        x_axis_label = plot_data()$x_axis_label,
        y_axis_label = plot_data()$y_axis_label,
        show_residual_analysis = input$show_residual_analysis,
        show_ols_line = TRUE)
    }
  }) # End Regression plots
  
  # Raw data table and summary for rawdata tab in sidebar ----
  raw_data <- reactive({
    if (input$data == 'iris') {
      iris[, 1:4]
    } else if (input$data == 'mtcars') {
      mtcars[, c(1, 3:7)]
    } else if (input$data == 'trees') {
      trees
    }
  }) # End raw_data
  
  # rawdata tab tabels ----
  output$raw_data_table <- renderDataTable(
    expr = raw_data(),
    options = list(searching = FALSE,
                   pageLength = 10)
  )
  
  output$raw_data_summary <- renderDataTable(
    expr = raw_data() %>% 
      mutate(temp = row_number()) %>% 
      pivot_longer(cols = -temp,
                   names_to = "variable",
                   values_to = "value") %>% 
      group_by(variable) %>% 
      summarise(min = round(min(value), 3),
                `1st Quartile` = round(quantile(value, probs = 0.25), 3),
                median = round(quantile(value, probs = 0.5), 3),
                mean = round(mean(value), 3),
                `3rd Quartile` = round(quantile(value, probs = 0.75), 3),
                max = round(max(value), 3)),
    options = list(searching = FALSE,
                   paging = FALSE,
                   pageLength = nrow(raw_data()))
  )
  
  # User regression summaries ----
  user_regression_summary <- reactive(
    {
      user_mse <- sum((plot_data()$data$residual)^2) / (plot_data()$n - 2)
      
      tribble(
        ~Intercept, ~Slope, ~MSE,
        intercept_input(),
        intercept_input(),
        round(user_mse, 3)
      )
  }
  )
  
  output$user_reg_summary <- renderTable(
    expr = user_regression_summary(),
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )
  
  # OLS regression summaries ----
  ols_regression_summary <- reactive(
    {
      ols_mse <- sum(ols_fit()$residuals^2) / ols_fit()$df.residual
    
      tribble(
        ~Intercept, ~Slope, ~MSE,
        round(ols_fit()$coefficients[1], 3), 
        round(ols_fit()$coefficients[2], 3),  
        round(ols_mse, 3)
      )
  }
  )
  
  output$ols_reg_summary <- renderTable(
    expr = ols_regression_summary(),
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )
  
  # Prediction output ----
  output$prediction_y <- renderTable(
    {
      if (input$prediction_x < min(plot_data()$data$x) | 
          input$prediction_x > max(plot_data()$data$x)) {
        
        output$extrapolation <- renderText("Extrapolation is not advised!")
      } else {
        output$extrapolation <- renderText("")
      }
      tibble(`Predicted y-value` = ols_fit()$coefficients[1] + 
               ols_fit()$coefficients[2] * input$prediction_x)
    }
  )
  
  # Developers ----
  output$developers <- renderTable(
    tibble(
      Name = c("Derenik Haghverdian", "Thuy Vu Dieu Lu", "Jeremy Lin"),
      email = c("dhaghver@uci.edu", "lutv@uci.edu", "jeremjl3@uci.edu")
    )
  )
}

# shinyAPP ----------------------------------------------------------------
shinyApp(ui, server)