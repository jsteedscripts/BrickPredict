library(shiny)
library(ggplot2)
library(caret)
library(kableExtra)
library(rpart)
library(rpart.plot)

# load LEGO dataset
dataset <- read.csv('sets.csv')
# filter to relevant columns to use for linear regression
dataset <- dataset[ , c('Pieces', 'USD_MSRP', 'Minifigures', 'Current_Price', 'Rating')]

ui <- fluidPage(
  # display usage info to user
  titlePanel("LEGO Price Prediction Explorer"),
  
  HTML("<div style='text-align: center; font-size: 16px; margin-bottom: 20px;'>
          <p>Welcome, LEGO enthusiast! This is a tool you can use to explore how the addition and removal
       of predictor variables can affect a model's performance. The data is from Kaggle, originally sourced
       from Brickset, the primary reference website for LEGO sets. Choose your predictor variables and model type and see what happens!</p>
          <ul style='list-style-type: disc; text-align: left; margin-left: 40px;'>
            <li>Pieces: Number of individual pieces in the set.</li>
            <li>Minifigures: Number of minifigures included in the set.</li>
            <li>Current_Price: Latest retail price in USD.</li>
            <li>Rating: Buyer rating out of 5.</li>
          </ul>
       </div>"
  ),
  
  sidebarLayout(
    sidebarPanel(
      # choose predictor(s)
      checkboxGroupInput(
        inputId = "predictors",
        label = "Select Predictors",
        choices = names(dataset[, -which(names(dataset) == "USD_MSRP")]),
      ),
      # choose model type
      radioButtons(
        inputId = "modelType",
        label = "Select Model Type",
        choices = c("Linear Regression", "Decision Tree"),
        selected = "Linear Regression"
      ),
      # run model
      actionButton("runModel", "Run Model")
    ),
    mainPanel(
      plotOutput("accuracyPlot"),
      verbatimTextOutput("performanceMetrics"),
      plotOutput("secondPlot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output) {
  
  model_results <- reactiveVal(NULL)
  
  # observeEvents are used to handle user changes to model or predictors
  # clears model state when model or predictors are changed
  # refreshes main panel when user hits "Run Model"
  observeEvent(input$modelType, {
    model_results(NULL)
  })
  
  observeEvent(input$predictors, {
    model_results(NULL)
  })
  
  observeEvent(input$runModel, {
    # construct model formula based on user's desired predictors
    selected_predictors <- input$predictors
    formula <- as.formula(paste("USD_MSRP ~ ", paste(selected_predictors, collapse = " + ")))
    
    if (input$modelType == "Linear Regression") {
      # linear regression
      fitted_model <- lm(formula, data = dataset)
    } else {
      # decision tree
      fitted_model <- rpart(formula, data = dataset, method = "anova")
    }
    
    predictions <- predict(fitted_model, newdata = dataset)
    
    model_summary <- summary(fitted_model)
    
    model_results(list(predictions = predictions, residuals = residuals(fitted_model), summary = model_summary, model = fitted_model))
  })
  
  output$accuracyPlot <- renderPlot({
    # only display plot if user has hit "Run Model"
    if (!is.null(model_results())) {
      if (input$modelType == "Linear Regression") {
        ggplot(dataset, aes(x = USD_MSRP, y = model_results()$predictions)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "blue") +
          labs(title = "Model MSRP Predictions vs Actuals", x = "Actuals", y = "Predictions")
      } else {
        rpart.plot(model_results()$model, main = "Decision Tree Predicting MSRP of LEGO sets")
      }
    }
  })
  
  output$summary <- renderPrint({
    # only display summary if user has hit "Run Model"
    if (!is.null(model_results())) {
      model_results()$summary
    }
  })
  
  output$performanceMetrics <- renderPrint({
    # only display summary if user has hit "Run Model"
    if (!is.null(model_results())) {
      if (input$modelType == "Linear Regression") {
        r_squared <- summary(model_results()$model)$r.squared
        cat("R-Squared:", r_squared, "\n")
      } else {
        mse <- mean(model_results()$residuals^2)
        rmse <- sqrt(mse)
        cat("Mean Squared Error:", mse, "\n")
        cat("Root Mean Squared Error:", rmse, "\n")
      }
    }
  })
  
  output$secondPlot <- renderPlot({
    # only display summary if user has hit "Run Model"
    if (!is.null(model_results()) && input$modelType == "Linear Regression") {
      qqnorm(residuals(model_results()$model), main = "QQ Plot for Residuals")
      qqline(residuals(model_results()$model), col = 2)
    } else if (!is.null(model_results()) && input$modelType == "Decision Tree") {
      importance <- model_results()$model$variable.importance
      importance_df <- data.frame(variable = names(importance), importance = importance)
      ggplot(importance_df, aes(x = reorder(variable, -importance), y = importance)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Decision Tree Feature Importance", x = "Predictors", y = "Importance")
    }
  })
  
}

shinyApp(ui, server)
