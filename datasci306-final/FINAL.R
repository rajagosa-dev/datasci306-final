## ------------------------------------------------------------------
## DATA 306 Final Project
## Research Question:
## "How much does controlling for home characteristics change 
##  the estimated price gradient with distance from downtown Ann Arbor?"
## ------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(scales)   # for dollar_format, dollar
library(viridis)  # for scale_fill_viridis_d

# Load the housing data
load("a2housing.RData")

# Clean data: drop missing key vars, remove zero sale_price
a2housing_clean <- a2housing %>%
  filter(
    !is.na(sale_price),
    !is.na(sale_date),
    !is.na(beds),
    !is.na(full_baths),
    !is.na(half_baths),
    !is.na(sqft),
    !is.na(lat),
    !is.na(long),
    sale_price > 0
  )

cat("Original dataset:", nrow(a2housing), "rows\n")
cat("Cleaned dataset:", nrow(a2housing_clean), "rows\n")
cat("Removed:", nrow(a2housing) - nrow(a2housing_clean), "rows with missing data\n\n")

# Compute distance & direction from a downtown center point -------------------
# Center: approx Liberty/Main (42.2795, -83.7421)
a2housing_clean <- a2housing_clean %>%
  mutate(
    lat_rad = lat * pi / 180,
    long_rad = long * pi / 180,
    center_lat_rad = 42.2795 * pi / 180,
    center_long_rad = -83.7421 * pi / 180,
    
    # Haversine, with numerical guard on acos() argument
    hav_arg = cos(center_lat_rad) * cos(lat_rad) * 
      cos(long_rad - center_long_rad) + 
      sin(center_lat_rad) * sin(lat_rad),
    hav_arg = pmin(pmax(hav_arg, -1), 1),
    dist_center = 3959 * acos(hav_arg),
    
    Direction = (atan2(
      sin(long_rad - center_long_rad) * cos(lat_rad),
      cos(center_lat_rad) * sin(lat_rad) - 
        sin(center_lat_rad) * cos(lat_rad) * cos(long_rad - center_long_rad)
    ) * 180 / pi + 360) %% 360
  ) %>%
  select(-lat_rad, -long_rad, -center_lat_rad, -center_long_rad, -hav_arg)

# Simple vs multiple regression models ----------------------------------------
simple_model <- lm(sale_price ~ dist_center, data = a2housing_clean)

multiple_model <- lm(
  sale_price ~ dist_center + Direction + beds + full_baths + half_baths + sqft,
  data = a2housing_clean
)

cat("=== SIMPLE Linear Regression Model ===\n")
print(summary(simple_model))
cat("\n\n=== MULTIPLE Linear Regression Model ===\n")
print(summary(multiple_model))

# Attach predictions for both models -----------------------------------------
a2housing_clean <- a2housing_clean %>%
  mutate(
    pred_simple   = as.numeric(predict(simple_model)),
    pred_multiple = as.numeric(predict(multiple_model))
  )

# Precompute some summary values for UI text / insight ------------------------
dist_range <- range(a2housing_clean$dist_center, na.rm = TRUE)
mean_vals <- list(
  sqft        = mean(a2housing_clean$sqft,        na.rm = TRUE),
  beds        = mean(a2housing_clean$beds,        na.rm = TRUE),
  full_baths  = mean(a2housing_clean$full_baths,  na.rm = TRUE),
  half_baths  = mean(a2housing_clean$half_baths,  na.rm = TRUE)
)

beta_simple  <- coef(simple_model)["dist_center"]
beta_multiple <- coef(multiple_model)["dist_center"]

# Define the user interface ---------------------------------------------------
ui <- navbarPage(
  title = "The Real Price of a Mile",
  windowTitle = "The Real Price of a Mile",
  
  header = tags$head(
    tags$style(HTML("
      .navbar-default {
        background-color: #000000 !important;
        border: none !important;
      }
      .navbar-default .navbar-brand {
        color: white !important;
        font-size: 24px !important;
        font-weight: bold !important;
      }
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-size: 16px !important;
      }
      .navbar-default .navbar-nav > li > a:hover {
        background-color: rgba(255, 255, 255, 0.2) !important;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: rgba(255, 255, 255, 0.3) !important;
        color: white !important;
      }
    "))
  ),
  
  # Tab 1: The Illusion -------------------------------------------------------
  tabPanel("The Illusion",
           p(strong("Research Question:"),
             " How much does controlling for home characteristics change the estimated price gradient with distance from downtown Ann Arbor?"),
           p(strong("Insight:"),
             " Distance alone creates the illusion of predicting price. The realistic model shows how characteristics change the true value."),
           br(),
           fluidRow(
             column(5,
                    div(style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px; height: 100%;",
                        h3("Home Characteristics"),
                        br(),
                        sliderInput(
                          "beds_input", "Number of Bedrooms:",
                          min = 1, max = 10, value = 3, step = 1
                        ),
                        sliderInput(
                          "full_baths_input", "Number of Full Bathrooms:",
                          min = 1, max = 8, value = 2, step = 1
                        ),
                        sliderInput(
                          "half_baths_input", "Number of Half Bathrooms:",
                          min = 0, max = 4, value = 0, step = 1
                        ),
                        sliderInput(
                          "dist_input", "Distance from City Center (miles):",
                          min = round(dist_range[1], 1),
                          max = round(dist_range[2], 1),
                          value = round(mean(a2housing_clean$dist_center), 1),
                          step = 0.1
                        ),
                        h5("Direction from City Center:"),
                        fluidRow(
                          column(6,
                                 radioButtons(
                                   "direction_ns",
                                   label = NULL,
                                   choices = c("North" = 0, "South" = 180),
                                   selected = 0
                                 )
                          ),
                          column(6,
                                 radioButtons(
                                   "direction_ew",
                                   label = NULL,
                                   choices = c("East" = 90, "West" = 270),
                                   selected = 90
                                 )
                          )
                        ),
                        numericInput(
                          "sqft_input", "Square Footage:",
                          value = round(mean_vals$sqft),
                          min = 500, max = 10000, step = 100
                        ),
                        br(),
                        actionButton(
                          "predict_button",
                          "Calculate Realistic Price",
                          class = "btn-primary btn-lg",
                          style = "width: 100%;"
                        )
                    )
             ),
             column(7,
                    h3("How much is a home really worth?"),
                    p("Compare predictions from the simple model (distance only) vs multiple regression model (distance + characteristics)."),
                    br(),
                    fluidRow(
                      column(6,
                             div(style = "background-color: #e3f2fd; padding: 20px; border-radius: 8px; text-align: center;",
                                 h4("The Embellished Price", style = "color: #1976d2;"),
                                 p("(Distance Only)"),
                                 h2(textOutput("simple_prediction"),
                                    style = "color: #1976d2; margin: 20px 0;")
                             )
                      ),
                      column(6,
                             div(style = "background-color: #ffebee; padding: 20px; border-radius: 8px; text-align: center;",
                                 h4("The Realistic Price", style = "color: #c62828;"),
                                 p("(Distance + Characteristics)"),
                                 h2(textOutput("multiple_prediction"),
                                    style = "color: #c62828; margin: 20px 0;")
                             )
                      )
                    ),
                    br(),
                    conditionalPanel(
                      condition = "input.predict_button > 0",
                      div(style = "background-color: #f5f5f5; padding: 20px; border-radius: 8px; margin-top: 20px;",
                          h4("Model Comparison"),
                          htmlOutput("model_comparison")
                      )
                    )
             )
           )
  ),
  
  # Tab 2: What Changed -------------------------------------------------------
  tabPanel("What Changed",
           h3("How the Distance Coefficient Changes as We Control for More Features"),
           p("Select which variables to add to the regression and see how the coefficient on distance (the price gradient per mile) changes."),
           br(),
           fluidRow(
             column(3,
                    div(style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
                        h4("Select Variables to Include:"),
                        checkboxInput("include_direction",   "Direction",       value = FALSE),
                        checkboxInput("include_beds",        "Bedrooms",        value = FALSE),
                        checkboxInput("include_full_baths",  "Full Bathrooms",  value = FALSE),
                        checkboxInput("include_half_baths",  "Half Bathrooms",  value = FALSE),
                        checkboxInput("include_sqft",        "Square Footage",  value = FALSE)
                    )
             ),
             column(9,
                    plotOutput("coefficient_change_plot", height = "400px"),
                    br(),
                    div(style = "background-color: #fff3cd; padding: 20px; border-radius: 8px;",
                        h4("Change in Distance Coefficient:"),
                        htmlOutput("distance_coefficient_change")
                    )
             )
           )
  ),
  
  # Tab 3: Error Patterns -----------------------------------------------------
  tabPanel("Error Patterns",
           p(strong("Insight:"),
             " The simple model systematically misprices certain types of homes. These patterns reveal weaknesses in using distance alone."),
           br(),
           h3("How Prediction Errors Vary by Home Characteristics"),
           p("Prediction Error = Actual Price − Simple Model Prediction"),
           br(),
           fluidRow(
             column(6,
                    h4("Prediction Error by Number of Bedrooms"),
                    plotOutput("error_by_beds", height = "400px")
             ),
             column(6,
                    h4("Prediction Error by Square Footage"),
                    plotOutput("error_by_sqft", height = "400px")
             )
           ),
           br(),
           fluidRow(
             column(6,
                    h4("Prediction Error by Direction from Center"),
                    plotOutput("error_by_direction", height = "400px")
             ),
             column(6,
                    div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 50px;",
                        h4("Understanding the Patterns"),
                        p("Positive errors (above 0): the simple model ",
                          strong("underestimates"), " the price."),
                        p("Negative errors (below 0): the simple model ",
                          strong("overestimates"), " the price."),
                        br(),
                        p("These patterns reveal which types of homes are systematically mispriced when only considering distance from the city center.")
                    )
             )
           )
  ),
  
  # Tab 4: Model Details ------------------------------------------------------
  navbarMenu("Model Details",
             tabPanel("Predicting Price",
                      p(strong("Insight:"),
                        " The red line is flatter/steeper because the multiple model holds other features constant, isolating distance’s partial effect."),
                      br(),
                      h3("Price vs Distance Visualization"),
                      fluidRow(
                        column(9,
                               p(paste("Showing", nrow(a2housing_clean), "properties with complete data (capped at $1,000,000)."))
                        ),
                        column(3,
                               div(style = "text-align: right;",
                                   div(style = "display: inline-block; background-color: #2196F3; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px;",
                                       "Simple Model"),
                                   br(),
                                   div(style = "display: inline-block; background-color: #FF5722; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px;",
                                       "Multiple Model")
                               )
                        )
                      ),
                      plotOutput("scatterPlot", height = "600px"),
                      br(), hr(), br(),
                      h3("Model Coefficients Comparison"),
                      h4("Simple Linear Regression Model"),
                      p("Predicting sale price based only on distance from downtown Ann Arbor."),
                      tableOutput("simpleCoefTable"),
                      br(),
                      h4("Multiple Linear Regression Model"),
                      p("Predicting a home's sale price based on distance, direction, number of beds, full bathrooms, half bathrooms, and square footage."),
                      p(em("Direction is measured in degrees (0–360°), where 0° = North, 90° = East, 180° = South, and 270° = West.")),
                      tableOutput("multipleCoefTable")
             ),
             
             tabPanel("Model Performance",
                      h3("Actual vs Predicted Sale Price"),
                      p("Compare how well each model predicts sale prices."),
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Model Accuracy Comparison"),
                                   uiOutput("model_mae_comparison")
                               )
                        )
                      ),
                      plotOutput("performancePlot", height = "600px"),
                      br(), hr(), br(),
                      h3("Residuals vs Fitted Values"),
                      p("Residuals help diagnose whether we are missing important patterns in the data."),
                      br(),
                      fluidRow(
                        column(6,
                               h4("Simple Model", style = "color: #1976d2; text-align: center;"),
                               plotOutput("residuals_simple", height = "500px")
                        ),
                        column(6,
                               h4("Multiple Model", style = "color: #c62828; text-align: center;"),
                               plotOutput("residuals_multiple", height = "500px")
                        )
                      )
             )
  )
)

# Server logic ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Scatter: price vs distance with both lines --------------------------------
  output$scatterPlot <- renderPlot({
    data_filtered <- a2housing_clean %>%
      filter(sale_price <= 1000000)
    
    ggplot(data_filtered, aes(x = dist_center, y = sale_price)) +
      geom_point(alpha = 0.2, color = "gray", size = 0.8) +
      geom_smooth(
        method = "lm", formula = y ~ x, se = FALSE,
        color = "blue", linewidth = 1.5
      ) +
      geom_smooth(
        aes(y = pred_multiple),
        method = "lm", formula = y ~ x, se = FALSE,
        color = "red", linewidth = 1.5
      ) +
      labs(
        x = "Distance from Center (miles)",
        y = "Sale Price ($)",
        title = "Sale Price vs Distance from Center (Homes ≤ $1,000,000)",
        subtitle = "Blue = Simple Regression | Red = Multiple Regression (Distance effect after controlling for characteristics)"
      ) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12)
      )
  })
  
  # Coefficient tables ---------------------------------------------------------
  output$simpleCoefTable <- renderTable({
    coef_summary <- summary(simple_model)$coefficients
    coef_df <- as.data.frame(coef_summary)
    coef_df <- cbind(Variable = rownames(coef_df), coef_df)
    rownames(coef_df) <- NULL
    colnames(coef_df) <- c("Variable", "Estimate", "Std. Error", "t value", "p-value")
    coef_df$`p-value` <- format(coef_df$`p-value`, scientific = TRUE, digits = 4)
    coef_df
  }, digits = 4)
  
  output$multipleCoefTable <- renderTable({
    coef_summary <- summary(multiple_model)$coefficients
    coef_df <- as.data.frame(coef_summary)
    coef_df <- cbind(Variable = rownames(coef_df), coef_df)
    rownames(coef_df) <- NULL
    colnames(coef_df) <- c("Variable", "Estimate", "Std. Error", "t value", "p-value")
    coef_df$`p-value` <- format(coef_df$`p-value`, scientific = TRUE, digits = 4)
    coef_df
  }, digits = 4)
  
  # Model performance: actual vs predicted ------------------------------------
  output$performancePlot <- renderPlot({
    performance_data <- data.frame(
      actual        = a2housing_clean$sale_price,
      simple_pred   = a2housing_clean$pred_simple,
      multiple_pred = a2housing_clean$pred_multiple
    )
    
    performance_long <- performance_data %>%
      pivot_longer(
        cols = c(simple_pred, multiple_pred),
        names_to = "model",
        values_to = "predicted"
      ) %>%
      mutate(
        model = case_when(
          model == "simple_pred"   ~ "Simple Model",
          model == "multiple_pred" ~ "Multiple Model"
        )
      ) %>%
      filter(actual <= 2000000, predicted <= 2000000)
    
    ggplot(performance_long, aes(x = actual, y = predicted, color = model)) +
      geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
      annotate("text", x = 1600000, y = 1700000,
               label = "Perfect Prediction", angle = 45, color = "black", size = 4) +
      geom_point(alpha = 0.15, size = 0.5) +
      scale_color_manual(values = c("Simple Model" = "blue", "Multiple Model" = "red")) +
      labs(
        x = "Actual Sale Price ($)",
        y = "Predicted Sale Price ($)",
        title = "Model Performance: Actual vs Predicted Prices (≤ $2M)",
        color = "Model"
      ) +
      scale_x_continuous(labels = dollar_format(), limits = c(0, 2000000)) +
      scale_y_continuous(labels = dollar_format(), limits = c(0, 2000000)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "top"
      )
  })
  
  # EventReactive predictions --------------------------------------------------
  simple_prediction <- eventReactive(input$predict_button, {
    new_data <- data.frame(dist_center = input$dist_input)
    predicted_price <- predict(simple_model, newdata = new_data)
    dollar(predicted_price)
  })
  
  multiple_prediction <- eventReactive(input$predict_button, {
    ns_value <- as.numeric(input$direction_ns)
    ew_value <- as.numeric(input$direction_ew)
    direction_angle <- (ns_value + ew_value) / 2
    
    new_data <- data.frame(
      dist_center = input$dist_input,
      Direction   = direction_angle,
      beds        = input$beds_input,
      full_baths  = input$full_baths_input,
      half_baths  = input$half_baths_input,
      sqft        = input$sqft_input
    )
    predicted_price <- predict(multiple_model, newdata = new_data)
    dollar(predicted_price)
  })
  
  output$simple_prediction <- renderText(simple_prediction())
  output$multiple_prediction <- renderText(multiple_prediction())
  
  # Model comparison text for a specific home ---------------------------------
  output$model_comparison <- renderUI({
    # simple prediction
    new_data_simple <- data.frame(dist_center = input$dist_input)
    pred_simple <- as.numeric(predict(simple_model, newdata = new_data_simple))
    
    # multiple prediction
    ns_value <- as.numeric(input$direction_ns)
    ew_value <- as.numeric(input$direction_ew)
    direction_angle <- (ns_value + ew_value) / 2
    
    new_data_multiple <- data.frame(
      dist_center = input$dist_input,
      Direction   = direction_angle,
      beds        = input$beds_input,
      full_baths  = input$full_baths_input,
      half_baths  = input$half_baths_input,
      sqft        = input$sqft_input
    )
    pred_multiple <- as.numeric(predict(multiple_model, newdata = new_data_multiple))
    
    difference <- pred_simple - pred_multiple
    
    avg_sqft       <- mean_vals$sqft
    avg_beds       <- mean_vals$beds
    avg_full_baths <- mean_vals$full_baths
    avg_half_baths <- mean_vals$half_baths
    
    diff_sqft       <- input$sqft_input       - avg_sqft
    diff_beds       <- input$beds_input       - avg_beds
    diff_full_baths <- input$full_baths_input - avg_full_baths
    diff_half_baths <- input$half_baths_input - avg_half_baths
    
    direction_label <- paste0(
      ifelse(ns_value == 0, "North", "South"),
      ifelse(ew_value == 90, "east", "west")
    )
    
    if (difference > 0) {
      comparison_text <- paste0(
        "<p style='font-size: 16px;'>A model that only accounts for distance ",
        "<strong>overestimates</strong> this home's price by ",
        "<strong style='color: #d32f2f;'>", dollar(abs(difference)), "</strong> ",
        "relative to a model that also includes home characteristics.</p>",
        "<p style='font-size: 14px; color: #666;'>For this home, distance alone ",
        "makes the gradient too strong and ignores how features change the value.</p>"
      )
    } else if (difference < 0) {
      comparison_text <- paste0(
        "<p style='font-size: 16px;'>A model that only accounts for distance ",
        "<strong>underestimates</strong> this home's price by ",
        "<strong style='color: #d32f2f;'>", dollar(abs(difference)), "</strong> ",
        "relative to a model that also includes home characteristics.</p>",
        "<p style='font-size: 14px; color: #666;'>For this home, distance alone ",
        "misses value added by size, bathrooms, and location direction.</p>"
      )
    } else {
      comparison_text <- paste0(
        "<p style='font-size: 16px;'>Both models predict the ",
        "<strong>same price</strong> for this home.</p>"
      )
    }
    
    comparison_text <- paste0(
      comparison_text,
      "<p style='font-size: 14px; color: #333;'><strong>Compared to the average home at this distance, your home is:</strong><br>",
      "• Located <strong>", direction_label, "</strong> of city center<br>",
      "• <strong>", format(round(abs(diff_sqft)), big.mark = ","), " sqft ",
      ifelse(diff_sqft >= 0, "larger", "smaller"), "</strong><br>",
      "• Has <strong>", round(abs(diff_beds), 1),
      ifelse(diff_beds >= 0, " more", " fewer"), " bedrooms</strong><br>",
      "• Has <strong>", round(abs(diff_full_baths), 1),
      ifelse(diff_full_baths >= 0, " more", " fewer"), " full bathrooms</strong><br>",
      "• Has <strong>", round(abs(diff_half_baths), 1),
      ifelse(diff_half_baths >= 0, " more", " fewer"), " half bathrooms</strong></p>"
    )
    
    HTML(comparison_text)
  })
  
  # MAE comparison -------------------------------------------------------------
  output$model_mae_comparison <- renderUI({
    mae_simple   <- mean(abs(a2housing_clean$sale_price - a2housing_clean$pred_simple))
    mae_multiple <- mean(abs(a2housing_clean$sale_price - a2housing_clean$pred_multiple))
    improvement  <- ((mae_simple - mae_multiple) / mae_simple) * 100
    
    HTML(paste0(
      "<p style='font-size: 16px;'>",
      "<strong>Simple Model:</strong> Average prediction error = ",
      dollar(mae_simple), "<br>",
      "<strong>Multiple Model:</strong> Average prediction error = ",
      dollar(mae_multiple), "<br><br>",
      "<strong>Improvement:</strong> Including home characteristics reduces prediction error by ",
      "<span style='color: #28a745; font-weight: bold;'>",
      round(improvement, 1), "%</span> compared to using distance alone.",
      "</p>",
      "<p style='font-size: 14px; color: #777;'>",
      "This directly answers our research question: controlling for characteristics ",
      "changes both the distance gradient and overall predictive accuracy.",
      "</p>"
    ))
  })
  
  # Dynamic regression for "What Changed" tab ---------------------------------
  dynamic_model <- reactive({
    formula_vars <- "dist_center"
    if (input$include_direction)   formula_vars <- c(formula_vars, "Direction")
    if (input$include_beds)        formula_vars <- c(formula_vars, "beds")
    if (input$include_full_baths)  formula_vars <- c(formula_vars, "full_baths")
    if (input$include_half_baths)  formula_vars <- c(formula_vars, "half_baths")
    if (input$include_sqft)        formula_vars <- c(formula_vars, "sqft")
    
    formula_str <- paste("sale_price ~", paste(formula_vars, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    lm(formula_obj, data = a2housing_clean)
  })
  
  output$distance_coefficient_change <- renderUI({
    model <- dynamic_model()
    
    dist_only_coef <- beta_simple
    current_coef   <- coef(model)["dist_center"]
    
    change <- current_coef - dist_only_coef
    percent_change <- (change / abs(dist_only_coef)) * 100
    
    num_vars <- sum(c(
      input$include_direction,
      input$include_beds,
      input$include_full_baths,
      input$include_half_baths,
      input$include_sqft
    ))
    
    if (num_vars == 0) {
      HTML("<p style='font-size: 16px; text-align: center;'>No additional variables selected. Select variables to see how the distance coefficient (price gradient) changes.</p>")
    } else {
      HTML(paste0(
        "<p style='font-size: 18px;'><strong>The distance coefficient changed by: </strong>",
        "<span style='color: ", ifelse(change > 0, "#388e3c", "#d32f2f"),
        "; font-size: 24px; font-weight: bold;'>",
        ifelse(change > 0, "+", ""), dollar(round(change, 2)),
        " (", ifelse(change > 0, "+", ""), round(percent_change, 1), "%)",
        "</span></p>",
        "<p style='font-size: 14px; color: #666;'>When adding <strong>", num_vars,
        " variable(s)</strong>, the distance coefficient went from <strong>",
        dollar(round(dist_only_coef, 2)), "</strong> to <strong>",
        dollar(round(current_coef, 2)), "</strong> per mile.",
        " This is how much controlling for home characteristics changes the estimated price gradient.</p>"
      ))
    }
  })
  
  output$coefficient_change_plot <- renderPlot({
    dist_only_coef <- beta_simple
    current_coef   <- coef(dynamic_model())["dist_center"]
    
    coef_data <- data.frame(
      Model       = factor(c("Distance Only", "With Selected Variables"),
                           levels = c("Distance Only", "With Selected Variables")),
      Coefficient = c(dist_only_coef, current_coef)
    )
    
    ggplot(coef_data, aes(x = Model, y = Coefficient, fill = Model)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("Distance Only" = "#2196F3", 
                                   "With Selected Variables" = "#FF5722")) +
      labs(
        x = "",
        y = "Distance Coefficient ($ per mile)",
        title = "How the Distance Coefficient Changes When Adding Variables"
      ) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(
        plot.title  = element_text(size = 14, face = "bold"),
        axis.title  = element_text(size = 12),
        axis.text.x = element_text(size = 12, face = "bold"),
        legend.position = "none"
      ) +
      geom_text(
        aes(label = dollar(round(Coefficient, 0))),
        vjust = ifelse(coef_data$Coefficient < 0, 1.5, -0.5),
        size = 5, fontface = "bold"
      )
  })
  
  # Error Patterns: by Bedrooms -----------------------------------------------
  output$error_by_beds <- renderPlot({
    error_data <- a2housing_clean %>%
      mutate(
        error      = sale_price - pred_simple,
        beds_factor = factor(beds)
      )
    
    ggplot(error_data, aes(x = beds_factor, y = error, fill = beds_factor)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_viridis_d() +
      labs(
        x = "Number of Bedrooms",
        y = "Prediction Error ($)",
        title = "Simple Model Errors by Bedroom Count"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 13, face = "bold")
      )
  })
  
  # Error Patterns: by Square Footage -----------------------------------------
  output$error_by_sqft <- renderPlot({
    error_data <- a2housing_clean %>%
      mutate(
        error   = sale_price - pred_simple,
        sqft_bin = cut(
          sqft,
          breaks = c(0, 1000, 1500, 2000, 2500, 3000, Inf),
          labels = c("<1000", "1000–1500", "1500–2000", "2000–2500", "2500–3000", "3000+")
        )
      )
    
    ggplot(error_data, aes(x = sqft_bin, y = error, fill = sqft_bin)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_viridis_d() +
      labs(
        x = "Square Footage Range",
        y = "Prediction Error ($)",
        title = "Simple Model Errors by Home Size"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Error Patterns: by Direction ----------------------------------------------
  output$error_by_direction <- renderPlot({
    error_data <- a2housing_clean %>%
      mutate(
        error = sale_price - pred_simple,
        direction_bin = cut(
          Direction,
          breaks = c(0, 45, 135, 225, 315, 360),
          labels = c("North", "East", "South", "West", "North"),
          include.lowest = TRUE
        )
      ) %>%
      mutate(direction_bin = factor(direction_bin, levels = c("North", "East", "South", "West")))
    
    ggplot(error_data, aes(x = direction_bin, y = error, fill = direction_bin)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_manual(values = c(
        "North" = "#4CAF50", "East" = "#2196F3",
        "South" = "#FF9800", "West" = "#9C27B0"
      )) +
      labs(
        x = "Direction from Center",
        y = "Prediction Error ($)",
        title = "Simple Model Errors by Direction"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 13, face = "bold")
      )
  })
  
  # Residuals vs Fitted: Simple Model -----------------------------------------
  output$residuals_simple <- renderPlot({
    residuals_simple  <- residuals(simple_model)
    fitted_simple     <- fitted(simple_model)
    residuals_multiple <- residuals(multiple_model)
    fitted_multiple   <- fitted(multiple_model)
    
    x_min <- min(c(fitted_simple, fitted_multiple))
    x_max <- max(c(fitted_simple, fitted_multiple))
    y_min <- min(c(residuals_simple, residuals_multiple))
    y_max <- max(c(residuals_simple, residuals_multiple))
    
    plot_data <- data.frame(
      Fitted    = fitted_simple,
      Residuals = residuals_simple
    )
    
    ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
      geom_point(alpha = 0.3, color = "#1976d2", size = 1) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      geom_smooth(se = FALSE, color = "orange", linewidth = 1) +
      labs(
        x = "Fitted Values ($)",
        y = "Residuals ($)",
        title = "Simple Model: Residuals vs Fitted"
      ) +
      scale_x_continuous(labels = dollar_format(), limits = c(x_min, x_max)) +
      scale_y_continuous(labels = dollar_format(), limits = c(y_min, y_max)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11)
      )
  })
  
  # Residuals vs Fitted: Multiple Model ---------------------------------------
  output$residuals_multiple <- renderPlot({
    residuals_simple   <- residuals(simple_model)
    fitted_simple      <- fitted(simple_model)
    residuals_multiple <- residuals(multiple_model)
    fitted_multiple    <- fitted(multiple_model)
    
    x_min <- min(c(fitted_simple, fitted_multiple))
    x_max <- max(c(fitted_simple, fitted_multiple))
    y_min <- min(c(residuals_simple, residuals_multiple))
    y_max <- max(c(residuals_simple, residuals_multiple))
    
    plot_data <- data.frame(
      Fitted    = fitted_multiple,
      Residuals = residuals_multiple
    )
    
    ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
      geom_point(alpha = 0.3, color = "#c62828", size = 1) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      geom_smooth(se = FALSE, color = "orange", linewidth = 1) +
      labs(
        x = "Fitted Values ($)",
        y = "Residuals ($)",
        title = "Multiple Model: Residuals vs Fitted"
      ) +
      scale_x_continuous(labels = dollar_format(), limits = c(x_min, x_max)) +
      scale_y_continuous(labels = dollar_format(), limits = c(y_min, y_max)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11)
      )
  })
}

# Run the Shiny app -----------------------------------------------------------
shinyApp(ui, server)
