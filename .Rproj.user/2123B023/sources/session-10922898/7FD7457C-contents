
# Libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(BayesFactor)
theme_set(theme_minimal())

source("plot_density_function.R")

ui <- fluidPage(
  # Use shinyjs to hide the default Shiny progress bar
  useShinyjs(),
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  div(class = "header", style = "display: flex; align-items: center;",
      # img(src = "ZZ-logo_Z-only.png", height = "90px", style = "margin-right: 10px;"),
      div(class = "titlePanel", 
          h1("AB Test Evaluation", style = "margin: 0; color: #333232; font-size: 36px;")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Is your test result significant? Does it have enough power?", style = "font-size: 20px"),
      h5("Play with the controls and get a better feel for how a lower confidence level will boost the power or how an increase in test size can make a small conversion rate difference significant."),
      br(),
      br(),
      # Use fluidRow and columns to position inputs side by side
      h4("Test data", style = "border-bottom: 1px solid #cccccc; padding-bottom: 5px; font-size: 14px"),
      fluidRow(
        column(6, numericInput("visitorsA", "Visitors A", value = 10000,
                               label = span("Visitors A", style = "color: #545454; font-size: 12px;"))),
        column(6, numericInput("conversionsA", "Conversions A", value = 1110,
                               label = span("Conversions A", style = "color: #545454; font-size: 12px;")))
      ),
      fluidRow(
        column(6, numericInput("visitorsB", "Visitors B", value = 10000,
                               label = span("Visitors B", style = "color: #545454; font-size: 12px;"))),
        column(6, numericInput("conversionsB", "Conversions B", value = 1200,
                               label = span("Conversions B", style = "color: #545454; font-size: 12px;")))
      ),
      br(),
      
      actionButton("applyBtn", "Apply Changes", class = "btn-primary"),
      
      br(),
      br(),
      h4("Settings", style = "border-bottom: 1px solid #cccccc; padding-bottom: 5px; font-size: 14px"),
      # Radio buttons for One-sided or Two-sided
      radioButtons("testType", HTML('<span style="color: #545454; font-size: 12px;">Hypothesis</span>'),
                   choices = c("One-sided", "Two-sided"),
                   selected = "One-sided"),
      
      # Radio buttons for Confidence Level
      radioButtons("confidenceLevel", HTML('<span style="color: #545454; font-size: 12px;">Confidence Level</span>'),
                   choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                   selected = 0.95)
    ),
    
    mainPanel(
      h4("Statistics", style = "border-bottom: 1px solid #cccccc; font-size: 14px; padding-bottom: 4px"),# Adjust the padding as needed
      fluidRow(
        column(2, valueBoxOutput("upliftBox"), offset = 1),
        column(2, valueBoxOutput("rateABox")),  # Added for rate A
        column(2, valueBoxOutput("rateBBox")),  # Added for rate B
        column(2, valueBoxOutput("zScoreBox")),
        column(2, valueBoxOutput("pValueBox"))      
        ),
      h6(style = "border-bottom: 1px solid #cccccc; padding-bottom: 4px"),# Adjust the padding as needed
      fluidRow(
        column(6, offset = 3, plotOutput("conversionComparisonPlot", height ="250px"))
      ),
      h4("The expected distributions of variation A and B", style = "border-bottom: 1px solid #cccccc; font-size: 14px; padding-bottom: 4px"),# Adjust the padding as needed
      plotOutput("densityComparisonPlot") # Adjust the padding as needed
      
    )
    
  )
)

# Server Logic
server <- function(input, output) {
  
  # Observer to handle input validation and UI feedback
  observe({
    visitors    <- c(A = input$visitorsA, B = input$visitorsB)
    conversions <- c(A = input$conversionsA, B = input$conversionsB)
    
    if (!checkInputs(visitors, conversions)) {
      shinyjs::html("errorMessage", "Please enter valid numeric values for visitors and conversions.")
    } else {
      shinyjs::html("errorMessage", "")
    }
  })
  
  test_type <- eventReactive(input$applyBtn,{
    return(input$testType)
  }, ignoreNULL = FALSE)
  
  
  # Reactive expression for conf level
  confidence_level_reactive <- eventReactive(input$applyBtn,{
    # Mapping selected confidence level to its z-score
    conf_level <- as.numeric(input$confidenceLevel)
    z_score <- qnorm((1 + conf_level) / 2)  # Calculating the z-score for the given confidence level
    return(z_score)
  }, ignoreNULL = FALSE)
  

  # Reactive expression for rates
  rates_reactive <- eventReactive(input$applyBtn,{
    
    visitors    <- c(A = input$visitorsA, B = input$visitorsB)
    conversions <- c(A = input$conversionsA, B = input$conversionsB)
    
    if(any(is.null(visitors), is.null(conversions), visitors < 0, conversions < 0, conversions > visitors, is.na(visitors), is.na(conversions))) {
      return(NULL)  # Return NULL if inputs are not valid, missing, or NA
    }
    
    rates       <- conversions / visitors
    return(rates)
  }, ignoreNULL = FALSE)
  
  # Reactive expression for data
  data_reactive <- eventReactive(input$applyBtn,{
  
    
    visitors    <- c(A = input$visitorsA,    B = input$visitorsB)
    conversions <- c(A = input$conversionsA, B = input$conversionsB)
    
    if(any(is.null(visitors), is.null(conversions), visitors < 0, conversions < 0, conversions > visitors)) {
      return(NULL)  # Return NULL if inputs are not valid or missing
    }
    
    # Posterior Distributions
    prior <- c(1, 1)
    post  <- rbind(
      c(conversions["A"] + prior[1], visitors["A"] - conversions["A"] + prior[2]),
      c(conversions["B"] + prior[1], visitors["B"] - conversions["B"] + prior[2])
    )
    
    # Simulations
    set.seed(123)
    simulations <- list(
      A = rbeta(100000, post[1, 1], post[1, 2]),
      B = rbeta(100000, post[2, 1], post[2, 2])
    )
    
    # Statistics (optional, used if you want to display them somewhere in the app)
    prob_B_better_than_A <- mean(simulations$B > simulations$A)
    rates                <- conversions / visitors
    overall_rate         <- sum(conversions) / sum(visitors)
    standard_errors      <- sqrt(rates * (1 - rates) / visitors)
    
    # Z-Score
    # z_score <- (rates["B"] - rates["A"]) / sqrt(sum(standard_errors^2))
    relative_uplift_conversion_rate <- (rates["B"] - rates["A"]) / rates["A"]
    
    z_score <- confidence_level_reactive()
    
    # Compare conversion rates with error bars
    comparison_df <- tibble(
      type          = names(rates),
      rate          = rates,
      std_err       = standard_errors,
      conf_lower    = rates - z_score * standard_errors,
      conf_upper    = rates + z_score * standard_errors
    )
    z_score <- (rates["B"] - rates["A"]) / sqrt(sum(standard_errors^2))
    
    return(
      list(
        comparison_df   = comparison_df,
        relative_uplift = relative_uplift_conversion_rate,
        z_score         = z_score,
        simulations     = simulations
        )
      )  # Return the DataFrame for plotting
  }, ignoreNULL = FALSE)
  
  # Plot Output
  output$conversionComparisonPlot <- renderPlot({
    comparison_df <- data_reactive()$comparison_df  # Fetching the reactive data
    if (is.null(comparison_df)) {
      return(ggplot() + 
               labs(x = NULL, y = NULL) +
               theme(panel.grid = element_blank(),
                     axis.text  = element_blank()) +
               annotate("text", x = 1, y = 1, 
                        label = "Invalid input. Please check your data.\nConversions must be less than visitors", 
                        size = 6, col = "#D81B60"))
    }
    
    if(is.null(comparison_df)) {
      return()  # Do not render the plot if data is NULL
    }
    
    # Ensure 'type' is a factor and handle NA values
    comparison_df$type <- factor(comparison_df$type, levels = unique(comparison_df$type))
    comparison_df <- na.omit(comparison_df)  # Remove rows with NA values
    
    # ggplot code
    ggplot(comparison_df, aes(x = fct_reorder(type, rate, .na_rm = TRUE), y = rate, ymin = conf_lower, ymax = conf_upper, color = type)) +
      geom_crossbar(position = position_dodge(0.5), width = 0.32, linewidth = 0.5) +
      geom_text(aes(label = scales::percent(rate, accuracy = 0.01)), nudge_x =  0.25, size = 5, check_overlap = TRUE) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = c("A" = "#2E465F", "B" = "#D81B60")) +
      labs(
        title = NULL,
        x     = "", 
        y     = NULL
      ) +
      coord_flip() +
      theme(
        legend.position = "none",
        panel.grid      = element_blank(),
        axis.text.x     = element_blank(),
        axis.text       = element_text(color = "#545454", size = 11, face = "bold")
      )
  }, width = 500)
  
  output$densityComparisonPlot <- renderPlot({
    
    # Ensure the simulations data is available
    simulations <- data_reactive()$simulations  # Replace with your actual reactive expression for simulations
    rates       <- rates_reactive()  # Replace with your actual reactive expression for rates
    
    if(is.null(simulations) || is.null(rates) || any(sapply(simulations, is.null)) || any(is.na(rates))) {
      return(ggplot() + 
               labs(x = NULL, y = NULL) +
               theme(panel.grid = element_blank(),
                     axis.text  = element_blank()) +
               annotate("text", x = 1, y = 1, 
                        label = "Invalid input. Please check your data.\nConversions must be less than visitors", 
                        size = 6, col = "#D81B60")) 
    }
  
    # Define group names and colors (adjust as needed)
    group_names <- c("A", "B")
    group_colors <- c("A" = "#2E465F", "B" = "#D81B60")
    
    # Call the plot_density_comparison function
    plot_density_comparison(simulations, rates, group_names, group_colors)
  }, height = 360)
  
  relative_uplift_reactive <- eventReactive(input$applyBtn, {
    data <- data_reactive()
    if (is.null(data) || is.null(data$relative_uplift)) {
      return(NULL)
    }
    return(data$relative_uplift)
  }, ignoreNULL = FALSE)
  
  
  output$upliftBox <- renderValueBox({
    uplift <- relative_uplift_reactive()
    if (is.null(uplift)) {
      valueBox("N/A", "CR Uplift", icon = NULL)
    } else {
      valueBox(scales::percent(uplift, accuracy = 0.01), "Uplift", icon = NULL)
    }
  })
  
  # Reactive expression for z-score
  z_score_reactive <- eventReactive(input$applyBtn, {
    data <- data_reactive()
    if (is.null(data)) {
      return(NULL)
    }
    return(data$z_score)  # Assuming z_score is calculated and stored in data
  }, ignoreNULL = FALSE)
  
  # Output of z-score
  output$zScoreBox <- renderValueBox({
    z_score <- z_score_reactive()
    if (is.null(z_score)) {
      valueBox("N/A", "Z-Score", width = NULL)
    } else {
      valueBox(round(z_score, 4), "zScore")
    }
  })
  
  
  # Output of p-value
  output$pValueBox <- renderValueBox({
    z_score <- z_score_reactive() # Ensure this reactive function is defined elsewhere in your Shiny app
    
    # Validate z_score
    if (is.na(z_score) || !is.numeric(z_score) || length(z_score) == 0) {
      return(valueBox("N/A", "p-value"))
    }
    
    # Calculate p-value from z-score using hypothesis test input
    if (test_type() == "Two-sided") {
      p_value <- 2 * pnorm(-abs(z_score)) # Use abs() to handle both positive and negative z-scores
    } else if (test_type() == "One-sided") {
      if (z_score < 0) {
        p_value <- pnorm(z_score, lower.tail = FALSE) # For negative z-scores, no need for lower.tail = FALSE
      } else {
        p_value <- pnorm(z_score, lower.tail = FALSE) # For positive z-scores in a one-sided test
      }
    } else {
      p_value <- NULL
    }
    
    # Display the p-value in a value box
    if (is.null(p_value)) {
      valueBox("N/A", "p-value")
    } else {
      valueBox(round(p_value, 4), "pValue")
    }
  })
  
  # Reactive expression for rate A
  rateA_reactive <- eventReactive(input$applyBtn, {
    data <- data_reactive()
    if(is.null(data)) {
      return(NULL)
    }
    return(data$comparison_df %>% filter(type == "A") %>% .$rate)
  }, ignoreNULL = FALSE)
  
  # Value box for rate A
  output$rateABox <- renderValueBox({
    rateA <- rateA_reactive()
    if(is.null(rateA)) {
      valueBox("N/A", "CR A")
    } else {
      valueBox(scales::percent(rateA, accuracy = 0.01), "CR A")
    }
  })
  
  # Reactive expression for rate B
  rateB_reactive <- eventReactive(input$applyBtn, {
    data <- data_reactive()
    if(is.null(data)) {
      return(NULL)
    }
    return(data$comparison_df %>% filter(type == "B") %>% .$rate)
  }, ignoreNULL = FALSE)
  
  # Value box for rate B
  output$rateBBox <- renderValueBox({
    rateB <- rateB_reactive()
    if(is.null(rateB)) {
      valueBox("N/A", "CR B")
    } else {
      valueBox(scales::percent(rateB, accuracy = 0.01), "CR B")
    }
  })
  
}

# Run the App
shinyApp(ui = ui, server = server)
