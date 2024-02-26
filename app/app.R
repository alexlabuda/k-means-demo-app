library(shiny)
library(tidyverse)
library(bslib)
library(palmerpenguins)
library(tidyquant)
library(tidyclust)
library(tidymodels)
library(ClusterR)
theme_set(theme_minimal())

pengs_tbl <- penguins
iris_tbl  <- iris |> as_tibble() |> rename(species = Species)


datasets <- list(
  "Penguins" = pengs_tbl,
  "Iris"     = iris_tbl
)

names_penguins <- datasets[["Penguins"]] |> select(where(is.numeric)) |> names()
names_iris     <- datasets[["Iris"]] |> select(where(is.numeric)) |> names()


ui <- page_sidebar(
  title   = "K-Means Clustering Demo",
  theme   = bs_theme(bootswatch = "minty"),
  sidebar = sidebar(
    width   = 450,
    p("This interactive tool is designed to help you explore the application of the K-means clustering algorithm on different datasets. By visualizing how data points are grouped into clusters, you can gain insights into data patterns and relationships.",
      style = "font-size: 15px"),
    br(),
    h4("Settings", style = "border-bottom: 1px solid #cccccc; padding-bottom: 5px; font-size: 16px"),
    selectInput(
      inputId  = "dataset",
      label    = "Choose a dataset",
      choices  = names(datasets),
      selected = "Penguins"
    ),
    sliderInput(
      inputId = "k",
      label   = "Number of clusters",
      step    = 1,
      value   = 3,
      min     = 1,
      max     = 5 
    ),
    br(),
    h4("Select columns for plots", style = "border-bottom: 1px solid #cccccc; padding-bottom: 5px; font-size: 16px"),
    conditionalPanel(
      condition = "input.dataset == 'Penguins'",
      selectInput(
        inputId = "penguins_x",
        label   = "X-axis Column",
        choices = names_penguins,
        selected = "bill_length_mm"
      ),
      selectInput(
        inputId = "penguins_y",
        label   = "Y-axis Column",
        choices = names_penguins,
        selected = "body_mass_g"
      )
    ),
    conditionalPanel(
      condition = "input.dataset == 'Iris'",
      selectInput(
        inputId = "iris_x",
        label   = "X-axis Column",
        choices = names_iris,
        selected = "Sepal.Length"
      ),
      selectInput(
        inputId = "iris_y",
        label   = "Y-axis Column",
        choices = names_iris,
        selected = "Sepal.Width"
      )
    )
  ),
  layout_column_wrap(
    card(
      card_header("Original Data"),
      plotOutput(outputId = "plot"),
    ),
    card(
      card_header("Clusters"),
      plotOutput(outputId = "cluster_plot")
    )
  ),
  card(
    card_header("SSE vs. Number of Clusters"),
    height = "25%",
    plotOutput(outputId = "tune_plot")
  )
)


server <- function(input, output) {
  # Reactive expression to get the selected dataset
  selected_data <- reactive({
    datasets[[input$dataset]] |>
      drop_na()
  })
  
  
  legend_loc <- "top"
  alpha_val  <- 0.5
  
  
  
  # Dynamically adjust plot based on the dataset selected
  output$plot <- renderPlot({
    data <- selected_data()
    x_col <-
      if (input$dataset == "Iris")
        input$iris_x
    else
      input$penguins_x
    y_col <-
      if (input$dataset == "Iris")
        input$iris_y
    else
      input$penguins_y
    
    ggplot(data, aes_string(x = x_col, y = y_col)) +
      geom_point(aes(color = species), size = 3, alpha = 0.5) +
      labs(title = paste(input$dataset, "Dataset")) +
      theme(legend.position = "top",
            legend.text = element_text(size = 12),
            panel.grid = element_blank(),
            axis.text = element_text(size = 11)) +
      tidyquant::scale_color_tq()
  })
  
  
  output$cluster_plot <- renderPlot({
    data <- selected_data() |> select(where(is.numeric))
    rec_spec <- recipe( ~ ., data = data) %>%
      step_normalize(all_numeric_predictors())
    
    prepped_rec <- prep(rec_spec)
    norm_means <- prepped_rec$steps[[1]]$means
    norm_sds   <- prepped_rec$steps[[1]]$sds
    
    kmeans_spec <- k_means(num_clusters = input$k) %>%
      set_engine("ClusterR")
    
    kmeans_wf <- workflow(prepped_rec, kmeans_spec)
    
    kmeans_fit <- kmeans_wf %>%
      fit(data = data)
    
    centroids <- extract_centroids(kmeans_fit)
    
    # Reverse normalization for centroids
    numeric_columns <- names(select(data, where(is.numeric)))
    for (col in numeric_columns) {
      centroids[[col]] <-
        (centroids[[col]] * norm_sds[col]) + norm_means[col]
    }
    
    km_result <- augment(kmeans_fit, data)
    
    # Now, select user-defined columns for plotting
    x_col <-
      if (input$dataset == "Iris")
        input$iris_x
    else
      input$penguins_x
    y_col <-
      if (input$dataset == "Iris")
        input$iris_y
    else
      input$penguins_y
    
    plot_data <- km_result %>%
      mutate(Cluster = factor(.pred_cluster))
    
    ggplot(plot_data, aes_string(x = x_col, y = y_col, color = "Cluster")) +
      geom_point(size = 3, alpha = alpha_val - 0.25) +
      geom_point(
        data = centroids,
        aes_string(x = x_col, y = y_col),
        color = "black",
        size = 5,
        shape = 15
      ) +
      labs(title = paste("Clustering on", input$dataset, "Dataset")) +
      theme(legend.position = legend_loc,
            legend.text = element_text(size = 12),
            panel.grid = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12)) +
      tidyquant::scale_color_tq()
  })
  
  output$tune_plot <- renderPlot({
    # Data
    data <- selected_data() |> select(where(is.numeric))
    
    kmeans_spec_tune <- k_means() %>%
      set_engine("ClusterR")
    
    ## TUNING
    kmeans_spec_tuned <- kmeans_spec_tune %>%
      set_args(num_clusters = tune())
    
    kmeans_wf <- workflow() %>%
      add_model(kmeans_spec_tuned) %>%
      add_formula( ~ .)
    
    set.seed(1234)
    x_boots <- bootstraps(data, times = 5)
    
    num_clusters_grid <- tibble(num_clusters = seq(1, 10))
    
    tune_res <- tune_cluster(object    = kmeans_wf,
                             resamples = x_boots,
                             grid      = num_clusters_grid)
    
    tune_res %>%
      collect_metrics() |>
      filter(.metric == "sse_within_total") |>
      ggplot(aes(x = num_clusters, y = mean)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = num_clusters),
                nudge_x = -0.15,
                check_overlap = TRUE) +
      labs(x = "Number of Clusters",
           y = "SSE Within Total") +
      theme(panel.grid = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12))
  })
  
}


shinyApp(ui, server)

