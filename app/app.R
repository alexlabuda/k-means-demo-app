
# LIBRARIES ---------------------------------------------------------------

library(shiny)
library(shinyBS)
library(tidyverse)
library(bslib)
library(palmerpenguins)
library(tidyquant)
library(tidyclust)
library(tidymodels)
library(ClusterR)


# set default theme for plots
ggplot2::theme_set(theme_minimal())

# read data used for the app
pengs_tbl <- palmerpenguins::penguins
iris_tbl  <- iris |> dplyr::as_tibble() |> dplyr::rename(species = Species)

# create a list of datasets
datasets <- list(
  "Penguins" = pengs_tbl,
  "Iris"     = iris_tbl
)

# select column names for scatter plot x, y-axis selections
names_penguins <- datasets[["Penguins"]] |> select(where(is.numeric)) |> names()
names_iris     <- datasets[["Iris"]] |> select(where(is.numeric)) |> names()


# UI ----------------------------------------------------------------------

ui <- bslib::page_sidebar(
  # Custom CSS to change sidebar opacity
  tags$head(
    tags$style(HTML("
        .sidebar { 
          background-color: rgba(255, 255, 255, 0.95) !important; 
        }
      "))
  ),
  
  title   = "K-Means Clustering Demo",
  theme   = bs_theme(bootswatch = "minty"),
  
  sidebar = sidebar(
    width   = 450,
    
    # Heading and introduction text
    p("This interactive tool is designed to help you explore the application of the K-means clustering algorithm on different datasets. By visualizing how data points are grouped into clusters, you can gain insights into data patterns and relationships.",
      style = "font-size: 15px"),
    br(),
    h4("Settings", style = "border-bottom: 1px solid #cccccc; padding-bottom: 5px; font-size: 16px"),
    
    # Inputs for dataset and number of clusters
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
    
    # Conditional panel selections based on dataset selected
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
  
  # Main UI section
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
    card_header(
      "SSE vs. Number of Clusters",
      tooltip(
        bsicons::bs_icon("question-circle"),
        "The total within-cluster sum of squares. One common method to choose the optimal number of clusters is the elbow method, which involves plotting the tot.withinss against various values of clusters and looking for an elbow point. This point indicates a diminishing return in improvement of cluster compactness, suggesting an optimal value.",
        placement = "right"
      )
      ),
    height = "25%",
    min_height = "240px",
    plotOutput(outputId = "tune_plot")
  )
)


server <- function(input, output, session) {
  
  # Reactive expression to get the selected dataset
  selected_data <- reactive({
    datasets[[input$dataset]] |>
      drop_na()
  })
  
  # set plotting defaults
  legend_loc <- "top"
  alpha_val  <- 0.5
  
  
  # Scatterplot 1 ---------------------------------------------------------
  # Dynamically adjust plot based on the dataset selected
  output$plot <- shiny::renderPlot({
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
    
    ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_point(aes(color = species), size = 3.5, alpha = 0.5) +
      labs(title = paste(input$dataset, "Dataset")) +
      theme(
        legend.position = legend_loc,
        legend.text     = element_text(size = 12),
        panel.grid      = element_blank(),
        axis.text       = element_text(size = 12),
        axis.title      = element_text(size = 12)) +
      tidyquant::scale_color_tq()
  })
  
  # K-means clustering ----------------------------------------------------
  output$cluster_plot <- shiny::renderPlot({
    
    data <- selected_data() |> select(where(is.numeric))
    
    # Recipe preprocessing specification
    rec_spec <- recipe( ~ ., data = data) %>%
      step_normalize(all_numeric_predictors())
    
    # Prep recipe to retrieve means and sds for reverse normalization
    prepped_rec <- prep(rec_spec)
    norm_means  <- prepped_rec$steps[[1]]$means
    norm_sds    <- prepped_rec$steps[[1]]$sds
    
    # K-means model specification
    kmeans_spec <- k_means(num_clusters = input$k) %>%
      set_engine("ClusterR")
    
    # Create a workflow for recipe and model
    kmeans_wf <- workflow(prepped_rec, kmeans_spec)
    
    # Fit the model
    kmeans_fit <- kmeans_wf %>% fit(data = data)
    
    # Extract cluster centroids for plotting
    centroids <- extract_centroids(kmeans_fit)
    
    # Select column names for reverse normalization
    numeric_columns <- names(select(data, where(is.numeric)))
    
    # Reverse normalization for centroids
    for (col in numeric_columns) {
      centroids[[col]] <- (centroids[[col]] * norm_sds[col]) + norm_means[col]
    }
    
    # Augment the original data with cluster predictions
    km_result <- augment(kmeans_fit, data)
    
    # Select user-defined columns for plotting
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
    
    # Rename the cluster column for plotting
    plot_data <- km_result %>%
      mutate(Cluster = factor(.pred_cluster))
    
    # Plot the clustered data
    ggplot(plot_data, aes(x = .data[[x_col]], y = .data[[y_col]], color = Cluster)) +
      geom_point(size = 3.5, alpha = alpha_val - 0.3) +
      geom_point(
        data = centroids,
        aes(x = .data[[x_col]], y = .data[[y_col]]),
        color = "black",
        size  = 5,
        shape = 15
      ) +
      labs(title = paste("Clustering on", input$dataset, "Dataset")) +
      theme(
        legend.position = legend_loc,
        legend.text     = element_text(size = 12),
        panel.grid      = element_blank(),
        axis.text       = element_text(size = 12),
        axis.title      = element_text(size = 12)) +
      tidyquant::scale_color_tq()
  })
  
  
  # K-means tuning --------------------------------------------------------
  # For selecting the best k clusters
  output$tune_plot <- shiny::renderPlot({
    
    # Data
    data <- selected_data() |> select(where(is.numeric))
    
    # Recipe preprocessing specification
    rec_spec <- recipe( ~ ., data = data) %>%
      step_normalize(all_numeric_predictors())
    
    # Prep and bake our data
    data <- prep(rec_spec) |> bake(new_data = data)
    
    kmeans_mapper <- function(centers = 3) {
      
      data |>
        kmeans(centers = centers, nstart = 1)
      
    }
    
    # map() function to many elements
    k_means_mapped_tbl <-
      tibble(centers = 1:9) |>
      mutate(k_means = centers |> map(kmeans_mapper)) |>
      mutate(glance  = k_means |> map(glance))
    
    
    ### Skree Plot ----
    k_means_mapped_tbl |>
      unnest(cols = glance) |>
      select(centers, tot.withinss) |>
      ggplot(aes(centers, tot.withinss)) +
      geom_point(size = 2) +
      geom_line() +
      ggrepel::geom_label_repel(aes(label = centers)) +
      theme_minimal() +
      labs(
        x = "Number of clusters"
        # title    = "Scree Plot",
        # subtitle = "Measures the distance that each customers \nare from the closest K-Means center",
        # caption  = "Based on the Scree Plot, we are selecting 5 clusters to segment customers"
      ) +
      theme(plot.title = element_text(face = "bold", size = 14),
            panel.grid = element_blank(),
            axis.text  = element_text(size = 12),
            axis.title = element_text(size = 12))
    
    
    # Old code for skree plot
    # # Model specification
    # kmeans_spec_tune <- k_means() %>%
    #   set_engine("ClusterR")
    # 
    # # Tuning
    # kmeans_spec_tuned <- kmeans_spec_tune %>%
    #   set_args(num_clusters = tune())
    # 
    # # Workflow
    # kmeans_wf <- workflow() %>%
    #   add_model(kmeans_spec_tuned) %>%
    #   add_formula( ~ .)
    # 
    # # Bootstrap resampling
    # set.seed(1234)
    # x_boots <- bootstraps(data, times = 5)
    # 
    # # Create a grid of k clusters
    # num_clusters_grid <- tibble(num_clusters = seq(1, 10))
    # 
    # # Tune the model
    # tune_res <- tune_cluster(object    = kmeans_wf,
    #                          resamples = x_boots,
    #                          grid      = num_clusters_grid)
    # 
    # # Plot the tuning results
    # tune_res %>%
    #   collect_metrics() |>
    #   filter(.metric == "sse_within_total") |>
    #   ggplot(aes(x = num_clusters, y = mean)) +
    #   geom_line() +
    #   geom_point() +
    #   geom_text(aes(label = num_clusters),
    #             nudge_x = -0.15,
    #             check_overlap = TRUE) +
    #   labs(x = "Number of Clusters",
    #        y = "SSE Within Total") +
    #   theme(panel.grid = element_blank(),
    #         axis.text  = element_text(size = 12),
    #         axis.title = element_text(size = 12))
  })
}


shiny::shinyApp(ui, server)
