# -------------------------------------------- #
# Sampling Distributions App
# -------------------------------------------- #

# ----
# This app shows how the dispersion of a sampling distribution
# changes as you increase the sample size in a sample


# ----
# load the libraries

library( shiny )
library( ggplot2 )



# ----
# set up the user interface

ui <- fluidPage(
  titlePanel("Sampling Distributions"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Per-panel sample sizes"),
      sliderInput("n1", "Sample size for Panel 1 (n1):", min = 1, max = 500, value = 100, step = 1),
      sliderInput("n2", "Sample size for Panel 2 (n2):", min = 1, max = 500, value = 100, step = 1),
      sliderInput("n3", "Sample size for Panel 3 (n3):", min = 1, max = 500, value = 100, step = 1),
      hr(),
      sliderInput("nrep", "Replications (per panel):", min = 100, max = 5000, value = 1000, step = 100),
      actionButton("resample", "Resample"),
      helpText("Adjust sample sizes, then click Resample (or click multiple times) to redraw.")
    ),
    mainPanel(
      plotOutput("distPlot", height = "420px"),
      verbatimTextOutput("summary")
    )
  )
)



# ----
# set up the server

server <- function(input, output, session) {
  
  # Generate simulated sample means when any relevant input changes OR when resample is pressed.
  sim_data <- eventReactive(
    list(input$n1, input$n2, input$n3, input$nrep, input$resample),
    {
      n_vec <- c(input$n1, input$n2, input$n3)
      nrep <- input$nrep
      panel_names <- c("Panel 1", "Panel 2", "Panel 3")
      
      df_list <- lapply(seq_along(n_vec), function(i) {
        n <- n_vec[i]
        means <- replicate(nrep, mean(rnorm(n)))
        data.frame(
          panel = panel_names[i],
          sample_mean = means,
          n = n
        )
      })
      
      df <- do.call(rbind, df_list)
      
      df$panel <- factor(df$panel,
                         levels = panel_names)
      
      df
    },
    ignoreNULL = FALSE
  )
  
  output$distPlot <- renderPlot({
    df <- sim_data()
    
    # Determine x-range for overlay lines / consistent x-limits
    x_min <- min(df$sample_mean)
    x_max <- max(df$sample_mean)
    x_grid <- seq(x_min, x_max, length.out = 300)
    
    # Theoretical density lines per panel (assumes population mean = 0 and sd = 1)
    panel_ns <- unique(df[, c("panel", "n")])
    theo_list <- lapply(seq_len(nrow(panel_ns)), function(i) {
      n_val <- panel_ns$n[i]
      panel_name <- panel_ns$panel[i]
      data.frame(x = x_grid,
                 density = dnorm(x_grid, mean = 0, sd = 1 / sqrt(n_val)),
                 panel = panel_name)
    })
    theo_df <- do.call(rbind, theo_list)
    
    # Observed means per panel for vertical lines
    obs_summary <- aggregate(sample_mean ~ panel, data = df, FUN = mean)
    
    ggplot(df, aes(x = sample_mean, fill = panel)) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.6, position = "identity", color = NA) +
      geom_line(data = theo_df, aes(x = x, y = density), inherit.aes = FALSE, color = "black", size = 1) +
      geom_vline(data = obs_summary, aes(xintercept = sample_mean), color = "red", linetype = "dashed", size = 0.8) +
      facet_wrap(~ panel, nrow = 1 ) +
      labs(title = "Sampling distribution of the sample mean",
           subtitle = paste0("Replications per panel = ", input$nrep),
           x = "Sample mean",
           y = "Density") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$summary <- renderPrint({
    df <- sim_data()
    obs_summary <- aggregate(sample_mean ~ panel, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
    # format output
    cat("Simulated summaries (per panel):\n\n")
    for(i in seq_len(nrow(obs_summary))) {
      panel_name <- obs_summary$panel[i]
      nums <- obs_summary$sample_mean[i, ]
      cat(panel_name, " — mean:", signif(nums["mean"], 4), " sd:", signif(nums["sd"], 4), "\n")
    }
    cat("\nTip: click 'Resample' to draw a new set of simulations with the current sample sizes.\n")
  })
  
}

shinyApp( ui, server )