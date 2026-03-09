# -------------------------------------------- #
# Network Disruption App
# -------------------------------------------- #

# ----
# This app gives an example of network disruption

library( shiny )
library( sna )

library(shiny)
library(sna)    # for rgraph, gplot, degree, betweenness

ui <- fluidPage(
  titlePanel("Network Disruption!"),
  
  p("This app illustrates how different targeting strategies disrupt a network.
     Compare random removal to removing highly central actors and observe
     the structural consequences."),

  p("To get started, adjust the slider inputs, then click the 'Intervene!' button."),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number of nodes:", min = 50, max = 250, value = 100, step = 1),
      sliderInput("remove_prop", "Proportion of nodes to remove:", min = 0.01, max = 0.5, value = 0.1, step = 0.01),
      selectInput("targeting", "Targeting method:", choices = c("Random" = "random",
                                                                "Highest degree actors" = "degree",
                                                                "Highest betweenness actor" = "betweenness")),
      actionButton("go", "Intervene!"),
      actionButton("reset", "Reset"),
      br(),
      helpText("The first panel shows a preview before intervention. Press 'Intervene!' to highlight removals (red) and show the after-plot. Press 'Reset' to return to the initial preview state.")
    ),
    mainPanel(
      plotOutput("networkPlot", height = "600px"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  
  # Preview graph: updates when 'n' changes (so user can see the network before clicking)
  preview_graph <- reactive({
    n <- as.integer(input$n)
    p <- 0.05
    
    A <- rgraph(n, tprob = p, mode = "graph")
    A <- (A + t(A)) > 0
    diag(A) <- 0
    A <- 1 * A
    
    # user requested coords <- gplot(A)
    coords <- gplot(A)
    
    list(A = A, coords = coords)
  })
  
  # ReactiveVal to store the intervention; NULL means "no intervention / preview mode"
  intervention <- reactiveVal(NULL)
  
  # When 'Intervene!' is clicked, compute & store intervention (based on current preview)
  observeEvent(input$go, {
    base <- preview_graph()            # use the currently-displayed preview
    A <- base$A
    coords <- base$coords
    targeting <- isolate(input$targeting)
    remove_prop <- isolate(input$remove_prop)
    
    n <- nrow(A)
    k <- max(1, floor(remove_prop * n))
    
    if(targeting == "degree") {
      deg <- degree(A, cmode = "freeman")
      ord <- order(deg, decreasing = TRUE)
      removed <- ord[seq_len(min(k, length(ord)))]
    } else if(targeting == "betweenness") {
      betw <- betweenness(A)
      ord <- order(betw, decreasing = TRUE)
      removed <- ord[seq_len(min(k, length(ord)))]
    } else {
      removed <- sample(seq_len(n), k)
    }
    
    kept <- setdiff(seq_len(n), removed)
    A_after <- A[kept, kept, drop = FALSE]
    coords_after <- coords[kept, , drop = FALSE]
    
    intervention(list(A = A, coords = coords,
                      removed = removed, kept = kept,
                      A_after = A_after, coords_after = coords_after,
                      targeting = targeting, remove_prop = remove_prop))
  })
  
  # Reset button: clear the intervention -> back to preview
  observeEvent(input$reset, {
    intervention(NULL)
  })
  
  output$networkPlot <- renderPlot({
    # If no intervention stored: show SINGLE preview panel (all blue)
    if (is.null(intervention())) {
      base <- preview_graph()
      A <- base$A
      coords <- base$coords
      n <- nrow(A)
      
      par(mfrow = c(1,1), mar = c(1,1,3,1))
      gplot(A,
            coord = coords,
            displaylabels = FALSE,
            usearrows = FALSE,
            vertex.cex = 1.2,
            vertex.col = rep("#1485F5", n),
            main = "Pre-Intervention")
      return()
    }
    
    # After intervention: draw two panels with removals highlighted in the BEFORE panel
    dat <- intervention()
    req(dat)
    
    A <- dat$A
    coords <- dat$coords
    n <- nrow(A)
    
    # colors: before -> removed red, others blue
    col_before <- rep("#1485F5", n)
    col_before[dat$removed] <- "red"
    
    par(mfrow = c(1,2), mar = c(1,1,3,1))
    
    gplot(A,
          coord = coords,
          displaylabels = FALSE,
          usearrows = FALSE,
          vertex.cex = 1.2,
          vertex.col = col_before,
          main = "Pre-Intervention")
    
    # After panel: only kept nodes, all blue
    gplot(dat$A_after,
          coord = dat$coords_after,
          displaylabels = FALSE,
          usearrows = FALSE,
          vertex.cex = 1.2,
          vertex.col = rep("#1485F5", length(dat$kept)),
          main = "Post-Intervention" )
  })
  
}

shinyApp(ui, server)