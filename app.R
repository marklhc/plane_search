#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

qnorm_trunc <- function(p, mean = 0, sd = 1, ll = 0, ul = 1) {
  cdf_ll <- pnorm(ll, mean = mean, sd = sd)
  cdf_ul <- pnorm(ul, mean = mean, sd = sd)
  qnorm(cdf_ll + p * (cdf_ul - cdf_ll), mean = mean, sd = sd)
}

rnorm_trunc <- function(n, mean = 0, sd = 1, ll = 0, ul = 1) {
  p <- runif(n)
  qnorm_trunc(p, mean = mean, sd = sd, ll = ll, ul = ul)
}

dnorm_trunc <- function(x, mean = 0, sd = 1, ll = 0, ul = 1) {
  out <- dnorm(x, mean, sd) / (pnorm(ul, mean, sd) - pnorm(ll, mean, sd))
  out[x > ul | x < ll] <- 0
  out
}

compute_lik <- function(x, pts = grid, sd = 0.2, binwidth = .01) {
  lik_vals <- vapply(x, dnorm_trunc, mean = pts, sd = sd, 
                     FUN.VALUE = numeric(length(pts)))
  lik <- apply(lik_vals, 1, prod)
  lik / sum(lik) / binwidth
}

update_probs <- function(prior_probs, lik, binwidth = .01) {
  post_probs <- prior_probs * lik
  post_probs / sum(post_probs) / binwidth
}

find_hdi_grid <- function(grid, dens, prob = .95) {
  dens <- dens / sum(dens)
  sorted_dens <- sort(dens, decreasing = TRUE)
  hdi_idx <- min(which(cumsum(sorted_dens) >= prob))
  hdi_height <- sorted_dens[hdi_idx]
  grid[range(which(dens >= hdi_height))]
}

ngrid <- 101
grid <- seq(0, 1, length.out = ngrid)
true_loc <- runif(1, 0.05, 0.95)

library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Plane Search Example"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("priors", "Prior",
                  c("Prior 1 (Flat)" = 1,
                    "Prior 2 (Weak)" = 2, 
                    "Prior 3 (Strong)" = 3)),
      actionButton("draw", "Find debris"), 
      actionButton("draw2", "Find 10 debris"),
      checkboxInput("show", "Reveal plane location"), 
      # textOutput("newcandy"), 
      textOutput("total")
      # textOutput("yellowcandy")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("priorPlot", height = "200px", width = "62%"), 
      plotOutput("likliPlot", height = "200px", width = "62%"), 
      plotOutput("posteriorPlot", height = "200px", width = "62%")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # prior_probs <- rep(1, ngrid)
  # prior_probs <- dnorm_trunc(grid, mean = .8, sd = 0.5)
  prior_probs <- reactive({
    switch(input$priors,
           "1" = rep(1, ngrid),
           "2" = dnorm_trunc(grid, mean = .8, sd = 0.5), 
           "3" = dnorm_trunc(grid, mean = .8, sd = 0.1))
  })
  v <- reactiveValues(data = NULL, lik = rep(1, ngrid), 
                      post_probs = isolate(prior_probs()))
  # b <- reactive({
  #   switch(input$priors, 
  #          "1" = 1, 
  #          "2" = 2, 
  #          "3" = 8)
  # })
  observeEvent(input$draw, {
    x <- rnorm_trunc(1, mean = true_loc, sd = 0.2)
    lik_x <- compute_lik(x)
    v$data <- c(v$data, x)
    v$lik <- update_probs(v$lik, lik_x)
    v$post_probs <- update_probs(v$post_probs, lik_x)
  })
  observeEvent(input$draw2, {
    x <- rnorm_trunc(10, mean = true_loc, sd = 0.2)
    lik_x <- compute_lik(x)
    v$data <- c(v$data, x)
    v$lik <- update_probs(v$lik, lik_x)
    v$post_probs <- update_probs(v$post_probs, lik_x)
  })
  observeEvent(input$priors, {
    v$data <- NULL
    v$lik <- rep(1, ngrid)
    v$post_probs <- prior_probs()
  })
  # output$newcandy <- renderText(paste0("Last Candy: ", v$data[length(v$data)]))
  output$total <- renderText(paste0("Total # of data points: ", length(v$data)))
  # output$yellowcandy <- renderText(paste0("# of 'Yellow' Candy: ", 
  #                                         sum(v$data)))
  output$priorPlot <- renderPlot({
    # generate plot for the prior
    ggplot(tibble(x = grid, dens = prior_probs()), 
           aes(x = x, y = dens)) + 
      geom_line() + 
      theme(axis.text.x = element_blank(), 
            axis.title.x = element_blank()) + 
      labs(y = "Prior probability") + 
      xlim(0, 1)
  })
  
  output$likliPlot <- renderPlot({
    # generate plot for the likelihood
    lik_p <- 
    ggplot(tibble(x = grid, dens = v$lik), 
           aes(x = x, y = dens)) + 
      geom_line() + 
      theme(axis.text.x = element_blank(), 
            axis.title.x = element_blank()) + 
      labs(y = "Likelihood (Scaled)") + 
      xlim(0, 1)
    if (!is.null(v$data)) {
      lik_p <- lik_p + 
        geom_point(data = tibble(x = v$data), aes(x = x), y = 0, 
                   shape = 1)
    }
    lik_p
  })
  
  output$posteriorPlot <- renderPlot({
    # generate plot for the likelihood
    post_p <- ggplot(tibble(x = grid, dens = v$post_probs), 
                     aes(x = x, y = dens)) + 
      geom_line() + 
      annotate("rect", xmin = find_hdi_grid(grid, v$post_probs)[1], 
               xmax = find_hdi_grid(grid, v$post_probs)[2], 
               ymin = -Inf, 
               ymax = Inf, col = "red", 
               fill = "red", alpha = 0.1) + 
      theme(axis.text.x = element_blank(), 
            axis.title.x = element_blank()) + 
      labs(y = "Density") + 
      xlim(0, 1)
    if (input$show) {
      post_p <- post_p + 
        geom_point(x = true_loc, y = 0, col = "red")
    }
    post_p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

