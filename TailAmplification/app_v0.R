library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Tail Amplification: How Small Mean Shifts Create Huge 'Extreme' Changes"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Distribution Settings"),
      selectInput("dist_type", "Distribution:",
                  choices = c("Normal" = "norm", "t-distribution" = "t")),
      conditionalPanel(
        condition = "input.dist_type == 't'",
        numericInput("df", "t df:", 5, min = 1, max = 50)
      ),
      sliderInput("sigma", "Standard deviation σ:",
                  min = 0.5, max = 3, value = 1, step = 0.1),
      
      hr(),
      h4("Shift & Threshold"),
      sliderInput("mean_shift", "Mean shift Δμ:",
                  min = 0, max = 3, value = 1.2, step = 0.1,
                  animate = animationOptions(interval = 300)),
      sliderInput("threshold", "Threshold for 'extreme':",
                  min = 0, max = 6, value = 2.5, step = 0.1),
      
      hr(),
      h4("Climate Analogy"),
      p("Think: μ = average temp, σ = 1°C day-to-day variation"),
      p("Threshold = 'extreme hot day' definition"),
      p("Δμ = 'global warming' shift")
    ),
    
    mainPanel(
      plotOutput("dist_plot", height = "400px"),
      br(),
      fluidRow(
        column(6,
               h4("Probabilities"),
               tableOutput("prob_table")
        ),
        column(6,
               h4("Amplification Factor"),
               h2(textOutput("amplification")),
               p("= P_after / P_before"),
               textOutput("interpretation")
        )
      ),
      hr(),
      h4("Key Insight"),
      p("The farther out in the tail your threshold is, the more sensitive it is to small mean shifts.
        Moving from μ to μ+1 when your threshold is at μ+2σ increases exceedances by ~6x.
        But if your threshold is at μ+1σ, the same shift only ~doubles it.")
    )
  )
)

server <- function(input, output, session) {
  
  # Update threshold range when sigma changes
  observe({
    updateSliderInput(session, "threshold",
                      max = 6 * input$sigma,
                      value = min(input$threshold, 6 * input$sigma))
  })
  
  # Calculate probabilities
  probs <- reactive({
    if(input$dist_type == "norm") {
      p_before <- 1 - pnorm(input$threshold, mean = 0, sd = input$sigma)
      p_after <- 1 - pnorm(input$threshold, mean = input$mean_shift, sd = input$sigma)
    } else {
      p_before <- 1 - pt(input$threshold / input$sigma, df = input$df)
      p_after <- 1 - pt((input$threshold - input$mean_shift) / input$sigma, df = input$df)
    }
    
    data.frame(
      Scenario = c("Before shift: μ = 0",
                   paste0("After shift: μ = ", input$mean_shift)),
      `P(X > threshold)` = c(p_before, p_after),
      `Per 100 days` = round(c(p_before, p_after) * 100, 2),
      check.names = FALSE
    )
  })
  
  output$prob_table <- renderTable({
    probs() %>% mutate(`P(X > threshold)` = sprintf("%.4f", `P(X > threshold)`))
  }, digits = 4)
  
  output$amplification <- renderText({
    p <- probs()$`P(X > threshold)`
    if(p[1] < 1e-6) return("∞")
    sprintf("%.1fx", p[2] / p[1])
  })
  
  output$interpretation <- renderText({
    p <- probs()$`P(X > threshold)`
    ratio <- p[2] / p[1]
    thresh_sigma <- input$threshold / input$sigma
    paste0("With threshold at ", round(thresh_sigma, 1), "σ above the original mean, ",
           "a ", input$mean_shift, "° shift increases extremes by ", round(ratio, 1), "x")
  })
  
  output$dist_plot <- renderPlot({
    x <- seq(-4*input$sigma, 4*input$sigma + input$mean_shift, length.out = 1000)
    
    if(input$dist_type == "norm") {
      y1 <- dnorm(x, 0, input$sigma)
      y2 <- dnorm(x, input$mean_shift, input$sigma)
    } else {
      y1 <- dt(x / input$sigma, df = input$df) / input$sigma
      y2 <- dt((x - input$mean_shift) / input$sigma, df = input$df) / input$sigma
    }
    
    df_plot <- data.frame(
      x = rep(x, 2),
      y = c(y1, y2),
      Dist = rep(c("Original", paste0("Shifted +", input$mean_shift)), each = length(x))
    )
    
    ggplot(df_plot, aes(x, y, color = Dist, fill = Dist)) +
      geom_line(linewidth = 1.2) +
      geom_area(data = subset(df_plot, x > input$threshold),
                alpha = 0.3, position = "identity") +
      geom_vline(xintercept = input$threshold, linetype = "dashed", linewidth = 1) +
      annotate("text", x = input$threshold, y = max(y1)*1.05,
               label = paste("Threshold =", input$threshold),
               hjust = -0.1, size = 4.5) +
      labs(title = "Shifting the Mean Increases Tail Probability Non-linearly",
           x = "Value (e.g., Daily Temperature)", y = "Density",
           subtitle = "Shaded area = P(X > threshold)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top") +
      scale_color_manual(values = c("Original" = "#2C3E50", "Shifted +1.2" = "#E74C3C")) +
      scale_fill_manual(values = c("Original" = "#2C3E50", "Shifted +1.2" = "#E74C3C"))
  })
}

shinyApp(ui = ui, server = server)