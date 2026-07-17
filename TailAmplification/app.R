library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinythemes)
library(latex2exp)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Tail Amplification: Why Small Mean Shifts Cause Huge Changes in Extremes"),
  
  tabsetPanel(
    tabPanel("Interactive Explorer",
             sidebarLayout(
               sidebarPanel(
                 h4("Distribution Settings"),
                 selectInput("preset", "Preset:",
                             choices = c("Berlin Summer Max Temp" = "berlin_summer",
                                         "Generic" = "generic")),
                 
                 # Always visible + editable
                 numericInput("mean", "Baseline mean μ:", 0, step = 0.1),
                 sliderInput("sigma", "Standard deviation σ:",
                             min = 0.5, max = 5, value = 1, step = 0.1),
                 
                 conditionalPanel(
                   condition = "input.preset == 'berlin_summer'",
                   helpText("DWD 1991-2020 climatology: Berlin-Dahlem"),
                   helpText("July-August daily Tmax: μ=24.5°C, σ=3.2°C"),
                   helpText("Source: Deutscher Wetterdienst")
                 ),
                 
                 selectInput("dist_type", "Distribution:",
                             choices = c("Normal" = "norm", "t-distribution" = "t")),
                 conditionalPanel(
                   condition = "input.dist_type == 't'",
                   numericInput("df", "t df:", 5, min = 1, max = 50)
                 ),
                 
                 hr(),
                 h4("Shift & Threshold"),
                 sliderInput("mean_shift", "Mean shift Δμ:",
                             min = 0, max = 5, value = 1.5, step = 0.1,
                             animate = animationOptions(interval = 300)),
                 
                 selectInput("dwd_threshold", "DWD Extreme Definition:",
                             choices = c("Custom" = "custom",
                                         "Sommertag ≥25°C" = "25",
                                         "Heißer Tag ≥30°C" = "30", 
                                         "Extrem heiß ≥35°C" = "35",
                                         "Rekordbereich ≥38°C" = "38"),
                             selected = "30"),
                 
                 conditionalPanel(
                   condition = "input.dwd_threshold == 'custom'",
                   sliderInput("threshold", "Custom threshold:",
                               min = 10, max = 45, value = 30, step = 0.1)
                 ),
                 
                 hr(),
                 h4("Climate Context"),
                 p("Δμ = 1.5°C matches observed warming in Central Europe 1950-2020"),
                 p("Berlin summer: ~92 days Jul-Aug")
               ),
               
               mainPanel(
                 plotOutput("dist_plot", height = "350px"),
                 plotOutput("amplification_plot", height = "300px"),
                 br(),
                 fluidRow(
                   column(6,
                          h4("Current Threshold"),
                          tableOutput("prob_table"),
                          h3(textOutput("amplification")),
                          p("= P_after / P_before")
                   ),
                   column(6,
                          h4("Key Numbers"),
                          verbatimTextOutput("key_stats")
                   )
                 )
               )
             )
    ),
    
    tabPanel("Explanation & Math",
             fluidRow(
               column(8, offset = 2,
                      h3("Why is the effect so nonlinear?"),
                      withMathJax(),
                      
                      p("For a bell-shaped distribution, the probability of exceeding threshold \\(T\\) is:"),
                      p("$$P(X > T | \\mu, \\sigma) = 1 - F\\left(\\frac{T - \\mu}{\\sigma}\\right)$$"),
                      p("Where \\(F\\) is the CDF. For a normal: \\(F = \\Phi\\), the standard normal CDF."),
                      
                      h4("The Amplification Ratio"),
                      p("If the mean shifts from \\(\\mu\\) to \\(\\mu + \\Delta\\), the ratio of exceedance probabilities is:"),
                      p("$$R(T, \\Delta) = \\frac{1 - \\Phi\\left(\\frac{T - \\mu - \\Delta}{\\sigma}\\right)}{1 - \\Phi\\left(\\frac{T - \\mu}{\\sigma}\\right)}$$"),
                      
                      h4("Why it explodes in the tails"),
                      p("For large \\(z\\), the normal tail behaves like \\(1 - \\Phi(z) \\approx \\frac{\\phi(z)}{z}\\), where \\(\\phi\\) is the PDF."),
                      p("This means:"),
                      p("$$R(T, \\Delta) \\approx \\exp\\left(\\frac{\\Delta(T - \\mu - \\Delta/2)}{\\sigma^2}\\right) \\text{ for } T \\gg \\mu$$"),
                      p("So the amplification is exponential in both the shift \\(\\Delta\\) and how far out \\(T\\) is."),
                      
                      h4("Berlin Example"),
                      p("DWD climatology: \\(\\mu = 24.5°C\\), \\(\\sigma = 3.2°C\\) for July-August Tmax"),
                      p("DWD 'Heißer Tag': \\(T = 30°C = \\mu + 1.7\\sigma\\)"),
                      p("With \\(\\Delta = 1.5°C\\) warming:"),
                      p("$$R \\approx \\exp\\left(\\frac{1.5 \\times (30 - 24.5 - 0.75)}{3.2^2}\\right) = \\exp(0.70) = 2.0$$"),
                      p("So hot days double. If we used \\(T = 35°C = \\mu + 3.3\\sigma\\), then \\(R \\approx 4.8\\). Same warming, 5x more extremes."),
                      p("At \\(T = 38°C\\), \\(R \\approx 12\\). The headline can be anything depending on threshold."),
                      
                      h4("Takeaway"),
                      tags$ul(
                        tags$li("The 'dramatic increase' in extremes is mathematically expected from small mean shifts"),
                        tags$li("It does NOT require increased variance or 'more volatile weather'"),
                        tags$li("The exact factor depends critically on threshold choice — DWD's definitions span 25-35°C+"),
                        tags$li("Policy relevance: Heatwave impacts scale non-linearly, even if mean warming seems 'modest'"),
                        tags$li("This is why IPCC uses multiple indices: TX90p, TXx, WSDI, etc. Each tells a different story")
                      )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Update defaults when preset changes
  observeEvent(input$preset, {
    if(input$preset == "berlin_summer") {
      updateNumericInput(session, "mean", value = 24.5)
      updateSliderInput(session, "sigma", value = 3.2)
      updateSelectInput(session, "dwd_threshold", selected = "30")
      updateSliderInput(session, "threshold", value = 30)
    } else {
      updateNumericInput(session, "mean", value = 0)
      updateSliderInput(session, "sigma", value = 1)
      updateSelectInput(session, "dwd_threshold", selected = "custom")
      updateSliderInput(session, "threshold", value = 2.5)
    }
  })
  
  # Sync threshold slider when DWD dropdown changes
  observeEvent(input$dwd_threshold, {
    if(input$dwd_threshold != "custom") {
      updateSliderInput(session, "threshold", value = as.numeric(input$dwd_threshold))
    }
  })
  
  # Get current threshold value
  thresh_val <- reactive({
    if(input$dwd_threshold == "custom") {
      input$threshold
    } else {
      as.numeric(input$dwd_threshold)
    }
  })
  
  # Params from inputs
  params <- reactive({
    list(mu = input$mean, sigma = input$sigma)
  })
  
  # Update threshold slider range based on current mu/sigma
  observe({
    p <- params()
    updateSliderInput(session, "threshold",
                      min = round(p$mu - 2*p$sigma, 1),
                      max = round(p$mu + 6*p$sigma, 1))
  })
  
  # Calculate probabilities for current threshold
  probs <- reactive({
    p <- params()
    T_val <- thresh_val()
    
    if(input$dist_type == "norm") {
      p_before <- 1 - pnorm(T_val, mean = p$mu, sd = p$sigma)
      p_after <- 1 - pnorm(T_val, mean = p$mu + input$mean_shift, sd = p$sigma)
    } else {
      z_before <- (T_val - p$mu) / p$sigma
      z_after <- (T_val - p$mu - input$mean_shift) / p$sigma
      p_before <- 1 - pt(z_before, df = input$df)
      p_after <- 1 - pt(z_after, df = input$df)
    }
    
    data.frame(
      Scenario = c("Baseline", paste0("+", input$mean_shift, "° shift")),
      `P(X > T)` = c(p_before, p_after),
      `Days/summer` = round(c(p_before, p_after) * 92, 1),
      `Days/year` = round(c(p_before, p_after) * 365, 1),
      check.names = FALSE
    )
  })
  
  output$prob_table <- renderTable({
    probs() %>% mutate(`P(X > T)` = sprintf("%.5f", `P(X > T)`))
  }, digits = 5)
  
  output$amplification <- renderText({
    p <- probs()$`P(X > T)`
    if(p[1] < 1e-7) return("Amplification: >1000x")
    sprintf("Amplification: %.1fx", p[2] / p[1])
  })
  
  output$key_stats <- renderText({
    p <- params()
    T_val <- thresh_val()
    z_score <- (T_val - p$mu) / p$sigma
    thresh_name <- switch(input$dwd_threshold,
                          "25" = "Sommertag",
                          "30" = "Heißer Tag", 
                          "35" = "Extrem heiß",
                          "38" = "Rekordbereich",
                          "custom" = "Custom")
    paste0(
      "Threshold: ", T_val, "°C (", thresh_name, ")\n",
      "Baseline mean: ", p$mu, "°C\n",
      "Std dev: ", p$sigma, "°C\n",
      "Threshold = μ + ", round(z_score, 2), "σ\n",
      "Baseline percentile: ", round(pnorm(z_score)*100, 1), "th\n",
      "Return period: 1 in ", round(1/(1-pnorm(z_score)), 0), " days"
    )
  })
  
  # Main distribution plot
  output$dist_plot <- renderPlot({
    p <- params()
    T_val <- thresh_val()
    x <- seq(p$mu - 4*p$sigma, p$mu + input$mean_shift + 4*p$sigma, length.out = 1000)
    
    if(input$dist_type == "norm") {
      y1 <- dnorm(x, p$mu, p$sigma)
      y2 <- dnorm(x, p$mu + input$mean_shift, p$sigma)
    } else {
      y1 <- dt((x - p$mu)/p$sigma, df = input$df) / p$sigma
      y2 <- dt((x - p$mu - input$mean_shift)/p$sigma, df = input$df) / p$sigma
    }
    
    df_plot <- data.frame(
      x = rep(x, 2),
      y = c(y1, y2),
      Dist = rep(c("Baseline", paste0("Shifted +", input$mean_shift, "°C")), each = length(x))
    )
    
    ggplot(df_plot, aes(x, y, color = Dist, fill = Dist)) +
      geom_line(linewidth = 1.2) +
      geom_area(data = subset(df_plot, x > T_val),
                alpha = 0.3, position = "identity") +
      geom_vline(xintercept = T_val, linetype = "dashed", linewidth = 1) +
      annotate("text", x = T_val, y = max(y1)*1.05,
               label = paste0("T = ", T_val, "°C"),
               hjust = -0.1, size = 4) +
      labs(title = "Shifting the Mean Increases Tail Probability",
           x = "Daily Maximum Temperature (°C)", y = "Density",
           subtitle = "Shaded = P(T > threshold)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top") +
      scale_color_manual(values = c("Baseline" = "#2C3E50", "Shifted +1.5°C" = "#E74C3C")) +
      scale_fill_manual(values = c("Baseline" = "#2C3E50", "Shifted +1.5°C" = "#E74C3C"))
  })
  
  # Amplification vs threshold plot
  output$amplification_plot <- renderPlot({
    p <- params()
    T_range <- seq(p$mu, p$mu + 6*p$sigma, length.out = 300)
    
    if(input$dist_type == "norm") {
      prob_before <- 1 - pnorm(T_range, p$mu, p$sigma)
      prob_after <- 1 - pnorm(T_range, p$mu + input$mean_shift, p$sigma)
    } else {
      prob_before <- 1 - pt((T_range - p$mu)/p$sigma, df = input$df)
      prob_after <- 1 - pt((T_range - p$mu - input$mean_shift)/p$sigma, df = input$df)
    }
    
    amp_ratio <- prob_after / pmax(prob_before, 1e-10)
    
    df_amp <- data.frame(
      threshold = T_range,
      ratio = amp_ratio,
      z_score = (T_range - p$mu) / p$sigma
    ) %>% filter(ratio < 100, ratio > 0.5)
    
    # DWD threshold lines
    dwd_lines <- data.frame(
      thresh = c(25, 30, 35),
      label = c("Sommertag", "Heißer Tag", "Extrem heiß")
    )
    
    ggplot(df_amp, aes(x = threshold, y = ratio)) +
      geom_line(linewidth = 1.5, color = "#E74C3C") +
      geom_vline(data = dwd_lines, aes(xintercept = thresh), 
                 linetype = "dotted", color = "grey40") +
      geom_vline(xintercept = thresh_val(), linetype = "dashed", linewidth = 1) +
      geom_hline(yintercept = 1, linetype = "dotted") +
      geom_point(data = filter(df_amp, abs(threshold - thresh_val()) < 0.1),
                 size = 4, color = "#2C3E50") +
      scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100),
                    labels = c("1x", "2x", "5x", "10x", "20x", "50x", "100x")) +
      labs(title = paste0("Amplification Factor vs Threshold (Δμ = ", input$mean_shift, "°C)"),
           x = "Threshold Temperature (°C)",
           y = "Amplification Factor (log scale)",
           subtitle = "Dashed = current, Dotted = DWD definitions") +
      theme_minimal(base_size = 13) +
      annotation_logticks(sides = "l")
  })
}

shinyApp(ui = ui, server = server)