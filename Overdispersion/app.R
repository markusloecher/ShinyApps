library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  titlePanel("Analytic Overdispersion: Beta-Binomial & Negative Binomial"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Common"),
      #numericInput("nsim", "Number of draws:", 5000, 100, 50000, 100),
      radioButtons("bar_position", "Bar layout:",
                   choices = c("Overlaid" = "identity", "Side-by-side" = "dodge"),
                   selected = "identity", inline = TRUE),
      actionButton("resample", "Resample", class = "btn-primary"),
      
      conditionalPanel("input.tabs == 'Beta-Binomial'",
                       h4("Binomial parameters"),
                       sliderInput("n_bb", "Trials (n):", 1, 100, 20),
                       sliderInput("p_bb", "Mean p:", 0.01, 0.99, 0.3, 0.01),
                       h4("Beta prior"),
                       sliderInput("alpha", "α:", 0.1, 50, 3, 0.1),
                       sliderInput("beta", "β:", 0.1, 50, 7, 0.1)
      ),
      
      conditionalPanel("input.tabs == 'Negative Binomial'",
                       h4("NegBin parameters"),
                       sliderInput("r_nb", "Size (r):", 0.1, 50, 10, 0.1),
                       sliderInput("p_nb", "Prob (p):", 0.01, 0.99, 0.4, 0.01),
                       helpText("Mean = r(1-p)/p. Variance = r(1-p)/p²")
      ),
      
      hr(),
      withMathJax(),
      conditionalPanel("input.tabs == 'Beta-Binomial'",
                       h5("Beta-Binomial variance:"),
                       helpText("$$Var = np(1-p)\\left[1 + \\frac{n-1}{\\alpha+\\beta+1}\\right]$$")
      ),
      conditionalPanel("input.tabs == 'Negative Binomial'",
                       h5("NegBin vs Poisson:"),
                       helpText("If $$X|\\lambda \\sim Pois(\\lambda),\\, \\lambda \\sim Gamma\\left(r, \\frac{1-p}{p}\\right)$$"),
                       helpText("Then $$X \\sim NB(r, p)$$ with $$Var = \\mu + \\mu^2/r > \\mu$$")
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Beta-Binomial",
                           plotOutput("distPlotBB", height = "400px"),
                           fluidRow(
                             column(12, h4("Theoretical comparison"), tableOutput("theoreticalBB"))
                           ),
                           plotOutput("betaPlot", height = "300px")
                  ),
                  tabPanel("Negative Binomial",
                           plotOutput("distPlotNB", height = "400px"),
                           fluidRow(
                             column(12, h4("Theoretical comparison"), tableOutput("theoreticalNB"))
                           ),
                           plotOutput("gammaPlot", height = "300px")
                  ),
                  tabPanel("Explanation",
                           h3("Analytic overdispersion"),
                           p("No simulation here — these are the exact probability mass functions."),
                           h4("Beta-Binomial"),
                           p("If p ~ Beta(α, β) and X | p ~ Binomial(n, p), then marginally:"),
                           p("X ~ BetaBinomial(n, α, β). Mean is np but variance is inflated by"),
                           p("1 + (n-1)/(α+β+1). As α+β → ∞, Beta-Binomial → Binomial."),
                           h4("Negative Binomial"), 
                           p("If λ ~ Gamma(r, (1-p)/p) and X | λ ~ Poisson(λ), then marginally:"),
                           p("X ~ NegBin(r, p). Variance = μ + μ²/r, so always > μ except as r → ∞."),
                           h4("Why this matters"),
                           p("Real data has heterogeneity. Assuming fixed p or λ underestimates variance, 
            gives overconfident intervals, and ruins hypothesis tests."),
                           p("These compound distributions are the 'default' when you have latent variation.")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$p_bb, {
    total <- input$alpha + input$beta
    updateSliderInput(session, "alpha", value = input$p_bb * total)
    updateSliderInput(session, "beta", value = (1 - input$p_bb) * total)
  })
  
  # Analytic Beta-Binomial PMF
  bb_pmf <- reactive({
    k <- 0:input$n_bb
    p_mean <- input$alpha / (input$alpha + input$beta)
    
    # Beta-Binomial via VGAM::dbetabinom.ab or manual
    # P(X=k) = choose(n,k) * Beta(k+α, n-k+β) / Beta(α,β)
    lchoose_nk <- lchoose(input$n_bb, k)
    lbeta_num <- lbeta(k + input$alpha, input$n_bb - k + input$beta)
    lbeta_den <- lbeta(input$alpha, input$beta)
    pmf_bb <- exp(lchoose_nk + lbeta_num - lbeta_den)
    
    # Standard Binomial
    pmf_bin <- dbinom(k, input$n_bb, p_mean)
    
    data.frame(
      k = rep(k, 2),
      prob = c(pmf_bb, pmf_bin),
      Distribution = rep(c("Beta-Binomial", "Binomial"), each = length(k))
    )
  })
  
  # Analytic Negative Binomial PMF
  nb_pmf <- reactive({
    mu <- input$r_nb * (1 - input$p_nb) / input$p_nb
    # Show up to 99.9th percentile to keep plot readable
    max_k <- qnbinom(0.999, size = input$r_nb, prob = input$p_nb)
    k <- 0:max_k
    
    pmf_nb <- dnbinom(k, size = input$r_nb, prob = input$p_nb)
    pmf_pois <- dpois(k, mu)
    
    data.frame(
      k = rep(k, 2),
      prob = c(pmf_nb, pmf_pois),
      Distribution = rep(c("Neg. Binomial", "Poisson"), each = length(k))
    )
  })
  
  output$distPlotBB <- renderPlot({
    df <- bb_pmf()
    # p_mean <- input$alpha / (input$alpha + input$beta)  # redundant line
    # bin_line <- data.frame(x = 0:input$n_bb, y = dbinom(0:input$n_bb, input$n_bb, p_mean))  # redundant line
    
    pos <- if(input$bar_position == "dodge") {
      position_dodge(width = 1, preserve = "single")
    } else {
      "identity"
    }
    
    alpha_val <- if(input$bar_position == "identity") 0.5 else 0.7
    
    ggplot(df, aes(x = k, y = prob, fill = Distribution)) +
      geom_col(width = 1, alpha = alpha_val, color = NA, position = pos) +
      # geom_line(data = bin_line, aes(x = x, y = y),  # redundant line
      #           color = "#21618C", size = 1.2, inherit.aes = FALSE) +  # redundant line
      labs(title = "Beta-Binomial vs Binomial PMF",
           subtitle = "Exact distributions — no simulation",
           x = "Successes k", y = "P(X = k)") +
      scale_fill_manual(values = c("Beta-Binomial" = "#E74C3C", "Binomial" = "#3498DB")) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "bottom")
  })
  
  output$distPlotNB <- renderPlot({
    df <- nb_pmf()
    # mu <- input$r_nb * (1 - input$p_nb) / input$p_nb  # redundant line
    # max_x <- max(df$k)  # redundant line
    # pois_line <- data.frame(x = 0:max_x, y = dpois(0:max_x, mu))  # redundant line
    
    pos <- if(input$bar_position == "dodge") {
      position_dodge(width = 1, preserve = "single")
    } else {
      "identity"
    }
    
    alpha_val <- if(input$bar_position == "identity") 0.5 else 0.7
    
    ggplot(df, aes(x = k, y = prob, fill = Distribution)) +
      geom_col(width = 1, alpha = alpha_val, color = NA, position = pos) +
      # geom_line(data = pois_line, aes(x = x, y = y),  # redundant line
      #           color = "#21618C", size = 1.2, inherit.aes = FALSE) +  # redundant line
      labs(title = "Negative Binomial vs Poisson PMF",
           subtitle = "Exact distributions — no simulation",
           x = "Count k", y = "P(X = k)") +
      scale_fill_manual(values = c("Neg. Binomial" = "#E74C3C", "Poisson" = "#3498DB")) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      coord_cartesian(xlim = c(0, max(df$k))) +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "bottom")
  })
  
  output$betaPlot <- renderPlot({
    p <- seq(0, 1, length.out = 400)
    dens <- dbeta(p, input$alpha, input$beta)
    p_mean <- input$alpha / (input$alpha + input$beta)
    ggplot(data.frame(p, dens), aes(p, dens)) +
      geom_line(color = "#8E44AD", size = 1.5) +
      geom_vline(xintercept = p_mean, linetype = "dashed", color = "red") +
      labs(title = paste0("Beta(α=", round(input$alpha,2), ", β=", round(input$beta,2), ") prior on p"),
           subtitle = paste("Mean p =", round(p_mean, 3)),
           x = "p", y = "Density") +
      theme_minimal(base_size = 14)
  })
  
  output$gammaPlot <- renderPlot({
    mu <- input$r_nb * (1-input$p_nb)/input$p_nb
    lambda <- seq(0, mu * 3, length.out = 400)
    dens <- dgamma(lambda, shape = input$r_nb, scale = (1-input$p_nb)/input$p_nb)
    ggplot(data.frame(lambda, dens), aes(lambda, dens)) +
      geom_line(color = "#8E44AD", size = 1.5) +
      geom_vline(xintercept = mu, linetype = "dashed", color = "red") +
      labs(title = paste0("Gamma(r=", input$r_nb, ", scale=", round((1-input$p_nb)/input$p_nb,2), ") prior on λ"),
           subtitle = paste("Mean λ =", round(mu, 2)),
           x = "λ", y = "Density") +
      theme_minimal(base_size = 14)
  })
  
  output$theoreticalBB <- renderTable({
    p <- input$alpha / (input$alpha + input$beta)
    var_bin <- input$n_bb * p * (1 - p)
    dispersion <- 1 + (input$n_bb - 1) / (input$alpha + input$beta + 1)
    data.frame(
      Metric = c("Mean", "Variance", "Overdispersion factor", "Std Dev"),
      Binomial = round(c(input$n_bb * p, var_bin, 1, sqrt(var_bin)), 3),
      `Beta-Binomial` = round(c(input$n_bb * p, var_bin * dispersion, dispersion, sqrt(var_bin * dispersion)), 3),
      check.names = FALSE
    )
  })
  
  output$theoreticalNB <- renderTable({
    mu <- input$r_nb * (1 - input$p_nb) / input$p_nb
    var_pois <- mu
    var_nb <- mu + mu^2 / input$r_nb
    data.frame(
      Metric = c("Mean", "Variance", "Index of dispersion", "Std Dev"),
      Poisson = round(c(mu, var_pois, 1, sqrt(var_pois)), 3),
      `Neg. Binomial` = round(c(mu, var_nb, var_nb/mu, sqrt(var_nb)), 3),
      check.names = FALSE
    )
  })
  
  output$empiricalBB <- renderTable({
    sim_bb() %>% 
      group_by(Distribution) %>% 
      summarise(Mean = round(sum(value * prop), 3), 
                Variance = round(sum(prop * (value - sum(value * prop))^2), 3), 
                .groups="drop")
  })
  
  output$empiricalNB <- renderTable({
    sim_nb() %>% 
      group_by(Distribution) %>% 
      summarise(Mean = round(sum(value * prop), 3), 
                Variance = round(sum(prop * (value - sum(value * prop))^2), 3), 
                .groups="drop")
  })
}

shinyApp(ui = ui, server = server)