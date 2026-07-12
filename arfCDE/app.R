# ============================================================
# ARF Conditional Density Estimation — Shiny App
# Converted from ARF_Conditional_Density_Estimation.Rmd
# ============================================================


library(shiny)
library(ranger)
library(arf)
library(ggpubr)
library(ggplot2)
library(data.table)

source("utils.R")   # must be in the same directory as app.R

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel("ARF Conditional Density Estimation"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Data Parameters"),
      sliderInput(
        "n_samples",
        label    = "Number of samples (n)",
        min      = 500,
        max      = 10000,
        value    = 10000,
        step     = 500
      ),
      sliderInput(
        "noiseCols",
        label    = "Noise columns",
        min      = 0,
        max      = 10,
        value    = 0,
        step     = 1
      ),
      
      hr(),
      h4("ARF Parameters"),
      sliderInput(
        "num_trees",
        label    = "Number of trees",
        min      = 50,
        max      = 500,
        value    = 500,
        step     = 50
      ),
      sliderInput(
        "min_node_size",
        label    = "Min node size",
        min      = 5,
        max      = 100,
        value    = 35,
        step     = 5
      ),
      sliderInput(
        "mtry",
        label    = "mtry",
        min      = 1,
        max      = 5,
        value    = 3,
        step     = 1
      ),
      selectInput(
        "finite_bounds",
        label   = "Finite bounds",
        choices = c("no", "local", "global"),
        selected = "local"
      ),
      
      hr(),
      actionButton(
        "run_btn",
        label = "Run Estimation",
        class = "btn-primary",
        width = "100%"
      ),
      br(),
      radioButtons("verbose", label = "Diagnostics",
                   choices  = c("Off" = "off", "Verbose" = "on"),
                   selected = "on", inline = TRUE),

      br(), br(),
      uiOutput("download_btns_ui"),

      hr(),
      h5("Global Parameters (current run):"),
      verbatimTextOutput("param_summary")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        
        tabPanel(
          "Overview",
          br(),
          p("This app benchmarks ARF (Adversarial Random Forests) against a ground-truth
             Titanic fare simulation. Each sex/class group has a distinct distribution,
             stress-testing different aspects of ARF's density estimation."),
          tableOutput("group_table"),
          br(),
          p("Press ", strong("Run Estimation"), " in the sidebar to fit the ARF and generate plots.")
        ),
        
        tabPanel(
          "Histograms",
          br(),
          conditionalPanel(
            condition = "output.has_results == 'true'",
            fluidRow(
              column(3, radioButtons("hist_yscale", label = "y-axis",
                                     choices  = c("Linear" = "linear", "sqrt" = "sqrt"),
                                     selected = "linear", inline = TRUE)),
              column(5, radioButtons("hist_xscale", label = "x-axis",
                                     choices  = c("Linear" = "linear", "sqrt" = "sqrt", "Log" = "log"),
                                     selected = "linear", inline = TRUE))
            ),
            uiOutput("hist_ui")
          ),
          conditionalPanel(
            condition = "output.has_results != 'true'",
            p("No results yet — press Run Estimation.", class = "text-muted")
          )
        ),
        
        tabPanel(
          "Likelihood Densities",
          br(),
          conditionalPanel(
            condition = "output.has_results == 'true'",
            fluidRow(
              column(3, radioButtons("lik_yscale", label = "y-axis",
                                     choices  = c("Linear" = "linear", "Sqrt" = "sqrt"),
                                     selected = "linear", inline = TRUE)),
              column(5, radioButtons("lik_xscale", label = "x-axis",
                                     choices  = c("Linear" = "linear", "sqrt" = "sqrt", "Log" = "log"),
                                     selected = "linear", inline = TRUE))
            ),
            uiOutput("lik_ui")
          ),
          conditionalPanel(
            condition = "output.has_results != 'true'",
            p("No results yet — press Run Estimation.", class = "text-muted")
          )
        ),
        
        tabPanel(
          "Log Likelihood",
          br(),
          conditionalPanel(
            condition = "output.has_results == 'true'",
            h4("Test Set Log Likelihood"),
            p("Evaluated on a held-out test sample (n = 1000, fixed seed)."),
            fluidRow(
              column(4,
                h5("Overall"),
                tableOutput("ll_summary")
              ),
              column(8,
                h5("By Sex / Pclass group"),
                tableOutput("ll_by_group")
              )
            )
          ),
          conditionalPanel(
            condition = "output.has_results != 'true'",
            p("No results yet — press Run Estimation.", class = "text-muted")
          )
        ),

        tabPanel(
          "Log",
          br(),
          verbatimTextOutput("run_log")
        )
      )
    )
  )
)

# ============================================================
# Default parameters (used as slider defaults)
# ============================================================

DEFAULTS <- list(
  n_samples     = 10000,
  num_trees     = 500L,
  min_node_size = 35,
  finite_bounds = "local",
  mtry          = 3,
  noiseCols     = 0
)

# ============================================================
# Load pre-saved default plots from disk (fast -- no simulation).
# Generate them once by running save_default_plots.R if missing.
# ============================================================

default_hist_png <- if (file.exists("default_hist.png")) "default_hist.png" else NULL
default_lik_png  <- if (file.exists("default_lik.png"))  "default_lik.png"  else NULL

if (is.null(default_hist_png))
  message("[startup] default_hist.png not found -- plots empty until Run is pressed.")
if (is.null(default_lik_png))
  message("[startup] default_lik.png not found -- plots empty until Run is pressed.")

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  # rv holds the displayed plots:
  #   p_hist / p_lik  = PNG path (startup) or ggplot object (after Run)
  #   p_is_png        = TRUE at startup, FALSE after first Run
  rv <- reactiveValues(
    p_hist      = default_hist_png,
    p_lik       = default_lik_png,
    p_is_png    = !is.null(default_hist_png),
    ll_results  = NULL,
    last_params = DEFAULTS,
    log_msgs = if (!is.null(default_hist_png))
                 paste0(format(Sys.time(), "[%H:%M:%S] "),
                        "Default PNG plots loaded from disk (n=", DEFAULTS$n_samples,
                        ", num_trees=", DEFAULTS$num_trees,
                        ", min_node_size=", DEFAULTS$min_node_size,
                        ", mtry=", DEFAULTS$mtry,
                        ", finite_bounds='", DEFAULTS$finite_bounds, "').")
               else
                 "[startup] No cached plots found -- press Run Estimation."
  )
  
  # Parameter summary (always live)
  output$param_summary <- renderText({
    paste0(
      "n_samples      = ", input$n_samples,  "\n",
      "num_trees      = ", input$num_trees,   "\n",
      "min_node_size  = ", input$min_node_size, "\n",
      "finite_bounds  = ", input$finite_bounds, "\n",
      "mtry           = ", input$mtry,         "\n",
      "noiseCols      = ", input$noiseCols
    )
  })
  
  # Static overview table
  output$group_table <- renderTable({
    data.frame(
      `Sex / Pclass` = c("female / 1", "male / 1", "female / 2",
                         "male / 2", "female / 3", "male / 3"),
      Distribution   = c("Bimodal Gaussian", "Bimodal Log-Normal",
                         "Power law (1/x²)", "Uniform",
                         "Gamma", "Spike + Gamma tail"),
      `ARF challenge` = c(
        "Valley between modes on linear scale",
        "Very difficult",
        "Extreme skew, mass near lower bound",
        "Flat density — ARF leaf Gaussians will over-smooth edges",
        "Moderate skew, unimodal — ARF's 2nd easiest case",
        "Two components at very different scales"
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Flag for conditional panels -- true as soon as any plot is available
  output$has_results <- reactive({
    if (is.null(rv$p_hist)) "false" else "true"
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  
  # ---- Shared estimation helper ----
  run_estimation <- function(n_samples, num_trees, min_node_size,
                             finite_bounds, mtry, noiseCols, log_msg) {
    log_msg("Simulating data (n = ", n_samples, ", noiseCols = ", noiseCols, ") ...")

    withProgress(message = "Running ARF estimation...", value = 0, {

      incProgress(0.1, detail = "Simulating data")
      set.seed(123)
      sim_data <<- as.data.table(
        simulate_titanic_dgp(n = n_samples, noiseCols = noiseCols, seed = NULL)
      )
      log_msg("Data simulated: ", nrow(sim_data), " rows")

      incProgress(0.2, detail = "Fitting ARF")
      log_msg("Fitting ARF (num_trees = ", num_trees,
              ", min_node_size = ", min_node_size,
              ", mtry = ", mtry,
              ", finite_bounds = '", finite_bounds, "') ...")

      res <- tryCatch(
        CondDensityEstimator(
          mtry          = mtry,
          num_trees     = as.integer(num_trees),
          min_node_size = min_node_size,
          finite_bounds = finite_bounds
        ),
        error = function(e) {
          log_msg("ERROR: ", conditionMessage(e))
          NULL
        }
      )

      incProgress(0.9, detail = "Storing results")

      if (!is.null(res)) {
        log_msg("Estimation complete.")
      } else {
        log_msg("Estimation failed — see error above.")
      }

      incProgress(1.0, detail = "Done")
      res
    })
  }

  # ---- Run button ----
  observeEvent(input$run_btn, {
    
    rv$log_msgs <- character(0)
    
    log_msg <- function(...) {
      msg <- paste0(format(Sys.time(), "[%H:%M:%S] "), ...)
      rv$log_msgs <- c(rv$log_msgs, msg)
    }

    res <- run_estimation(
      n_samples     = input$n_samples,
      num_trees     = input$num_trees,
      min_node_size = input$min_node_size,
      finite_bounds = input$finite_bounds,
      mtry          = input$mtry,
      noiseCols     = input$noiseCols,
      log_msg       = log_msg
    )
    if (!is.null(res)) {
      rv$p_hist   <- plotHists(res$hist, commonAxis = TRUE)
      rv$p_lik    <- plotLiks(res$lik,   commonAxis = TRUE)
      rv$p_is_png <- FALSE
      rv$last_params <- list(
        n_samples     = input$n_samples,
        num_trees     = input$num_trees,
        min_node_size = input$min_node_size,
        finite_bounds = input$finite_bounds,
        mtry          = input$mtry,
        noiseCols     = input$noiseCols
      )

      # ---- Test set CONDITIONAL log likelihoods: log f(Fare | sex, Pclass) ----
      log_msg("Simulating test set (n = 1000) ...")
      set.seed(999)
      test_data <- as.data.table(simulate_titanic_dgp(n = 10000, noiseCols = 0, seed = NULL))

      verbose <- input$verbose == "on"
      if (verbose) {
        cat("\n", strrep("=", 60), "\n")
        cat("VERBOSE DIAGNOSTICS — Conditional LL: log f(Fare | sex, Pclass)\n")
        cat(strrep("=", 60), "\n")
        cat("Test set size:", nrow(test_data), "(same n as training)\n\n")
      }

      true_ll_vec <- numeric(nrow(test_data))
      arf_ll_vec  <- numeric(nrow(test_data))

      for (s in c("female", "male")) {
        for (p in 1:3) {
          idx <- which(test_data$sex == s & test_data$Pclass == p)
          if (length(idx) == 0) next

          fares <- test_data$Fare[idx]

          # -- True conditional LL: log f(Fare | sex, Pclass) --
          true_vals <- log(sapply(fares, function(f) get_cond_density(f, s, as.character(p))))

          # -- ARF conditional LL: condition on sex & Pclass via evidence --
          arf_vals <- lik(
            res$psi,
            data.table(Fare = fares),
            arf      = res$arf,
            evidence = data.table(Pclass = p, sex = s),
            log      = TRUE
          )

          # Floor -Inf at log(.Machine$double.xmin) ~ -708
          ll_floor      <- log(.Machine$double.xmin)
          n_inf         <- sum(is.infinite(arf_vals) | arf_vals < ll_floor)
          arf_vals      <- pmax(arf_vals, ll_floor)

          true_ll_vec[idx] <- true_vals
          arf_ll_vec[idx]  <- arf_vals

          if (verbose) {
            cat(strrep("-", 50), "\n")
            cat(sprintf("Group: sex=%-8s  Pclass=%d   n=%d\n", s, p, length(idx)))
            cat(sprintf("  Fare range : [%.2f, %.2f]\n", min(fares), max(fares)))
            cat(sprintf("  True cLL   : mean=%.4f  sd=%.4f  min=%.4f  max=%.4f\n",
                        mean(true_vals), sd(true_vals), min(true_vals), max(true_vals)))
            cat(sprintf("  ARF  cLL   : mean=%.4f  sd=%.4f  min=%.4f  max=%.4f\n",
                        mean(arf_vals),  sd(arf_vals),  min(arf_vals),  max(arf_vals)))
            cat(sprintf("  Difference : mean=%.4f  (ARF - True)\n",
                        mean(arf_vals) - mean(true_vals)))
            if (n_inf > 0)
              cat(sprintf("  *** %d point(s) floored (ARF near-zero density) ***\n", n_inf))
            if (verbose && length(idx) <= 20) {
              # For small groups print every point
              cat("  Point-level detail:\n")
              for (k in seq_along(idx)) {
                cat(sprintf("    Fare=%7.2f  true_cLL=%8.4f  arf_cLL=%8.4f  diff=%8.4f\n",
                            fares[k], true_vals[k], arf_vals[k], arf_vals[k]-true_vals[k]))
              }
            } else if (verbose) {
              # For large groups show worst 5 ARF points, excluding floored ones
              not_floored <- which(arf_vals > ll_floor + 1)
              cat("  5 worst ARF points excl. floored (largest negative difference):\n")
              if (length(not_floored) == 0) {
                cat("    (all points at floor — nothing to show)\n")
              } else {
                worst_rel <- order(arf_vals[not_floored] - true_vals[not_floored])[1:min(5, length(not_floored))]
                worst     <- not_floored[worst_rel]
                for (k in worst) {
                  cat(sprintf("    Fare=%7.2f  true_cLL=%8.4f  arf_cLL=%8.4f  diff=%8.4f\n",
                              fares[k], true_vals[k], arf_vals[k], arf_vals[k]-true_vals[k]))
                }
              }
            }
          }
        }
      }

      if (verbose) {
        cat("\n", strrep("=", 60), "\n")
        cat(sprintf("OVERALL  true_cLL=%.4f  arf_cLL=%.4f  diff=%.4f\n",
                    mean(true_ll_vec), mean(arf_ll_vec),
                    mean(arf_ll_vec) - mean(true_ll_vec)))
        cat(strrep("=", 60), "\n\n")
      }

      # Per-group summary table
      test_data[, true_ll := true_ll_vec]
      test_data[, arf_ll  := arf_ll_vec]

      by_group <- test_data[, .(
        N          = .N,
        True_cLL   = round(mean(true_ll), 3),
        ARF_cLL    = round(mean(arf_ll),  3),
        Difference = round(mean(arf_ll) - mean(true_ll), 3),
        N_floored  = sum(arf_ll <= log(.Machine$double.xmin) + 1)
      ), by = .(sex, Pclass)][order(sex, Pclass)]

      overall <- data.frame(
        Metric = c("True cLL (mean)", "ARF cLL (mean)", "Difference (ARF - True)"),
        Value  = round(c(mean(true_ll_vec), mean(arf_ll_vec),
                         mean(arf_ll_vec) - mean(true_ll_vec)), 3)
      )

      rv$ll_results <- list(overall = overall, by_group = as.data.frame(by_group))
      log_msg("Conditional LL: True = ", overall$Value[1],
              "  ARF = ", overall$Value[2],
              "  Diff = ", overall$Value[3],
              if (verbose) "  (see console for full diagnostics)" else "")
    }
  })
  
  # ---- Dynamic UI: imageOutput (PNG) at startup, plotOutput after Run ----
  output$hist_ui <- renderUI({
    req(rv$p_hist)
    if (rv$p_is_png) imageOutput("hist_img", height = "600px")
    else             plotOutput("hist_plot", height = "600px")
  })

  output$lik_ui <- renderUI({
    req(rv$p_lik)
    if (rv$p_is_png) imageOutput("lik_img", height = "600px")
    else             plotOutput("lik_plot", height = "600px")
  })

  # ---- PNG display (startup) ----
  output$hist_img <- renderImage({
    req(rv$p_hist, rv$p_is_png)
    list(src = rv$p_hist, contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)

  output$lik_img <- renderImage({
    req(rv$p_lik, rv$p_is_png)
    list(src = rv$p_lik, contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)

  # ---- ggplot display (after Run) ----
  output$hist_plot <- renderPlot({
    req(rv$p_hist, !rv$p_is_png)
    p <- rv$p_hist
    if (input$hist_yscale == "sqrt")
      p <- p + scale_y_sqrt()
    if (input$hist_xscale == "sqrt")
      p <- p + scale_x_sqrt()
    if (input$hist_xscale == "log")
      p <- p + scale_x_log10()
    print(p)
  })

  output$lik_plot <- renderPlot({
    req(rv$p_lik, !rv$p_is_png)
    p <- rv$p_lik
    if (input$lik_yscale == "sqrt")
      p <- p + scale_y_sqrt()
    if (input$lik_xscale == "sqrt")
      p <- p + scale_x_sqrt()
    if (input$lik_xscale == "log")
      p <- p + scale_x_log10()
    print(p)
  })
  
  # ---- Download buttons: only shown after a Run ----
  output$download_btns_ui <- renderUI({
    if (rv$p_is_png || is.null(rv$p_hist)) {
      # No live ggplot yet — show disabled placeholders
      tagList(
        tags$button("Save Histograms",        class = "btn btn-default", disabled = NA, style = "width:100%; color:#aaa;"),
        br(), br(),
        tags$button("Save Likelihood Densities", class = "btn btn-default", disabled = NA, style = "width:100%; color:#aaa;")
      )
    } else {
      tagList(
        downloadButton("dl_hist", "Save Histograms",         style = "width:100%;"),
        br(), br(),
        downloadButton("dl_lik",  "Save Likelihood Densities", style = "width:100%;")
      )
    }
  })

  # Helper: build a filename from current parameter values
  plot_filename <- function(prefix) {
    paste0(prefix,
           "_n",    rv$last_params$n_samples,
           "_tr",   rv$last_params$num_trees,
           "_mn",   rv$last_params$min_node_size,
           "_mt",   rv$last_params$mtry,
           "_fb",   rv$last_params$finite_bounds,
           "_nc",   rv$last_params$noiseCols,
           ".png")
  }

  output$dl_hist <- downloadHandler(
    filename = function() plot_filename("hist"),
    content  = function(file) {
      ggsave(file, plot = rv$p_hist, width = 12, height = 8, dpi = 150)
    }
  )

  output$dl_lik <- downloadHandler(
    filename = function() plot_filename("lik"),
    content  = function(file) {
      ggsave(file, plot = rv$p_lik, width = 12, height = 8, dpi = 150)
    }
  )

  # ---- Log Likelihood tables ----
  output$ll_summary <- renderTable({
    req(rv$ll_results)
    rv$ll_results$overall
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$ll_by_group <- renderTable({
    req(rv$ll_results)
    rv$ll_results$by_group
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ---- Log output ----
  output$run_log <- renderText({
    if (length(rv$log_msgs) == 0) {
      "Press 'Run Estimation' to start."
    } else {
      paste(rv$log_msgs, collapse = "\n")
    }
  })
}

# ============================================================
# Launch
# ============================================================

shinyApp(ui = ui, server = server)
