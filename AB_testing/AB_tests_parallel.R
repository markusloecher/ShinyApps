library(shiny)
library(Barnard)
library(parallel)
library(ggplot2)
library(reshape2) 

# ====== CORE FUNCTIONS - same as before ======
sim_data <- function(n1, n2, p1, p2, blocked=FALSE) {
  if(blocked) {
    n1s <- ceiling(n1/2); n1m <- n1 - n1s
    n2s <- ceiling(n2/2); n2m <- n2 - n2s
    x1 <- rbinom(1, n1s, 0.01) + rbinom(1, n1m, 0.05)
    x2 <- rbinom(1, n2s, 0.01) + rbinom(1, n2m, 0.05)
  } else {
    x1 <- rbinom(1, n1, p1)
    x2 <- rbinom(1, n2, p2)
  }
  list(x1=x1, n1=n1, x2=x2, n2=n2)
}

test_fisher <- function(x1, n1, x2, n2) fisher.test(matrix(c(x1, n1-x1, x2, n2-x2),2))$p.value
test_z <- function(x1, n1, x2, n2) prop.test(c(x1,x2), c(n1,n2))$p.value
test_barnard <- function(x1, n1, x2, n2) barnard.test(x1, x2, n1, n2)$p.value

test_perm <- function(x1, n1, x2, n2, R=500) {
  data <- c(rep(1,x1), rep(0,n1-x1), rep(1,x2), rep(0,n2-x2))
  obs <- x1/n1 - x2/n2
  diffs <- replicate(R, mean(sample(data)[1:n1]) - mean(sample(data)[(n1+1):(n1+n2)]))
  mean(abs(diffs) >= abs(obs))
}

run_one <- function(i, n1, n2, p1, p2, alpha, R, blocked, do_fisher, do_z, do_barnard, do_perm) {
  d <- sim_data(n1, n2, p1, p2, blocked)
  pvals <- c()
  if(do_fisher)  pvals["fisher"]  <- test_fisher(d$x1, d$n1, d$x2, d$n2)
  if(do_z)       pvals["z"]       <- test_z(d$x1, d$n1, d$x2, d$n2)
  if(do_barnard) pvals["barnard"] <- test_barnard(d$x1, d$n1, d$x2, d$n2)
  if(do_perm)    pvals["perm"]    <- test_perm(d$x1, d$n1, d$x2, d$n2, R)
  pvals < alpha
}

run_sims <- function(nsim, cores, n1, n2, p1, p2, alpha, R, blocked, do_fisher, do_z, do_barnard, do_perm) {
  fns <- c("sim_data","test_fisher","test_z","test_barnard","test_perm","run_one")
  if(cores > 1) {
    cl <- makeCluster(cores)
    clusterExport(cl, fns, envir=environment())
    clusterEvalQ(cl, library(Barnard))
    res <- parLapply(cl, 1:nsim, run_one, n1,n2,p1,p2,alpha,R,blocked,do_fisher,do_z,do_barnard,do_perm)
    stopCluster(cl)
  } else {
    res <- lapply(1:nsim, run_one, n1,n2,p1,p2,alpha,R,blocked,do_fisher,do_z,do_barnard,do_perm)
  }
  rejects <- do.call(rbind, res)
  colMeans(rejects, na.rm=TRUE)
}

# NEW: power curve over p2
run_power_curve <- function(p2_vec,...) {
  out <- list()
  for(i in seq_along(p2_vec)) {
    cat("Running p2 =", p2_vec[i], "\n")
    tmp <- run_sims(p2 = p2_vec[i],...)
    tmp_df <- as.data.frame(as.list(tmp)) # force to data.frame
    tmp_df$p2 <- p2_vec[i] # add p2 column per row
    out[[i]] <- tmp_df
  }
  df <- do.call(rbind, out) # now rbinds data.frames, not matrix
  rownames(df) <- NULL
  df
}
# ====== END CORE FUNCTIONS ======

# ====== SHINY APP ======
ui <- fluidPage(
  titlePanel("2-Prop Tests + Power Curve"),
  tabsetPanel(
    tabPanel("Single Sim",
             sidebarLayout(
               sidebarPanel(
                 numericInput("n1", "n1 A:", 100, min=1),
                 numericInput("p1", "p1 A:", 0.1, min=0, max=1, step=0.01),
                 numericInput("n2", "n2 B:", 100, min=1),
                 numericInput("p2", "p2 B:", 0.2, min=0, max=1, step=0.01),
                 checkboxInput("blocked", "Day Confounding", FALSE),
                 numericInput("cores", "Cores:", min(4, parallel::detectCores()), min=1, max=parallel::detectCores()),
                 hr(),
                 checkboxInput("do_fisher", "Fisher", TRUE),
                 checkboxInput("do_barnard", "Barnard", FALSE),
                 checkboxInput("do_z", "Z-test", TRUE),
                 checkboxInput("do_perm", "Permutation", FALSE),
                 hr(),
                 numericInput("alpha", "Alpha:", 0.05, step=0.01),
                 numericInput("nsim", "Simulations:", 500, min=100),
                 numericInput("nperm", "Perms:", 200, min=50),
                 actionButton("run", "Run")
               ),
               mainPanel(verbatimTextOutput("out"))
             )
    ),
    tabPanel("Power Curve",
             sidebarLayout(
               sidebarPanel(
                 numericInput("pc_n1", "n1:", 100, min=1),
                 numericInput("pc_n2", "n2:", 100, min=1),
                 numericInput("pc_p1", "p1 fixed:", 0.1, min=0, max=1, step=0.01),
                 textInput("pc_p2seq", "p2 sequence:", "0.10,0.12,0.14,0.16,0.18,0.20"),
                 numericInput("pc_nsim", "Sims per point:", 500, min=100),
                 numericInput("pc_cores", "Cores:", min(4, parallel::detectCores()), min=1, max=parallel::detectCores()),
                 checkboxInput("pc_blocked", "Day Confounding", FALSE),
                 checkboxInput("pc_do_fisher", "Fisher", TRUE),
                 checkboxInput("pc_do_z", "Z-test", TRUE),
                 numericInput("pc_alpha", "Alpha:", 0.05),
                 actionButton("run_pc", "Run Power Curve")
               ),
               mainPanel(
                 plotOutput("powerPlot"),
                 verbatimTextOutput("powerTable")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  # Tab 1: Single sim
  output$out <- renderPrint({
    req(input$run)
    isolate({
      n1<-input$n1; n2<-input$n2; p1<-input$p1; p2<-input$p2
      alpha<-input$alpha; nsim<-input$nsim; R<-input$nperm
      blocked<-input$blocked; cores<-input$cores
      do_fisher<-input$do_fisher; do_z<-input$do_z
      do_barnard<-input$do_barnard; do_perm<-input$do_perm
      
      rej <- run_sims(nsim,cores,n1,n2,p1,p2,alpha,R,blocked,do_fisher,do_z,do_barnard,do_perm)
      cat("Rejection Rates at alpha =", alpha, "\n\n")
      print(round(rej, 4))
    })
  })
  
  # Tab 2: Power curve
  power_res <- eventReactive(input$run_pc, {
    isolate({
      p2_vec <- as.numeric(strsplit(input$pc_p2seq, ",")[[1]])
      withProgress(message="Running power curve...", {
        df <- run_power_curve(
          p2_vec, nsim=input$pc_nsim, cores=input$pc_cores,
          n1=input$pc_n1, n2=input$pc_n2, p1=input$pc_p1,
          alpha=input$pc_alpha, R=200, blocked=input$pc_blocked,
          do_fisher=input$pc_do_fisher, do_z=input$pc_do_z, 
          do_barnard=FALSE, do_perm=FALSE
        )
      })
      df
    })
  })
  
  output$powerPlot <- renderPlot({
    df <- power_res()
    df_long <- melt(df, id.vars="p2", variable.name="Test", value.name="Power") # melt now works
    ggplot(df_long, aes(x=p2, y=Power, color=Test, group=Test)) + # added group=
      geom_line(linewidth=1) + geom_point(size=2) +
      geom_hline(yintercept=input$pc_alpha, linetype="dashed") +
      ylim(0,1) + labs(title="Power Curve", x="p2", y="Rejection Rate")
  })
  
  output$powerTable <- renderPrint({
    df <- power_res()
    print(round(df, 4))
  })
}

shinyApp(ui, server)