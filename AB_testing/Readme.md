# 2-Prop Test Simulator

A Shiny app to compare Type I error and Power of different 2-proportion tests under realistic A/B conditions. Built because the "textbook" answer isn't always the best answer when data is messy.

**Live demo**: [https://treeml.shinyapps.io/binomial_AB_testing/]

## Why this app?

In real A/B tests we rarely have perfect binomial data. Day-of-week effects, low rates, and small samples break assumptions.

This app lets you simulate and compare 4 common 2-prop tests head-to-head:

1.  **Fisher Exact**: Conservative, exact. Gold standard for small n.
2.  **Z-test / prop.test**: Fast, asymptotic. What most dashboards use by default.
3.  **Barnard's Test**: More powerful than Fisher for 2x2 tables, but slower.
4.  **Permutation Test**: Non-parametric. Robust to weird distributions.

Plus: simulate **confounding**. E.g. 50% traffic on Sunday @ 1% CVR, 50% on Monday @ 5% CVR. This breaks naive tests.

## Features

- **Single Simulation Tab**: Pick n1, n2, p1, p2. Run N simulations. Get rejection rate at your alpha. Use parallel cores for speed.
- **Power Curve Tab**: Sweep p2 across a range to see how power changes with effect size. Plot all tests on one graph.
- **Blocked Simulation**: Model day-of-week / traffic source confounding to see inflated Type I error.
- **Modular R code**: Core functions `sim_data`, `run_one`, `run_sims` can be tested outside Shiny.

## Quick Start

```r
# 1. Install deps
install.packages(c("shiny","Barnard","ggplot2","reshape2"))

# 2. Run
Rscript app.R
