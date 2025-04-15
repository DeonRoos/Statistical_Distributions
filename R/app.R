# Library Loading -------------------------------------------------------------
library(shiny)
library(ggplot2)
library(tweedie)
library(shinyWidgets)
library(LaplacesDemon)
library(extraDistr)
library(shinydashboard)
library(ggfortify)
library(patchwork)

# Default Parameter Values -------------------------------------------------------------
default_params <- list(
  Normal = list(mean = 0, sd = 1),
  Poisson = list(lambda = 3),
  Uniform = list(min = -3, max = 3),
  Beta = list(alpha = 3, beta = 2),
  `Student's T` = list(df = 2),
  Exponential = list(rate = 2),
  Gamma = list(shape = 2, rate = 1),
  `Log-Normal` = list(meanlog = 0, sdlog = 1),
  `Half-Cauchy` = list(scale = 1),
  Tweedie = list(p = 3, mu = 3, phi = 1),
  Wald = list(mu = 3, lambda = 3),
  Binomial = list(size = 10, prob = 0.7),
  Bernoulli = list(p = 0.5),
  ZIP = list(lambda = 1, pi = 0.2),
  `Negative Binomial` = list(size = 1, mu = 3)
)

# Default X-Axis Interval -------------------------------------------------------------
range_defaults <- list(
  Normal = list(xmin = -10, xmax = 10),
  Uniform = list(xmin = -5, xmax = 5),
  Poisson = list(xmin = 0, xmax = 10),
  Beta = list(xmin = 0, xmax = 1),
  `Student's T` = list(xmin = -10, xmax = 10),
  Binomial = list(xmin = 0, xmax = 10),
  Exponential = list(xmin = 0, xmax = 10),
  Gamma = list(xmin = 0, xmax = 10),
  `Log-Normal` = list(xmin = 0, xmax = 10),
  `Half-Cauchy` = list(xmin = 0, xmax = 10),
  Tweedie = list(xmin = 0, xmax = 10),
  Wald = list(xmin = 0, xmax = 10),
  ZIP = list(xmin = 0, xmax = 10),
  Bernoulli = list(xmin = -0.1, xmax = 1.1),
  `Negative Binomial` = list(xmin = 0, xmax = 10)
)

# Discrete Distributions List -------------------------------------------------------------
discrete_dists <- c("Poisson", "Binomial", "Bernoulli", "ZIP", "Negative Binomial")

# Density Functions Mapping -------------------------------------------------------------
dens_funcs <- list(
  Normal = function(x, p) dnorm(x, mean = p$mean, sd = p$sd),
  Poisson = function(x, p) dpois(as.integer(x), lambda = p$lambda),
  Uniform = function(x, p) dunif(x, min = p$min, max = p$max),
  Beta = function(x, p) dbeta(x, shape1 = p$alpha, shape2 = p$beta),
  `Student's T` = function(x, p) dt(x, df = p$df),
  Exponential = function(x, p) dexp(x, rate = p$rate),
  Gamma = function(x, p) dgamma(x, shape = p$shape, rate = p$rate),
  `Log-Normal` = function(x, p) dlnorm(x, meanlog = p$meanlog, sdlog = p$sdlog),
  `Half-Cauchy` = function(x, p) dhalfcauchy(x, scale = p$scale),
  Tweedie = function(x, p) dtweedie(x, p = p$p, mu = p$mu, phi = p$phi),
  Wald = function(x, p) dwald(x, mu = p$mu, lambda = p$lambda),
  Binomial = function(x, p) dbinom(as.integer(x), size = p$size, prob = p$prob),
  Bernoulli = function(x, p) dbern(as.integer(x), p = p$p),
  ZIP = function(x, p) dzip(as.integer(x), lambda = p$lambda, pi = p$pi),
  `Negative Binomial` = function(x, p) dnbinom(as.integer(x), size = p$size, mu = p$mu)
)

# RNG Functions Mapping -------------------------------------------------------------
rand_funcs <- list(
  Normal = function(n, p) rnorm(n, mean = p$mean, sd = p$sd),
  Poisson = function(n, p) rpois(n, lambda = p$lambda),
  Uniform = function(n, p) runif(n, min = p$min, max = p$max),
  Beta = function(n, p) rbeta(n, shape1 = p$alpha, shape2 = p$beta),
  `Student's T` = function(n, p) rt(n, df = p$df),
  Exponential = function(n, p) rexp(n, rate = p$rate),
  Gamma = function(n, p) rgamma(n, shape = p$shape, rate = p$rate),
  `Log-Normal` = function(n, p) rlnorm(n, meanlog = p$meanlog, sdlog = p$sdlog),
  `Half-Cauchy` = function(n, p) rhalfcauchy(n, scale = p$scale),
  Tweedie = function(n, p) rtweedie(n, p = p$p, mu = p$mu, phi = p$phi),
  Wald = function(n, p) rwald(n, mu = p$mu, lambda = p$lambda),
  Binomial = function(n, p) rbinom(n, size = p$size, prob = p$prob),
  Bernoulli = function(n, p) rbern(n, p = p$p),
  ZIP = function(n, p) rzip(n, lambda = p$lambda, pi = p$pi),
  `Negative Binomial` = function(n, p) rnbinom(n, size = p$size, mu = p$mu)
)

# getParams Function -------------------------------------------------------------
getParams <- function(dist, i, input) {
  switch(dist,
         Normal = list(
           mean = input[[paste0("mean_", i)]],
           sd = input[[paste0("sd_", i)]]
         ),
         Poisson = list(lambda = input[[paste0("lambda_", i)]]),
         Uniform = list(
           min = input[[paste0("min_", i)]],
           max = input[[paste0("max_", i)]]
         ),
         Beta = list(
           alpha = input[[paste0("alpha_", i)]],
           beta = input[[paste0("beta_", i)]]
         ),
         `Student's T` = list(df = input[[paste0("df_", i)]]),
         Exponential = list(rate = input[[paste0("rate_", i)]]),
         Gamma = list(
           shape = input[[paste0("shape_", i)]],
           rate = input[[paste0("rate_", i)]]
         ),
         `Log-Normal` = list(
           meanlog = input[[paste0("meanlog_", i)]],
           sdlog = input[[paste0("sdlog_", i)]]
         ),
         `Half-Cauchy` = list(scale = input[[paste0("scale_", i)]]),
         Tweedie = list(
           p = input[[paste0("p_", i)]],
           mu = input[[paste0("mu_", i)]],
           phi = input[[paste0("phi_", i)]]
         ),
         Wald = list(
           mu = input[[paste0("mu_", i)]],
           lambda = input[[paste0("lambda_", i)]]
         ),
         Binomial = list(
           size = input[[paste0("size_", i)]],
           prob = input[[paste0("prob_", i)]]
         ),
         Bernoulli = list(p = input[[paste0("p_", i)]]),
         ZIP = list(
           lambda = input[[paste0("lambda_", i)]],
           pi = input[[paste0("pi_", i)]]
         ),
         `Negative Binomial` = list(
           size = input[[paste0("size_", i)]],
           mu = input[[paste0("mu_", i)]]
         )
  )
}

# paramInputsForDist Function -------------------------------------------------------------
paramInputsForDist <- function(dist, i, defaults) {
  switch(dist,
         Normal = tagList(
           numericInput(paste0("mean_", i),
                        label = tags$span("Mean (μ):", title = "Average value of the distribution"),
                        value = defaults$mean, step = 0.1
           ),
           numericInput(paste0("sd_", i),
                        label = tags$span("Standard Deviation (σ):", title = "Measure of spread about the mean"),
                        value = defaults$sd, step = 0.1
           )
         ),
         Poisson = tagList(
           numericInput(paste0("lambda_", i),
                        label = tags$span("Lambda (λ):", title = "Average rate of events"),
                        value = defaults$lambda, step = 1
           )
         ),
         Uniform = tagList(
           numericInput(paste0("min_", i),
                        label = tags$span("Minimum (a):", title = "Smallest possible value in the range"),
                        value = defaults$min, step = 0.1
           ),
           numericInput(paste0("max_", i),
                        label = tags$span("Maximum (b):", title = "Largest possible value in the range"),
                        value = defaults$max, step = 0.1
           )
         ),
         Beta = tagList(
           numericInput(paste0("alpha_", i),
                        label = tags$span("Shape α:", title = "Controls the left side of the distribution"),
                        value = defaults$alpha, step = 0.1
           ),
           numericInput(paste0("beta_", i),
                        label = tags$span("Shape β:", title = "Controls the right side of the distribution"),
                        value = defaults$beta, step = 0.1
           )
         ),
         `Student's T` = tagList(
           numericInput(paste0("df_", i),
                        label = tags$span("Degrees of Freedom (ν):", title = "Lower degrees imply heavier tails"),
                        value = defaults$df, step = 1
           )
         ),
         Exponential = tagList(
           numericInput(paste0("rate_", i),
                        label = tags$span("Rate (λ):", title = "Average number of events per time unit (for waiting times)"),
                        value = defaults$rate, step = 0.1
           )
         ),
         Gamma = tagList(
           numericInput(paste0("shape_", i),
                        label = tags$span("Shape (α):", title = "Controls the form of the distribution"),
                        value = defaults$shape, step = 0.1
           ),
           numericInput(paste0("rate_", i),
                        label = tags$span("Rate (λ):", title = "Inversely related to the scale; controls decay"),
                        value = defaults$rate, step = 0.1
           )
         ),
         `Log-Normal` = tagList(
           numericInput(paste0("meanlog_", i),
                        label = tags$span("Log-mean (μ):", title = "Mean of the logarithm of the data"),
                        value = defaults$meanlog, step = 0.1
           ),
           numericInput(paste0("sdlog_", i),
                        label = tags$span("Log-standard Deviation (σ):", title = "Spread of the logarithm of the data"),
                        value = defaults$sdlog, step = 0.1
           )
         ),
         `Half-Cauchy` = tagList(
           numericInput(paste0("scale_", i),
                        label = tags$span("Scale (α):", title = "Determines the spread of the distribution"),
                        value = defaults$scale, step = 0.1
           )
         ),
         Tweedie = tagList(
           numericInput(paste0("p_", i),
                        label = tags$span("Power (p):", title = "Determines the type of Tweedie distribution"),
                        value = defaults$p, step = 0.1
           ),
           numericInput(paste0("mu_", i),
                        label = tags$span("Mean (μ):", title = "Average value of the distribution"),
                        value = defaults$mu, step = 0.1
           ),
           numericInput(paste0("phi_", i),
                        label = tags$span("Dispersion (φ):", title = "Variability of the data beyond the mean"),
                        value = defaults$phi, step = 0.1
           )
         ),
         Wald = tagList(
           numericInput(paste0("mu_", i),
                        label = tags$span("Mean (μ):", title = "Central tendency of the data"),
                        value = defaults$mu, step = 0.1
           ),
           numericInput(paste0("lambda_", i),
                        label = tags$span("Lambda (λ):", title = "Shape parameter controlling tail behavior"),
                        value = defaults$lambda, step = 0.1
           )
         ),
         Binomial = tagList(
           numericInput(paste0("size_", i),
                        label = tags$span("Number of Trials (n):", title = "Total number of independent experiments"),
                        value = defaults$size, step = 1
           ),
           numericInput(paste0("prob_", i),
                        label = tags$span("Success Probability (p):", title = "Probability of success on a single trial"),
                        value = defaults$prob, min = 0, max = 1, step = 0.1
           )
         ),
         Bernoulli = tagList(
           numericInput(paste0("p_", i),
                        label = tags$span("Probability (p):", title = "Probability of success in the trial"),
                        value = defaults$p, min = 0, max = 1, step = 0.1
           )
         ),
         ZIP = tagList(
           numericInput(paste0("lambda_", i),
                        label = tags$span("Lambda (λ):", title = "Average rate for the Poisson component"),
                        value = defaults$lambda, step = 0.1
           ),
           numericInput(paste0("pi_", i),
                        label = tags$span("Zero Inflation Probability (π):", title = "Probability that the value is an extra zero"),
                        value = defaults$pi, min = 0, max = 1, step = 0.1
           )
         ),
         `Negative Binomial` = tagList(
           numericInput(paste0("size_", i),
                        label = tags$span("Dispersion Parameter (n):", title = "Controls overdispersion; higher values yield less variance"),
                        value = defaults$size, min = 0, step = 1
           ),
           numericInput(paste0("mu_", i),
                        label = tags$span("Mean (μ):", title = "Average count value"),
                        value = defaults$mu, min = 0, step = 0.1
           )
         )
  )
}

# UI Definition -------------------------------------------------------------
ui <- navbarPage(
  title = "Explore Statistical Distributions",
  tags$head(
    tags$style(HTML("
      body { background-color: #202123 !important; color: white; }
      .mainPanel { background-color: #202123 !important; }
      h4, h6, p, .form-control-static, .text-output { color: white; }
      .navbar .navbar-brand { font-size: 24px; color: #FFFFFF !important; }
      .navbar { background-color: #444654; color: #00A68A; font-weight: bold; border: 2px solid #00A68A; box-shadow: 0 0 10px 5px rgba(0,166,138,0.3); font-size: 20px; }
      .navbar .nav > li > a:hover,
      .navbar .nav > li > a:focus { background-color: #00A68A; color: #FFFFFF !important; border: 2px solid #00A68A; box-shadow: 0 0 10px 5px rgba(0,166,138,0.3); }
      .navbar .nav .active > a,
      .navbar .nav .active > a:hover,
      .navbar .nav .active > a:focus { background-color: #00A68A; color: #FFFFFF !important; border: 2px solid #00A68A; box-shadow: 0 0 10px 5px rgba(0,166,138,0.3); }
    "))
  ),
  tabPanel("Explore",
           icon = icon("search-plus"),
           sidebarLayout(
             sidebarPanel(
               style = "background: linear-gradient(#444654, #3F3D39); border-radius: 10px; border: 2px solid #00A68A; box-shadow: 0 0 10px 5px rgba(0,166,138,0.3);",
               pickerInput(
                 inputId = "distribution",
                 label = tags$span("Select distribution:", title = "Choose which statistical distribution to explore"),
                 choices = list(
                   "Continuous real" = c("Normal", sort(c("Uniform", "Beta", "Student's T", "Half-Cauchy"))),
                   "Continuous positive" = c("Exponential", "Tweedie", "Gamma", "Log-Normal", "Wald"),
                   "Discrete" = sort(c("Poisson", "Binomial", "Bernoulli", "ZIP", "Negative Binomial"))
                 ),
                 options = list(`live-search` = TRUE)
               ),
               tags$hr(style = "border-color: #00A68A;"),
               h4("X-axis interval for visualization"),
               numericInput("xmin",
                            label = tags$span("Minimum x-value:", title = "Specify the smallest x-value for the plots"),
                            value = range_defaults[["Normal"]]$xmin, step = 0.5
               ),
               numericInput("xmax",
                            label = tags$span("Maximum x-value:", title = "Specify the largest x-value for the plots"),
                            value = range_defaults[["Normal"]]$xmax, step = 0.5
               ),
               numericInput("nobs",
                            label = tags$span("Number of random values:", title = "Enter how many random samples to generate"),
                            value = 100, min = 1, max = 5000, step = 25
               ),
               tags$hr(style = "border-color: #00A68A;"),
               h4("Explore different parameter values"),
               fluidRow(
                 column(6, actionButton("addSet", "Add Parameter Set", title = "Add a new set of parameters")),
                 column(6, actionButton("removeSet", "Remove Parameter Set", title = "Remove the most recently added set"))
               ),
               br(),
               uiOutput("setCountDisplay"),
               tags$hr(style = "border-color: #00A68A;"),
               h4(tags$span("Distribution specific parameters", title = "Change distribution parameters")),
               uiOutput("distributionParams"),
               actionButton("resetDefaults", "Reset to Defaults", title = "Revert all parameters to defaults")
             ),
             mainPanel(
               fluidRow(
                 column(
                   6,
                   div(id = "myTextOutput", textOutput("distName")),
                   plotOutput("densityPlot"),
                   h6("Fig. 1: The PDF plotted using the actual density values."),
                   br(),
                   plotOutput("randomPlot"),
                   h6("Fig. 2: Histogram of randomly generated values.")
                 ),
                 column(
                   6,
                   br(), br(),
                   uiOutput("distributionText"),
                   br(),
                   h6("Notation:"),
                   uiOutput("distributionNote"),
                   h6("PDF/PMF:"),
                   uiOutput("distributionPDF"),
                   br(),
                   tags$hr(style = "border-color: #00A68A;"),
                   h4("Examples from this distribution"),
                   textOutput("distributionEG"),
                   br(),
                   tags$hr(style = "border-color: #00A68A;"),
                   p("This app was created by Deon Roos. The code is available on GitHub:"),
                   tags$a(href = "https://github.com/DeonRoos/Statistical_Distributions", "GitHub Repo")
                 )
               )
             )
           )
  ),
  tabPanel("Compare",
           icon = icon("line-chart"),
           sidebarLayout(
             sidebarPanel(
               style = "background: linear-gradient(#444654, #3F3D39); border-radius: 10px; border: 2px solid #00A68A; box-shadow: 0 0 10px 5px rgba(0,166,138,0.3);",
               pickerInput(
                 inputId = "selectedDistributions",
                 label = tags$span("Select multiple distributions:", title = "Choose distributions for comparison"),
                 choices = list(
                   "Continuous real" = c("Normal", sort(c("Uniform", "Beta", "Student's T", "Half-Cauchy"))),
                   "Continuous positive" = c("Exponential", "Tweedie", "Gamma", "Log-Normal", "Wald"),
                   "Discrete" = sort(c("Poisson", "Binomial", "Bernoulli", "ZIP", "Negative Binomial"))
                 ),
                 multiple = TRUE,
                 selected = c("Normal", "Poisson"),
                 options = list(`live-search` = TRUE)
               ),
               h4("X-axis interval for visualization"),
               numericInput("xminmulti",
                            label = tags$span("Minimum x-value:", title = "Smallest x-value for comparison plots"),
                            value = -10, step = 0.5
               ),
               numericInput("xmaxmulti",
                            label = tags$span("Maximum x-value:", title = "Largest x-value for comparison plots"),
                            value = 10, step = 0.5
               ),
               uiOutput("paramInputs")
             ),
             mainPanel(
               h4("Multi-distribution Comparison Plot"),
               plotOutput("densityPlotMulti"),
               h6("Densities are plotted with actual values (peak = 1 is not enforced)."),
               tags$hr(style = "border-color: #00A68A;"),
               p("This app was created by Deon Roos. The code is available on GitHub:"),
               tags$a(href = "https://github.com/DeonRoos/Statistical_Distributions", "GitHub Repo")
             )
           )
  ),
  tabPanel("Modelling",
           icon = icon("flask"),
           sidebarLayout(
             sidebarPanel(
               style = "background: linear-gradient(#444654, #3F3D39); border-radius: 10px; border: 2px solid #00A68A; box-shadow: 0 0 10px 5px rgba(0,166,138,0.3);",
               # Simulation Distribution Selector -------------------------------------------------------------
               selectInput("sim_dist", "Simulation Distribution:",
                           choices = c("Normal", "Poisson", "Binomial", "Bernoulli"),
                           selected = "Normal"
               ),
               # Conditional: Number of Trials for Binomial -------------------------------------------------------------
               conditionalPanel(
                 condition = "input.sim_dist == 'Binomial'",
                 numericInput("sim_trials", "Number of trials:", value = 10, min = 1)
               ),
               numericInput("sim_N", "Sample size (N):", value = 100, min = 10),
               conditionalPanel(
                 condition = "input.sim_dist == 'Normal'",
                 numericInput("sim_sd", "Error standard deviation:", value = 1, min = 0, step = 0.5)
               ),
               h4("Data Generating Process"),
               uiOutput("dgp_eq"),
               tags$hr(style = "border-color: #00A68A;"),
               # Intercept Input -------------------------------------------------------------
               numericInput("sim_intercept", "Intercept:", value = 0, step = 0.5),
               # Continuous Predictor -------------------------------------------------------------
               checkboxInput("sim_use_x", "Include continuous predictor (x)", value = TRUE),
               conditionalPanel(
                 condition = "input.sim_use_x == true",
                 tagList(
                   conditionalPanel(
                     condition = "input.nonlinear_x == false",
                     numericInput("sim_slope", "Slope:", value = 1, step = 0.1)
                   ),
                   checkboxInput("nonlinear_x", "Make x non-linear", value = FALSE)
                 )
               ),
               # Categorical Predictor -------------------------------------------------------------
               checkboxInput("sim_use_grp", "Include categorical predictor (grp)", value = FALSE),
               conditionalPanel(
                 condition = "input.sim_use_grp == true",
                 selectInput("sim_grps", "Number of groups:", choices = c(2, 3), selected = 2),
                 numericInput("sim_grpB", "Effect for Group B (reference = Group A):", value = 1, step = 0.5),
                 conditionalPanel(
                   condition = "input.sim_grps == 3",
                   numericInput("sim_grpC", "Effect for Group C:", value = 2, step = 0.5)
                 )
               ),
               # Unobserved Effects -------------------------------------------------------------
               checkboxInput("sim_unobs_linear", "Include unobserved linear effect", value = FALSE),
               checkboxInput("sim_unobs_cat", "Include unobserved categorical effect", value = FALSE),
               checkboxInput("sim_unobs_nonlinear", "Include unobserved non-linear effect", value = FALSE),
               # Model Family for Fitting -------------------------------------------------------------
               selectInput("mod_family", "Model Family for Fitting:",
                           choices = c("Normal", "Poisson", "Binomial", "Bernoulli"),
                           selected = "Normal"
               ),
               h4("Model Equation"),
               uiOutput("model_eq")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Predicted Relationship", plotOutput("predPlot")),
                 tabPanel("Forest Plot", plotOutput("forestPlot")),
                 tabPanel("Parameter Comparison",
                          tags$style(HTML("
              .dataTables_wrapper .dataTables_info,
              .dataTables_wrapper .dataTables_length label,
              .dataTables_wrapper .dataTables_filter label,
              .dataTables_wrapper .dataTables_paginate,
              .dataTables_wrapper .dataTables_paginate .paginate_button {
                color: white !important;
              }
              table.dataTable thead .sorting,
              table.dataTable thead .sorting_asc,
              table.dataTable thead .sorting_desc {
                color: white !important;
              }
              table.dataTable td,
              table.dataTable th {
                border-color: #444654 !important;
                color: white !important;
              }
              .dataTables_wrapper .dataTables_paginate .paginate_button {
                background-color: #444654 !important;
                color: white !important;
              }
            ")),
            DT::dataTableOutput("paramSummary")
                 ),
            tabPanel("Diagnostics", plotOutput("diagnosticsPlot")),
            tabPanel("Raw Data Figures", plotOutput("rawFigures", width = "100%", height = "700px")),
            tabPanel("Raw Data", DT::dataTableOutput("rawData"))
               )
             )
           )
  ),
  tabPanel("About",
           icon = icon("info-circle"),
           h2("App Guidance"),
           h4("This app helps you visualize and understand a selection of statistical distributions. Adjust parameters to see how the probability density/mass functions change and to generate random data."),
           h4("Use the 'Compare' tab to view multiple distributions at once."),
           tags$hr(style = "border-color: #00A68A;"),
           h2("Caveats"),
           h4("Only a limited set of distributions is available and some formulas use simplified notation."),
           tags$hr(style = "border-color: #00A68A;"),
           p("This app was created by Deon Roos. The code is available on GitHub:"),
           tags$a(href = "https://github.com/DeonRoos/Statistical_Distributions", "GitHub Repo")
  )
)

# Server Logic -------------------------------------------------------------
server <- function(input, output, session) {
  
  # Parameter Set Counter -------------------------------------------------------------
  parameterSetCount <- reactiveVal(1)
  
  observeEvent(input$addSet, {
    if (parameterSetCount() < 10) {
      parameterSetCount(parameterSetCount() + 1)
    }
  })
  
  observeEvent(input$removeSet, {
    if (parameterSetCount() > 1) {
      parameterSetCount(parameterSetCount() - 1)
    }
  })
  
  output$setCountDisplay <- renderUI({
    tags$p(strong("Current number of Parameter Sets:"), parameterSetCount())
  })
  
  # Explore: Distribution Parameter Inputs -------------------------------------------------------------
  output$distributionParams <- renderUI({
    dist <- input$distribution
    defaults <- default_params[[dist]]
    count <- parameterSetCount()
    tagList(lapply(seq_len(count), function(i) {
      tagList(
        h4(paste("Parameter Set", i)),
        paramInputsForDist(dist, i, defaults)
      )
    }))
  })
  
  # Explore: Density Plot -------------------------------------------------------------
  output$densityPlot <- renderPlot({
    dist <- input$distribution
    xmin <- input$xmin
    xmax <- input$xmax
    count <- parameterSetCount()
    all_dfs <- lapply(seq_len(count), function(i) {
      p <- getParams(dist, i, input)
      x <- if (dist %in% discrete_dists) {
        seq(ceiling(xmin), floor(xmax), length.out = 100)
      } else {
        seq(xmin, xmax, length.out = 100)
      }
      dens <- if (dist %in% discrete_dists) {
        dens_funcs[[dist]](as.integer(x), p)
      } else {
        dens_funcs[[dist]](x, p)
      }
      data.frame(x = x, density = dens, Set = factor(i))
    })
    df <- do.call(rbind, all_dfs)
    ggplot(df, aes(x = x, y = density, color = Set, group = Set)) +
      geom_line(size = 1) +
      scale_x_continuous(limits = c(xmin, xmax)) +
      xlab("Possible values (x)") +
      ylab("Density") +
      scale_color_brewer(palette = "Set2") +
      theme_minimal() +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#202123"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 14),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 10),
        panel.grid.major = element_line(color = "#444654"),
        panel.grid.minor = element_line(color = "#444654")
      )
  })
  
  # Explore: Distribution Name -------------------------------------------------------------
  output$distName <- renderText({
    paste("The", input$distribution, "distribution")
  })
  
  # Explore: Random Values Histogram -------------------------------------------------------------
  output$randomPlot <- renderPlot({
    dist <- input$distribution
    nobs <- input$nobs
    xmin <- input$xmin
    xmax <- input$xmax
    count <- parameterSetCount()
    all_samples <- lapply(seq_len(count), function(i) {
      p <- getParams(dist, i, input)
      rand_funcs[[dist]](nobs, p)
    })
    samples <- unlist(all_samples)
    comb <- rep(seq_len(count), each = nobs)
    df <- data.frame(value = samples, Set = factor(comb))
    ggplot(df, aes(x = value, fill = Set, color = Set)) +
      geom_histogram(position = "dodge", binwidth = if (dist %in% discrete_dists) 1 else NULL, alpha = 0.3) +
      scale_x_continuous(limits = c(xmin, xmax)) +
      xlab("Realised values (x)") +
      ylab("Frequency") +
      scale_color_brewer(palette = "Set2") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#202123"),
        plot.title = element_text(color = "white", size = 16),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 14),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 10),
        panel.grid.major = element_line(color = "#444654"),
        panel.grid.minor = element_line(color = "#444654")
      )
  })
  
  # Explore: Update X-Axis Interval on Distribution Change -------------------------------------------------------------
  observeEvent(input$distribution, {
    dist <- input$distribution
    rd <- range_defaults[[dist]]
    updateNumericInput(session, "xmin", value = rd$xmin)
    updateNumericInput(session, "xmax", value = rd$xmax)
  })
  
  # Explore: Reset Default Parameters -------------------------------------------------------------
  observeEvent(input$resetDefaults, {
    dist <- input$distribution
    defaults <- default_params[[dist]]
    rd <- range_defaults[[dist]]
    updateNumericInput(session, "xmin", value = rd$xmin)
    updateNumericInput(session, "xmax", value = rd$xmax)
    count <- parameterSetCount()
    for (i in seq_len(count)) {
      switch(dist,
             Normal = {
               updateNumericInput(session, paste0("mean_", i), value = defaults$mean)
               updateNumericInput(session, paste0("sd_", i), value = defaults$sd)
             },
             Poisson = {
               updateNumericInput(session, paste0("lambda_", i), value = defaults$lambda)
             },
             Uniform = {
               updateNumericInput(session, paste0("min_", i), value = defaults$min)
               updateNumericInput(session, paste0("max_", i), value = defaults$max)
             },
             Beta = {
               updateNumericInput(session, paste0("alpha_", i), value = defaults$alpha)
               updateNumericInput(session, paste0("beta_", i), value = defaults$beta)
             },
             `Student's T` = {
               updateNumericInput(session, paste0("df_", i), value = defaults$df)
             },
             Exponential = {
               updateNumericInput(session, paste0("rate_", i), value = defaults$rate)
             },
             Gamma = {
               updateNumericInput(session, paste0("shape_", i), value = defaults$shape)
               updateNumericInput(session, paste0("rate_", i), value = defaults$rate)
             },
             `Log-Normal` = {
               updateNumericInput(session, paste0("meanlog_", i), value = defaults$meanlog)
               updateNumericInput(session, paste0("sdlog_", i), value = defaults$sdlog)
             },
             `Half-Cauchy` = {
               updateNumericInput(session, paste0("scale_", i), value = defaults$scale)
             },
             Tweedie = {
               updateNumericInput(session, paste0("p_", i), value = defaults$p)
               updateNumericInput(session, paste0("mu_", i), value = defaults$mu)
               updateNumericInput(session, paste0("phi_", i), value = defaults$phi)
             },
             Wald = {
               updateNumericInput(session, paste0("mu_", i), value = defaults$mu)
               updateNumericInput(session, paste0("lambda_", i), value = defaults$lambda)
             },
             Binomial = {
               updateNumericInput(session, paste0("size_", i), value = defaults$size)
               updateNumericInput(session, paste0("prob_", i), value = defaults$prob)
             },
             Bernoulli = {
               updateNumericInput(session, paste0("p_", i), value = defaults$p)
             },
             ZIP = {
               updateNumericInput(session, paste0("lambda_", i), value = defaults$lambda)
               updateNumericInput(session, paste0("pi_", i), value = defaults$pi)
             },
             `Negative Binomial` = {
               updateNumericInput(session, paste0("size_", i), value = defaults$size)
               updateNumericInput(session, paste0("mu_", i), value = defaults$mu)
             }
      )
    }
  })
  
  # Explore: Distribution Description -------------------------------------------------------------
  output$distributionText <- renderUI({
    distribution <- input$distribution
    summary_text <- switch(distribution,
                           "Normal" = HTML("
        <b>The Normal Distribution (Gaussian):</b><br>
        The Normal distribution is one of the most fundamental probability distributions in statistics. It is defined by a mathematical formula that approximates a wide variety of datasets—ranging from human traits (like height and weight) to test scores. Unlike a uniform distribution, which treats all outcomes equally, the Normal follows a distinct “bell curve” pattern.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Bell-Shaped and Symmetric:</b> Values cluster around the mean with a symmetric decline on either side.</li>
          <li><b>Standard Deviation Controls Spread:</b> A smaller standard deviation yields a narrow, steep curve; a larger one, a wider, flatter curve.</li>
          <li><b>Range:</b> It covers all real numbers, from negative infinity to positive infinity.</li>
        </ul>
        <b>When to Consider:</b> For continuous data that cluster around a central value and extend in both directions.
      "),
      "Poisson" = HTML("
        <b>The Poisson Distribution:</b><br>
        The Poisson distribution models the number of times an event occurs in a fixed interval when events occur independently at a constant rate.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Discrete Count Data:</b> Outputs non-negative integers.</li>
          <li><b>Mean Equals Variance:</b> A unique property where the average rate equals the variance.</li>
          <li><b>Unbounded Above:</b> Although counts start at zero, there is no theoretical upper limit.</li>
        </ul>
        <b>When to Consider:</b> For modeling counts of rare events, like customer arrivals or emails per hour.
      "),
      "Uniform" = HTML("
        <b>The Uniform Distribution:</b><br>
        The Uniform distribution gives all outcomes within a specified interval equal probability, resulting in a flat, rectangular shape.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Equal Likelihood:</b> Every value within the range is equally likely.</li>
          <li><b>Constant Density:</b> The probability density remains unchanged across the interval.</li>
          <li><b>Defined Interval:</b> Only values between two endpoints are possible.</li>
        </ul>
        <b>When to Consider:</b> When there is no reason to favor one outcome over another within a fixed range.
      "),
      "Beta" = HTML("
        <b>The Beta Distribution:</b><br>
        The Beta distribution is defined on the interval [0, 1] and is ideal for modeling proportions or probabilities.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Bounded:</b> All outcomes lie between 0 and 1.</li>
          <li><b>Flexible Shape:</b> Two parameters allow the distribution to take on many shapes (e.g., U-shaped, uniform, bell-shaped).</li>
        </ul>
        <b>When to Consider:</b> Ideal for modeling conversion rates, proportions, or any metric naturally restricted to [0, 1].
      "),
      "Student's T" = HTML("
        <b>The Student's T Distribution:</b><br>
        The Student's T distribution resembles the Normal distribution but with heavier tails, making it valuable when sample sizes are small or when the population variance is uncertain.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavy Tails:</b> More likelihood for extreme values than the Normal distribution.</li>
          <li><b>Degrees of Freedom:</b> As sample size increases, it converges to the Normal distribution.</li>
        </ul>
        <b>When to Consider:</b> For small samples or when the population variance is unknown.
      "),
      "Binomial" = HTML("
        <b>The Binomial Distribution:</b><br>
        The Binomial distribution models the number of successes in a fixed number of independent, binary (success/failure) experiments.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Discrete and Limited:</b> Generates integer outcomes from 0 up to the number of trials.</li>
          <li><b>Fixed Number of Trials:</b> The total number of experiments is predetermined.</li>
        </ul>
        <b>When to Consider:</b> For binary outcomes (e.g., heads/tails) in a fixed number of trials.
      "),
      "Exponential" = HTML("
        <b>The Exponential Distribution:</b><br>
        The Exponential distribution models the time between successive random events in a memoryless process.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Memoryless Property:</b> The probability of an event occurring in the future is independent of the past.</li>
          <li><b>Continuous and Non-negative:</b> Defined only for x ≥ 0.</li>
        </ul>
        <b>When to Consider:</b> For modeling waiting times or durations until an event occurs.
      "),
      "Gamma" = HTML("
        <b>The Gamma Distribution:</b><br>
        The Gamma distribution generalizes the Exponential by modeling the waiting time until multiple events occur, defined by shape and rate parameters.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Continuous &amp; Non-negative:</b> Suitable for time or duration measurements.</li>
          <li><b>Flexible Shape:</b> Two parameters allow for various forms of skewed data.</li>
        </ul>
        <b>When to Consider:</b> When the time until an event depends on the occurrence of several events.
      "),
      "Log-Normal" = HTML("
        <b>The Log-Normal Distribution:</b><br>
        The Log-Normal distribution applies when the logarithm of the data follows a Normal distribution. It is useful for strictly positive, right-skewed data.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Positively Skewed:</b> Most values are small with a long tail on the right.</li>
          <li><b>Multiplicative Effects:</b> Often results from the product of many independent factors.</li>
        </ul>
        <b>When to Consider:</b> For variables such as incomes, stock prices, or biological measurements that cannot be negative.
      "),
      "Half-Cauchy" = HTML("
        <b>The Half-Cauchy Distribution:</b><br>
        The Half-Cauchy distribution is defined only for positive values and has heavy tails, making it popular as a prior for scale parameters in Bayesian analysis.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavy Tails:</b> Can accommodate occasional extreme observations.</li>
          <li><b>Restricted to Positive Values:</b> Suitable only for non-negative data.</li>
        </ul>
        <b>When to Consider:</b> When modeling a scale parameter where data may have outliers.
      "),
      "Tweedie" = HTML("
        <b>The Tweedie Distribution:</b><br>
        The Tweedie distribution is a flexible family that combines elements of both discrete and continuous models, particularly useful when data have many zeros alongside positive values.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Compound Structure:</b> Blends features of Poisson (for zeros/counts) and Gamma (for continuous values).</li>
          <li><b>Handles Excess Zeros:</b> Specifically designed for data with a surplus of zeros.</li>
        </ul>
        <b>When to Consider:</b> For modeling data like insurance claims or rainfall intensities where zeros are common.
      "),
      "Wald" = HTML("
        <b>The Wald Distribution (Inverse Gaussian):</b><br>
        The Wald distribution, also known as the Inverse Gaussian, models positive, skewed data and is often used for response times or failure times.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Positively Skewed:</b> Emphasizes lower values with a long right tail.</li>
          <li><b>Time-to-Event Modeling:</b> Suitable for data representing durations until an event occurs.</li>
        </ul>
        <b>When to Consider:</b> For strictly positive, skewed data such as reaction times.
      "),
      "ZIP" = HTML("
        <b>The Zero-Inflated Poisson (ZIP) Distribution:</b><br>
        The ZIP distribution extends the Poisson by incorporating a mass at zero to handle excess zero counts. It is used for modeling count data with more zeros than expected by a standard Poisson process.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Excess Zeros:</b> Explicitly models a higher chance for zero outcomes.</li>
          <li><b>Discrete Counts:</b> Results are non-negative integers.</li>
        </ul>
        <b>When to Consider:</b> For count data with a surplus of zeros, such as defect counts or species counts.
      "),
      "Bernoulli" = HTML("
        <b>The Bernoulli Distribution:</b><br>
        The Bernoulli distribution models a single binary trial with two possible outcomes—success (1) or failure (0). It is the most basic discrete probability model.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Binary Outcome:</b> Only two possible results (0 or 1).</li>
          <li><b>Foundational Model:</b> Serves as the basis for more complex models such as the Binomial distribution.</li>
        </ul>
        <b>When to Consider:</b> For modeling the outcome of a single yes/no experiment.
      "),
      "Negative Binomial" = HTML("
        <b>The Negative Binomial Distribution:</b><br>
        The Negative Binomial distribution models overdispersed count data—that is, data whose variance exceeds the mean. It represents the number of failures before achieving a predetermined number of successes.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Overdispersion:</b> More flexible than the Poisson when data exhibit greater variability.</li>
          <li><b>Discrete Outcomes:</b> Produces integer counts with additional spread.</li>
        </ul>
        <b>When to Consider:</b> For count data that are more variable than what a Poisson model predicts.
      ")
    )
    summary_text
  })
  
  # Explore: Distribution Notation -------------------------------------------------------------
  output$distributionNote <- renderUI({
    distribution <- input$distribution
    note <- switch(distribution,
                   "Normal" = withMathJax("$$\\mathcal{N}(\\mu,\\sigma^2)$$"),
                   "Poisson" = withMathJax("$$Pois(\\lambda)$$"),
                   "Uniform" = withMathJax("$$\\mathcal{U}(a,b)$$"),
                   "Beta" = withMathJax("$$Beta(\\alpha,\\beta)$$"),
                   "Student's T" = withMathJax("$$T(\\nu)$$"),
                   "Binomial" = withMathJax("$$Binom(n,p)$$"),
                   "Exponential" = withMathJax("$$Exp(\\lambda)$$"),
                   "Gamma" = withMathJax("$$Gamma(\\alpha,\\beta)$$"),
                   "Log-Normal" = withMathJax("$$LN(\\mu,\\sigma^2)$$"),
                   "Half-Cauchy" = withMathJax("$$HC(\\alpha)$$"),
                   "Tweedie" = withMathJax("$$TW_p(\\mu,\\phi)$$"),
                   "Wald" = tagList(
                     withMathJax("$$IG(\\mu,\\lambda)$$"),
                     h6("Wald is another term for the Inverse Gaussian distribution.")
                   ),
                   "ZIP" = withMathJax("$$ZIP(\\lambda,p)$$"),
                   "Bernoulli" = withMathJax("$$Bern(p)$$"),
                   "Negative Binomial" = withMathJax("$$NB(\\mu,n)$$")
    )
    note |> tagList()
  })
  
  # Explore: PDF/PMF Description -------------------------------------------------------------
  output$distributionPDF <- renderUI({
    distribution <- input$distribution
    note <- switch(distribution,
                   "Normal" = withMathJax("$$f(x)=\\frac{1}{\\sigma\\sqrt{2\\pi}}\\exp\\Big(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\Big)$$"),
                   "Poisson" = withMathJax("$$f(x)=\\frac{\\lambda^{as.integer(x)}e^{-\\lambda}}{(as.integer(x))!}$$"),
                   "Uniform" = withMathJax("$$f(x)=\\frac{1}{b-a}\\quad \\text{for }a\\le x\\le b$$"),
                   "Beta" = withMathJax("$$f(x)=\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}x^{\\alpha-1}(1-x)^{\\beta-1}\\quad \\text{for }0\\le x\\le 1$$"),
                   "Student's T" = withMathJax("$$f(x)=\\frac{\\Gamma((\\nu+1)/2)}{\\sqrt{\\nu\\pi}\\,\\Gamma(\\nu/2)}\\Big(1+\\frac{x^2}{\\nu}\\Big)^{-\\frac{\\nu+1}{2}}$$"),
                   "Binomial" = withMathJax("$$f(x)=\\binom{n}{as.integer(x)}p^{as.integer(x)}(1-p)^{n-as.integer(x)}$$"),
                   "Exponential" = withMathJax("$$f(x)=\\lambda e^{-\\lambda x}\\quad \\text{for }x\\ge 0$$"),
                   "Gamma" = withMathJax("$$f(x)=\\frac{\\lambda^\\alpha}{\\Gamma(\\alpha)}x^{\\alpha-1}e^{-\\lambda x}\\quad \\text{for }x\\ge 0$$"),
                   "Log-Normal" = withMathJax("$$f(x)=\\frac{1}{x\\sigma\\sqrt{2\\pi}}\\exp\\Big(-\\frac{(\\ln x-\\mu)^2}{2\\sigma^2}\\Big)\\quad \\text{for }x>0$$"),
                   "Half-Cauchy" = withMathJax("$$f(x)=\\frac{2\\alpha}{\\pi(x^2+\\alpha^2)}\\quad \\text{for }x\\ge 0$$"),
                   "Tweedie" = withMathJax("$$f(x)=h(\\sigma^2,x)\\exp\\Big[\\frac{px-A(p)}{\\sigma^2}\\Big]$$"),
                   "Wald" = withMathJax("$$f(x)=\\sqrt{\\frac{\\lambda}{2\\pi x^3}}\\exp\\Big(-\\frac{\\lambda(x-\\mu)^2}{2\\mu^2x}\\Big)$$"),
                   "ZIP" = withMathJax("$$f(x)=\\begin{cases} \\pi+(1-\\pi)e^{-\\lambda} & x=0\\\\ (1-\\pi)\\frac{\\lambda^{as.integer(x)}e^{-\\lambda}}{(as.integer(x))!} & x>0 \\end{cases}$$"),
                   "Bernoulli" = withMathJax("$$f(x)=p^{as.integer(x)}(1-p)^{1-as.integer(x)}\\quad \\text{for }x\\in\\{0,1\\}$$"),
                   "Negative Binomial" = withMathJax("$$f(x)=\\frac{\\Gamma(as.integer(x)+n)}{\\Gamma(n)\\,\\Gamma(as.integer(x)+1)}\\Big(\\frac{n}{n+\\mu}\\Big)^n\\Big(\\frac{\\mu}{n+\\mu}\\Big)^{as.integer(x)}\\quad \\text{for }x=0,1,2,...$$")
    )
    note |> tagList()
  })
  
  # Explore: Distribution Examples -------------------------------------------------------------
  output$distributionEG <- renderText({
    distribution <- input$distribution
    text <- switch(distribution,
                   "Normal" = "Examples: Plant height, exam scores, IQ scores, blood pressure readings.",
                   "Poisson" = "Examples: Number of phone calls per hour, customer arrivals, manufacturing defects.",
                   "Uniform" = "Examples: Random lottery numbers, random sampling within a fixed range.",
                   "Beta" = "Examples: Conversion rates, proportion of defective items, survey proportions.",
                   "Student's T" = "Examples: Test scores from small samples, reaction times, small-sample measurements.",
                   "Binomial" = "Examples: Successes in coin tosses, pass/fail outcomes, survey responses.",
                   "Exponential" = "Examples: Time between arrivals, waiting times, duration until an event.",
                   "Gamma" = "Examples: Waiting times until multiple events, machine lifespans, service times.",
                   "Log-Normal" = "Examples: Income distributions, stock prices, biological growth measures.",
                   "Half-Cauchy" = "Examples: Priors for scale in Bayesian models, or data with potential outliers.",
                   "Tweedie" = "Examples: Insurance claim amounts, rainfall intensities, ecological count data.",
                   "Wald" = "Examples: Reaction times, reliability data, time until system failure.",
                   "ZIP" = "Examples: Count data with excess zeros, such as species counts or defect counts.",
                   "Bernoulli" = "Examples: Outcome of a single yes/no experiment.",
                   "Negative Binomial" = "Examples: Overdispersed counts such as goals in sports or failures before success."
    )
    text
  })
  
  # Compare: Parameter Inputs for Selected Distributions -------------------------------------------------------------
  output$paramInputs <- renderUI({
    selected_distributions <- input$selectedDistributions
    param_inputs <- lapply(selected_distributions, function(dist) {
      params <- default_params[[dist]]
      param_list <- lapply(names(params), function(param) {
        value <- params[[param]]
        numericInput(paste0(param, "_", dist), label = param, value = value)
      })
      tagList(
        h4(dist),
        param_list
      )
    })
    do.call(tagList, param_inputs)
  })
  
  # Compare: Multi-distribution Density Plot -------------------------------------------------------------
  output$densityPlotMulti <- renderPlot({
    selected_distributions <- input$selectedDistributions
    if (is.null(selected_distributions) || length(selected_distributions) == 0) {
      return(NULL)
    }
    df_density <- data.frame(x = numeric(0), density = numeric(0), distribution = character(0), stringsAsFactors = FALSE)
    for (dist in selected_distributions) {
      params <- default_params[[dist]]
      param_values <- reactiveValuesToList(input)
      param_values <- param_values[grep(paste0(dist, "$"), names(param_values))]
      xmin <- input$xminmulti
      xmax <- input$xmaxmulti
      x <- seq(xmin, xmax, length.out = 100)
      density_out <- switch(dist,
                            "Normal" = {
                              if (!is.null(param_values$mean) && !is.null(param_values$sd)) {
                                dnorm(x, mean = param_values$mean, sd = param_values$sd)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Poisson" = {
                              if (!is.null(param_values$lambda)) {
                                dpois(as.integer(x), lambda = param_values$lambda)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Uniform" = {
                              if (!is.null(param_values$min) && !is.null(param_values$max)) {
                                dunif(x, min = param_values$min, max = param_values$max)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Beta" = {
                              if (!is.null(param_values$alpha) && !is.null(param_values$beta)) {
                                dbeta(x, shape1 = param_values$alpha, shape2 = param_values$beta)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Student's T" = {
                              if (!is.null(param_values$df)) {
                                dt(x, df = param_values$df)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Exponential" = {
                              if (!is.null(param_values$rate)) {
                                dexp(x, rate = param_values$rate)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Gamma" = {
                              if (!is.null(param_values$shape) && !is.null(param_values$rate)) {
                                dgamma(x, shape = param_values$shape, rate = param_values$rate)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Log-Normal" = {
                              if (!is.null(param_values$meanlog) && !is.null(param_values$sdlog)) {
                                dlnorm(x, meanlog = param_values$meanlog, sdlog = param_values$sdlog)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Half-Cauchy" = {
                              if (!is.null(param_values$scale)) {
                                dcauchy(x, location = 0, scale = param_values$scale)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Tweedie" = {
                              if (!is.null(param_values$power) && !is.null(param_values$mu) && !is.null(param_values$phi)) {
                                dtweedie(x, power = param_values$power, mu = param_values$mu, phi = param_values$phi)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Wald" = {
                              if (!is.null(param_values$mu) && !is.null(param_values$lambda)) {
                                dwald(x, mu = param_values$mu, lambda = param_values$lambda)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Binomial" = {
                              if (!is.null(param_values$size) && !is.null(param_values$prob)) {
                                dbinom(as.integer(x), size = param_values$size, prob = param_values$prob)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Bernoulli" = {
                              if (!is.null(param_values$prob)) {
                                dbern(as.integer(x), p = param_values$prob)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "ZIP" = {
                              if (!is.null(param_values$lambda) && !is.null(param_values$pi)) {
                                dzip(as.integer(x), lambda = param_values$lambda, pi = param_values$pi)
                              } else {
                                rep(0, length(x))
                              }
                            },
                            "Negative Binomial" = {
                              if (!is.null(param_values$size) && !is.null(param_values$mu)) {
                                dnbinom(as.integer(x), size = param_values$size, mu = param_values$mu)
                              } else {
                                rep(0, length(x))
                              }
                            }
      )
      df <- data.frame(x = x, density = density_out, distribution = dist, stringsAsFactors = FALSE)
      df$density <- df$density / max(df$density, na.rm = TRUE)
      df_density <- rbind(df_density, df)
    }
    df_density <- df_density[-1, ]
    ggplot(df_density, aes(x = x, y = density, color = distribution, group = as.factor(distribution))) +
      geom_line(size = 1) +
      xlab("Possible values (x)") +
      ylab("Standardised Density") +
      scale_color_brewer(palette = "Set2") +
      theme_minimal() +
      guides(color = guide_legend(title = "Distribution")) +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#202123"),
        plot.title = element_text(color = "white", size = 16),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 14),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 10),
        panel.grid.major = element_line(color = "#444654"),
        panel.grid.minor = element_line(color = "#444654")
      )
  })
  
  # Duplicate: setCountDisplay and distName (if needed by Compare) -------------------------------------------------------------
  output$setCountDisplay <- renderUI({
    tags$p(strong("Current number of Parameter Sets:"), parameterSetCount())
  })
  
  output$distName <- renderText({
    paste("The", input$distribution, "distribution")
  })
  
  # Duplicate: Distribution Description, Notation, PDF, and Examples (if present) -------------------------------------------------------------
  output$distributionText <- renderUI({
    distribution <- input$distribution
    summary_text <- switch(distribution,
                           "Normal" = HTML("
        <b>The Normal Distribution (Gaussian):</b><br>
        The Normal distribution is one of the most fundamental probability distributions in statistics. It is defined by a mathematical formula that approximates a wide variety of datasets—ranging from human traits (like height and weight) to test scores. Unlike a uniform distribution, which treats all outcomes equally, the Normal follows a distinct “bell curve” pattern.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Bell-Shaped and Symmetric:</b> Values cluster around the mean with a symmetric decline on either side.</li>
          <li><b>Standard Deviation Controls Spread:</b> A smaller standard deviation yields a narrow, steep curve; a larger one, a wider, flatter curve.</li>
          <li><b>Range:</b> It covers all real numbers, from negative infinity to positive infinity.</li>
        </ul>
        <b>When to Consider:</b> For continuous data that cluster around a central value and extend in both directions.
      "),
      "Poisson" = HTML("
        <b>The Poisson Distribution:</b><br>
        The Poisson distribution models the number of times an event occurs in a fixed interval when events occur independently at a constant rate.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Discrete Count Data:</b> Outputs non-negative integers.</li>
          <li><b>Mean Equals Variance:</b> A unique property where the average rate equals the variance.</li>
          <li><b>Unbounded Above:</b> Although counts start at zero, there is no theoretical upper limit.</li>
        </ul>
        <b>When to Consider:</b> For modeling counts of rare events, like customer arrivals or emails per hour.
      "),
      "Uniform" = HTML("
        <b>The Uniform Distribution:</b><br>
        The Uniform distribution gives all outcomes within a specified interval equal probability, resulting in a flat, rectangular shape.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Equal Likelihood:</b> Every value within the range is equally likely.</li>
          <li><b>Constant Density:</b> The probability density remains unchanged across the interval.</li>
          <li><b>Defined Interval:</b> Only values between two endpoints are possible.</li>
        </ul>
        <b>When to Consider:</b> When there is no reason to favor one outcome over another within a fixed range.
      "),
      "Beta" = HTML("
        <b>The Beta Distribution:</b><br>
        The Beta distribution is defined on the interval [0, 1] and is ideal for modeling proportions or probabilities.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Bounded:</b> All outcomes lie between 0 and 1.</li>
          <li><b>Flexible Shape:</b> Two parameters allow the distribution to take on many shapes (e.g., U-shaped, uniform, bell-shaped).</li>
        </ul>
        <b>When to Consider:</b> For modeling conversion rates, proportions, or any variable constrained between 0 and 1.
      "),
      "Student's T" = HTML("
        <b>The Student's T Distribution:</b><br>
        The Student's T distribution resembles the Normal distribution but with heavier tails, making it valuable when sample sizes are small or when the population variance is uncertain.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavy Tails:</b> More likelihood for extreme values than the Normal distribution.</li>
          <li><b>Degrees of Freedom:</b> As sample size increases, it converges to the Normal distribution.</li>
        </ul>
        <b>When to Consider:</b> For small samples or when the population variance is unknown.
      "),
      "Binomial" = HTML("
        <b>The Binomial Distribution:</b><br>
        The Binomial distribution models the number of successes in a fixed number of independent, binary (success/failure) experiments.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Discrete:</b> Outcomes are integers ranging from 0 to the total number of trials.</li>
          <li><b>Fixed Number of Trials:</b> The number of experiments is predetermined.</li>
        </ul>
        <b>When to Consider:</b> For binary outcomes (e.g., heads/tails) in a fixed number of trials.
      "),
      "Exponential" = HTML("
        <b>The Exponential Distribution:</b><br>
        The Exponential distribution is used for modeling the time between successive random events in a memoryless process.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Memoryless Property:</b> The probability of an event occurring in the future is independent of the past.</li>
          <li><b>Continuous and Non-negative:</b> Defined only for x ≥ 0.</li>
        </ul>
        <b>When to Consider:</b> For modeling waiting times or durations until an event occurs.
      "),
      "Gamma" = HTML("
        <b>The Gamma Distribution:</b><br>
        The Gamma distribution generalizes the Exponential, modeling the waiting time until multiple events occur. It is defined by shape and rate parameters.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Continuous &amp; Non-negative:</b> Suitable for time or duration measurements.</li>
          <li><b>Flexible Shape:</b> With two parameters, it can mimic various forms of skewed data.</li>
        </ul>
        <b>When to Consider:</b> When modeling the time until several events occur.
      "),
      "Log-Normal" = HTML("
        <b>The Log-Normal Distribution:</b><br>
        The Log-Normal distribution applies when the logarithm of the data is Normally distributed. It is useful for strictly positive, right-skewed data.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Positively Skewed:</b> Most values are small with a long tail on the right.</li>
          <li><b>Multiplicative Effects:</b> Often results from the product of many independent factors.</li>
        </ul>
        <b>When to Consider:</b> For variables such as incomes, stock prices, or biological growth measures.
      "),
      "Half-Cauchy" = HTML("
        <b>The Half-Cauchy Distribution:</b><br>
        The Half-Cauchy distribution is defined for positive values and has heavy tails, making it popular as a prior for scale parameters in Bayesian analysis.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavy Tails:</b> Accommodates occasional extreme observations.</li>
          <li><b>Only Positive:</b> Suitable for non-negative data.</li>
        </ul>
        <b>When to Consider:</b> For modeling a scale parameter with potential outliers.
      "),
      "Tweedie" = HTML("
        <b>The Tweedie Distribution:</b><br>
        The Tweedie distribution is a flexible family that combines discrete and continuous outcomes—especially useful when data contain many zeros along with positive values.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Compound Structure:</b> Merges features of Poisson (for zeros) with Gamma (for positive values).</li>
          <li><b>Handles Excess Zeros:</b> Designed for data with a surplus of zeros.</li>
        </ul>
        <b>When to Consider:</b> For modeling insurance claims or rainfall intensities.
      "),
      "Wald" = HTML("
        <b>The Wald Distribution (Inverse Gaussian):</b><br>
        The Wald (or Inverse Gaussian) models positive, skewed data, often used for reliability or response time analysis.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Positively Skewed:</b> Emphasizes lower values with a long right tail.</li>
          <li><b>Time-to-Event:</b> Suitable for durations until an event occurs.</li>
        </ul>
        <b>When to Consider:</b> For strictly positive, skewed data such as reaction times.
      "),
      "ZIP" = HTML("
        <b>The Zero-Inflated Poisson (ZIP) Distribution:</b><br>
        The ZIP extends the Poisson by including a mass at zero for excess zeros. Used when standard Poisson underestimates zeros.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Excess Zeros:</b> Higher probability for zeros.</li>
          <li><b>Discrete Counts:</b> Produces non-negative integers.</li>
        </ul>
        <b>When to Consider:</b> For count data with surplus zeros.
      "),
      "Bernoulli" = HTML("
        <b>The Bernoulli Distribution:</b><br>
        The Bernoulli models a single binary trial with two outcomes: success (1) or failure (0). It is the simplest discrete model.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Binary Outcome:</b> Two possible values.</li>
          <li><b>Foundational:</b> Basis for the Binomial distribution.</li>
        </ul>
        <b>When to Consider:</b> For single trial outcomes.
      "),
      "Negative Binomial" = HTML("
        <b>The Negative Binomial Distribution:</b><br>
        The Negative Binomial models overdispersed count data, representing the number of failures before a target number of successes.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Overdispersion:</b> More flexible variance than Poisson.</li>
          <li><b>Discrete Outcomes:</b> Provides integer counts with extra variability.</li>
        </ul>
        <b>When to Consider:</b> For count data that exceed Poisson variance.
      ")
    )
    summary_text
  })
  
  # Duplicate: Distribution Notation (if repeated) -------------------------------------------------------------
  output$distributionNote <- renderUI({
    distribution <- input$distribution
    note <- switch(distribution,
                   "Normal" = withMathJax("$$\\mathcal{N}(\\mu,\\sigma^2)$$"),
                   "Poisson" = withMathJax("$$Pois(\\lambda)$$"),
                   "Uniform" = withMathJax("$$\\mathcal{U}(a,b)$$"),
                   "Beta" = withMathJax("$$Beta(\\alpha,\\beta)$$"),
                   "Student's T" = withMathJax("$$T(\\nu)$$"),
                   "Binomial" = withMathJax("$$Binom(n,p)$$"),
                   "Exponential" = withMathJax("$$Exp(\\lambda)$$"),
                   "Gamma" = withMathJax("$$Gamma(\\alpha,\\beta)$$"),
                   "Log-Normal" = withMathJax("$$LN(\\mu,\\sigma^2)$$"),
                   "Half-Cauchy" = withMathJax("$$HC(\\alpha)$$"),
                   "Tweedie" = withMathJax("$$TW_p(\\mu,\\phi)$$"),
                   "Wald" = tagList(
                     withMathJax("$$IG(\\mu,\\lambda)$$"),
                     h6("Wald is another term for the Inverse Gaussian distribution.")
                   ),
                   "ZIP" = withMathJax("$$ZIP(\\lambda,p)$$"),
                   "Bernoulli" = withMathJax("$$Bern(p)$$"),
                   "Negative Binomial" = withMathJax("$$NB(\\mu,n)$$")
    )
    note |> tagList()
  })
  
  # Simulate Data -------------------------------------------------------------
  simData <- reactive({
    N <- input$sim_N
    if (input$sim_use_x) {
      x <- runif(N, 0, 10)
    } else {
      x <- rep(NA, N)
    }
    grp <- if (input$sim_use_grp) {
      factor(paste("Group", toupper(letters[sample(1:as.numeric(input$sim_grps), size = N, replace = TRUE)])))
    } else rep(NA, N)
    
    linear_pred <- rep(0, N)
    if (input$sim_use_x) {
      if (isTRUE(input$nonlinear_x)) {
        linear_pred <- linear_pred + input$sim_intercept + input$sim_slope * (4 * sin(2 + 0.6 * x + 1))
      } else {
        linear_pred <- linear_pred + input$sim_intercept + input$sim_slope * x
      }
    } else {
      linear_pred <- linear_pred + input$sim_intercept
    }
    if (input$sim_use_grp) {
      effect <- rep(0, N)
      effect[grp == "Group B"] <- input$sim_grpB
      if(as.numeric(input$sim_grps) == 3)
        effect[grp == "Group C"] <- input$sim_grpC
      linear_pred <- linear_pred + effect
    }
    
    if (input$sim_unobs_linear) {
      z <- rnorm(N)
      linear_pred <- linear_pred + 1 * z
    } else {
      z <- NA
    }
    
    if (input$sim_unobs_cat) {
      unobs_cat <- factor(sample(c("A", "B"), N, replace = TRUE))
      linear_pred <- linear_pred + ifelse(unobs_cat == "B", 1, 0) * 5
    } else {
      unobs_cat <- NA
    }
    
    if (input$sim_unobs_nonlinear) {
      v <- runif(N, 0, 2 * pi)
      linear_pred <- linear_pred + 8 * sin(0.8 * v)
    } else {
      v <- NA
    }
    
    sim_y <- switch(input$sim_dist,
                    "Normal" = rnorm(N, mean = linear_pred, sd = input$sim_sd),
                    "Poisson" = rpois(N, lambda = exp(linear_pred)),
                    "Binomial" = rbinom(N, size = input$sim_trials, prob = plogis(linear_pred)),
                    "Bernoulli" = rbinom(N, size = 1, prob = plogis(linear_pred))
    )
    
    data.frame(y = sim_y, x = x, grp = grp, z = z, unobs_cat = unobs_cat, v = v)
  })
  
  # Fit Model -------------------------------------------------------------
  fitModel <- reactive({
    df <- simData()
    if (input$sim_use_x & input$sim_use_grp) {
      predictor_str <- "x + grp"
    } else if (input$sim_use_x) {
      predictor_str <- "x"
    } else if (input$sim_use_grp) {
      predictor_str <- "grp"
    } else {
      predictor_str <- "1"
    }
    if (input$mod_family == "Binomial" & input$sim_dist != "Bernoulli") {
      df$fail <- input$sim_trials - df$y
      form <- as.formula(paste("cbind(y, fail) ~", predictor_str))
      mod <- glm(form, data = df, family = binomial())
    } else {
      form <- as.formula(paste("y ~", predictor_str))
      fam <- switch(input$mod_family,
                    "Normal"    = gaussian(),
                    "Poisson"   = poisson(),
                    "Bernoulli" = binomial()
      )
      mod <- glm(form, data = df, family = fam)
    }
    mod
  })
  
  # Forest Plot -------------------------------------------------------------
  output$forestPlot <- renderPlot({
    df <- simData()
    mod <- fitModel()
    true_params <- list("(Intercept)" = input$sim_intercept)
    if (input$sim_use_x) {
      true_params[["x"]] <- input$sim_slope
    }
    if (input$sim_use_grp) {
      true_params[["grpGroup B"]] <- input$sim_grpB
      if (as.numeric(input$sim_grps) == 3) {
        true_params[["grpGroup C"]] <- input$sim_grpC
      }
    }
    est <- coef(mod)
    cis <- confint(mod)
    if (is.null(dim(cis))) {
      cis <- matrix(cis, nrow = 1)
    }
    param_names <- names(est)
    df_forest <- data.frame(
      Parameter = param_names,
      Estimated = unname(est),
      Lower = cis[, 1],
      Upper = cis[, 2],
      True = sapply(param_names, function(p) {
        if (p %in% names(true_params)) true_params[[p]] else NA
      }),
      stringsAsFactors = FALSE
    )
    ggplot(df_forest, aes(x = Parameter, y = Estimated)) +
      geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "#1B9E77") +
      geom_point(aes(y = True), color = "#D95F02", shape = 18, size = 3) +
      coord_flip() +
      labs(
        title = "Comparison of True vs Estimated Parameters",
        y = "Coefficient Value"
      ) +
      theme_minimal() +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#202123"),
        plot.title = element_text(color = "white", size = 16),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 14),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 10),
        panel.grid.major = element_line(color = "#444654"),
        panel.grid.minor = element_line(color = "#444654")
      )
  })
  
  # Predicted Relationship Plot -------------------------------------------------------------
  output$predPlot <- renderPlot({
    df <- simData()
    mod <- fitModel()
    df$predicted <- predict(mod, newdata = df, type = "response")
    if (input$mod_family == "Binomial") {
      df$predicted <- input$sim_trials * df$predicted
    }
    
    true_lp <- rep(0, nrow(df))
    
    true_lp <- true_lp + input$sim_intercept
    
    if (isTRUE(input$sim_use_x)) {
      if (isTRUE(input$nonlinear_x)) {
        true_lp <- true_lp + input$sim_slope * (4 * sin(2 + 0.6 * df$x + 1))
      } else {
        true_lp <- true_lp + input$sim_slope * df$x
      }
    }
    
    if (input$sim_use_grp) {
      effect <- rep(0, nrow(df))
      effect[df$grp == "Group B"] <- input$sim_grpB
      if (as.numeric(input$sim_grps) == 3) {
        effect[df$grp == "Group C"] <- input$sim_grpC
      }
      true_lp <- true_lp + effect
    }
    df$true <- switch(input$sim_dist,
                      "Normal" = true_lp,
                      "Poisson" = exp(true_lp),
                      "Binomial" = input$sim_trials * plogis(true_lp),
                      "Bernoulli" = plogis(true_lp)
    )
    if (input$sim_use_x & input$sim_use_grp) {
      p <- ggplot(df, aes(x = x)) +
        geom_point(aes(y = y), color = "grey") +
        geom_line(aes(y = true, color = "Truth"), size = 1) +
        geom_line(aes(y = predicted, color = "Estimated"), size = 1, linetype = "dashed") +
        facet_wrap(~grp) +
        labs(
          title = "True vs Predicted Relationships by Group",
          x = "x", y = "y", color = ""
        ) +
        scale_color_brewer(palette = "Dark2", breaks = c("Truth", "Estimated")) +
        theme_minimal() +
        theme(
          panel.background = element_blank(),
          plot.background = element_rect(fill = "#202123"),
          plot.title = element_text(color = "white", size = 16),
          axis.text = element_text(color = "white", size = 12),
          axis.title = element_text(color = "white", size = 14),
          legend.title = element_text(color = "white", size = 12),
          legend.text = element_text(color = "white", size = 10),
          panel.grid.major = element_line(color = "#444654"),
          panel.grid.minor = element_line(color = "#444654")
        )
    } else if (input$sim_use_x) {
      p <- ggplot(df, aes(x = x)) +
        geom_point(aes(y = y), color = "grey") +
        geom_line(aes(y = true, color = "Truth"), size = 1) +
        geom_line(aes(y = predicted, color = "Estimated"), size = 1, linetype = "dashed") +
        labs(
          title = "True vs Predicted Relationship",
          x = "x", y = "y", color = ""
        ) +
        scale_color_brewer(palette = "Dark2", breaks = c("Truth", "Estimated")) +
        theme_minimal() +
        theme(
          panel.background = element_blank(),
          plot.background = element_rect(fill = "#202123"),
          plot.title = element_text(color = "white", size = 16),
          axis.text = element_text(color = "white", size = 12),
          axis.title = element_text(color = "white", size = 14),
          legend.title = element_text(color = "white", size = 12),
          legend.text = element_text(color = "white", size = 10),
          panel.grid.major = element_line(color = "#444654"),
          panel.grid.minor = element_line(color = "#444654")
        )
    } else if (input$sim_use_grp) {
      p <- ggplot(df, aes(x = grp)) +
        geom_boxplot(aes(y = y), fill = "#72758d", colour = "white") +
        geom_point(aes(y = true, color = "Truth"), size = 3) +
        geom_point(aes(y = predicted, color = "Estimated"), size = 3, shape = 17) +
        labs(
          title = "True vs Predicted Group Means",
          x = "Group", y = "y", color = ""
        ) +
        scale_color_brewer(palette = "Dark2", breaks = c("Truth", "Estimated")) +
        theme_minimal() +
        theme(
          panel.background = element_blank(),
          plot.background = element_rect(fill = "#202123"),
          plot.title = element_text(color = "white", size = 14),
          plot.subtitle = element_text(color = "white", size = 12),
          axis.title = element_text(color = "white", size = 12),
          axis.text = element_text(color = "white", size = 8),
          legend.title = element_text(color = "white", size = 12),
          legend.text = element_text(color = "white", size = 8),
          strip.text = element_text(color = "white", size = 8),
          strip.background = element_rect(color = "#202123", fill = "#202123", size = 1),
          panel.border = element_rect(color = "#202123", fill = NA, size = 1),
          panel.grid.major = element_line(color = "#444654"),
          panel.grid.minor = element_line(color = "#444654")
        )
    } else {
      p <- ggplot(df, aes(x = 1:nrow(df), y = y)) +
        geom_point(color = "grey") +
        geom_line(aes(y = true, color = "Truth"), size = 1) +
        geom_line(aes(y = predicted, color = "Estimated"), size = 1, linetype = "dashed") +
        labs(
          title = "True vs Predicted (Intercept-only model)",
          x = "Observation", y = "y", color = ""
        ) +
        scale_color_brewer(palette = "Dark2", breaks = c("Truth", "Estimated")) +
        theme_minimal() +
        theme(
          panel.background = element_blank(),
          plot.background = element_rect(fill = "#202123"),
          plot.title = element_text(color = "white", size = 14),
          plot.subtitle = element_text(color = "white", size = 12),
          axis.title = element_text(color = "white", size = 12),
          axis.text = element_text(color = "white", size = 8),
          legend.title = element_text(color = "white", size = 12),
          legend.text = element_text(color = "white", size = 8),
          strip.text = element_text(color = "white", size = 8),
          strip.background = element_rect(color = "#202123", fill = "#202123", size = 1),
          panel.border = element_rect(color = "#202123", fill = NA, size = 1),
          panel.grid.major = element_line(color = "#444654"),
          panel.grid.minor = element_line(color = "#444654")
        )
    }
    p
  })
  
  # Parameter Summary Table -------------------------------------------------------------
  output$paramSummary <- DT::renderDataTable({
    mod <- fitModel()
    est <- coef(mod)
    true_params <- list()
    if (input$sim_use_x) {
      true_params[["(Intercept)"]] <- input$sim_intercept
      true_params[["x"]] <- input$sim_slope
    } else {
      true_params[["(Intercept)"]] <- input$sim_intercept
    }
    if (input$sim_use_grp) {
      true_params[["grpGroup B"]] <- input$sim_grpB
      if (as.numeric(input$sim_grps) == 3) {
        true_params[["grpGroup C"]] <- input$sim_grpC
      }
    }
    df_forest <- data.frame(
      Parameter = names(est),
      Estimated = unname(est),
      True = sapply(names(est), function(p) {
        if (p %in% names(true_params)) true_params[[p]] else NA
      }),
      stringsAsFactors = FALSE
    )
    DT::datatable(df_forest, rownames = FALSE, options = list(pageLength = 10)) |>
      DT::formatStyle(columns = names(df_forest), color = "white")
  })
  
  # Diagnostics Plot -------------------------------------------------------------
  output$diagnosticsPlot <- renderPlot({
    model <- fitModel()
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(
      mfrow = c(2, 2),
      bg = "#202123",
      col.lab = "white",
      col.axis = "white",
      col.main = "white",
      col.sub = "white",
      fg = "white",
      cex.lab = 1.2,
      cex.main = 1.4
    )
    plot(model)
  })
  
  # Data Generating Process Equation -------------------------------------------------------------
  output$dgp_eq <- renderUI({
    sim_dist <- input$sim_dist
    use_x <- input$sim_use_x
    use_grp <- input$sim_use_grp
    grp_count <- if (use_grp) as.numeric(input$sim_grps) else 0
    nonlinear <- isTRUE(input$nonlinear_x)
    
    if (sim_dist == "Normal") {
      eq1 <- "$$ y_i \\sim Normal(\\mu_i, \\sigma^2) $$"
      if (use_x && use_grp) {
        if (grp_count == 2) {
          eq2 <- if (nonlinear) {
            "$$ \\mu_i = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB $$"
          } else {
            "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB $$"
          }
        } else {
          eq2 <- if (nonlinear) {
            "$$ \\mu_i = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          } else {
            "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          }
        }
      } else if (use_x) {
        eq2 <- if (nonlinear) {
          "$$ \\mu_i = \\beta_0 + f(x_i) $$"
        } else {
          "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i $$"
        }
      } else if (use_grp) {
        if (grp_count == 2) {
          eq2 <- "$$ \\mu_i = \\beta_0 + \\beta_1 \\times GrpB $$"
        } else {
          eq2 <- "$$ \\mu_i = \\beta_0 + \\beta_1 \\times GrpB + \\beta_2 \\times GrpC $$"
        }
      } else {
        eq2 <- "$$ \\mu_i = \\beta_0 $$"
      }
    } else if (sim_dist == "Poisson") {
      eq1 <- "$$ y_i \\sim Poisson(\\lambda_i) $$"
      if (use_x && use_grp) {
        if (grp_count == 2) {
          eq2 <- if (nonlinear) {
            "$$ \\log(\\lambda_i) = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB $$"
          } else {
            "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB $$"
          }
        } else {
          eq2 <- if (nonlinear) {
            "$$ \\log(\\lambda_i) = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          } else {
            "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          }
        }
      } else if (use_x) {
        eq2 <- if (nonlinear) {
          "$$ \\log(\\lambda_i) = \\beta_0 + f(x_i) $$"
        } else {
          "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1\\,x_i $$"
        }
      } else if (use_grp) {
        if (grp_count == 2) {
          eq2 <- "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1 \\times GrpB $$"
        } else {
          eq2 <- "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1 \\times GrpB + \\beta_2 \\times GrpC $$"
        }
      } else {
        eq2 <- "$$ \\log(\\lambda_i) = \\beta_0 $$"
      }
    } else if (sim_dist == "Binomial") {
      eq1 <- "$$ y_i \\sim Binomial(n, \\pi_i) $$"
      if (use_x && use_grp) {
        if (grp_count == 2) {
          eq2 <- if (nonlinear) {
            "$$ logit(\\pi_i) = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB $$"
          } else {
            "$$ logit(\\pi_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB $$"
          }
        } else {
          eq2 <- if (nonlinear) {
            "$$ logit(\\pi_i) = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          } else {
            "$$ logit(\\pi_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          }
        }
      } else if (use_x) {
        eq2 <- if (nonlinear) {
          "$$ logit(\\pi_i) = \\beta_0 + f(x_i) $$"
        } else {
          "$$ logit(\\pi_i) = \\beta_0 + \\beta_1\\,x_i $$"
        }
      } else if (use_grp) {
        if (grp_count == 2) {
          eq2 <- "$$ logit(\\pi_i) = \\beta_0 + \\beta_1 \\times GrpB $$"
        } else {
          eq2 <- "$$ logit(\\pi_i) = \\beta_0 + \\beta_1 \\times GrpB + \\beta_2 \\times GrpC $$"
        }
      } else {
        eq2 <- "$$ logit(\\pi_i) = \\beta_0 $$"
      }
    } else if (sim_dist == "Bernoulli") {
      eq1 <- "$$ y_i \\sim Bernoulli(\\pi_i) $$"
      if (use_x && use_grp) {
        if (grp_count == 2) {
          eq2 <- if (nonlinear) {
            "$$ logit(\\pi_i) = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB $$"
          } else {
            "$$ logit(\\pi_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB $$"
          }
        } else {
          eq2 <- if (nonlinear) {
            "$$ logit(\\pi_i) = \\beta_0 + f(x_i) + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          } else {
            "$$ logit(\\pi_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
          }
        }
      } else if (use_x) {
        eq2 <- if (nonlinear) {
          "$$ logit(\\pi_i) = \\beta_0 + f(x_i) $$"
        } else {
          "$$ logit(\\pi_i) = \\beta_0 + \\beta_1\\,x_i $$"
        }
      } else if (use_grp) {
        if (grp_count == 2) {
          eq2 <- "$$ logit(\\pi_i) = \\beta_0 + \\beta_1 \\times GrpB $$"
        } else {
          eq2 <- "$$ logit(\\pi_i) = \\beta_0 + \\beta_1 \\times GrpB + \\beta_2 \\times GrpC $$"
        }
      } else {
        eq2 <- "$$ logit(\\pi_i) = \\beta_0 $$"
      }
    }
    withMathJax(HTML(paste(eq1, eq2)))
  })
  
  
  # Model Equation for Fitting -------------------------------------------------------------
  output$model_eq <- renderUI({
    mod_fam <- input$mod_family
    use_x <- input$sim_use_x
    use_grp <- input$sim_use_grp
    grp_count <- if (use_grp) as.numeric(input$sim_grps) else 0
    
    if (mod_fam == "Normal") {
      eq1 <- "$$ y_i \\sim Normal(\\mu_i, \\sigma^2) $$"
      if (use_x && use_grp) {
        eq2 <- if (grp_count == 2) {
          "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB $$"
        } else {
          "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB + \\beta_3\\times GrpC $$"
        }
      } else if (use_x) {
        eq2 <- "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i $$"
      } else if (use_grp) {
        eq2 <- if (grp_count == 2) {
          "$$ \\mu_i = \\beta_0 + \\beta_1 \\times GrpB $$"
        } else {
          "$$ \\mu_i = \\beta_0 + \\beta_1 \\times GrpB + \\beta_2 \\times GrpC $$"
        }
      } else {
        eq2 <- "$$ \\mu_i = \\beta_0 $$"
      }
    } else if (mod_fam == "Poisson") {
      eq1 <- "$$ y_i \\sim Poisson(\\lambda_i) $$"
      if (use_x && use_grp) {
        eq2 <- if (grp_count == 2) {
          "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB $$"
        } else {
          "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB + \\beta_3\\times GrpC $$"
        }
      } else if (use_x) {
        eq2 <- "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1\\,x_i $$"
      } else if (use_grp) {
        eq2 <- if (grp_count == 2) {
          "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1 \\times GrpB $$"
        } else {
          "$$ \\log(\\lambda_i) = \\beta_0 + \\beta_1 \\times GrpB + \\beta_2 \\times GrpC $$"
        }
      } else {
        eq2 <- "$$ \\log(\\lambda_i) = \\beta_0 $$"
      }
    } else if (mod_fam %in% c("Binomial", "Bernoulli")) {
      if(mod_fam == "Binomial")
        eq1 <- "$$ y_i \\sim Binomial(n, \\pi_i) $$"
      else
        eq1 <- "$$ y_i \\sim Bernoulli(\\pi_i) $$"
      if (use_x && use_grp) {
        eq2 <- if (grp_count == 2) {
          "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB $$"
        } else {
          "$$ \\mu_i = \\beta_0 + \\beta_1\\,x_i + \\beta_2 \\times GrpB + \\beta_3 \\times GrpC $$"
        }
      } else if (use_x) {
        eq2 <- "$$ logit(\\pi_i) = \\beta_0 + \\beta_1\\,x_i $$"
      } else if (use_grp) {
        eq2 <- if (grp_count == 2) {
          "$$ \\mu_i = \\beta_0 + \\beta_1 \\times GrpB $$"
        } else {
          "$$ \\mu_i = \\beta_0 + \\beta_1 \\times GrpB + \\beta_2 \\times GrpC $$"
        }
      } else {
        eq2 <- "$$ logit(\\pi_i) = \\beta_0 $$"
      }
    }
    
    withMathJax(HTML(paste(eq1, eq2)))
  })
  
  # Raw data figure ---------------------------------------------------------

  output$rawFigures <- renderPlot({ 
    
    df <- simData()
  
    p_hist <- ggplot(df, aes(x = y)) + 
      geom_histogram(fill = "white", color = "grey", bins = 30) + 
      theme_minimal() + 
      theme(
        panel.background = element_blank(), 
        plot.background = element_rect(fill = "#202123"), 
        axis.title = element_text(color = "white", size = 12), 
        axis.text = element_text(color = "white", size = 8), 
        panel.grid.major = element_line(color = "#444654"), 
        panel.grid.minor = element_line(color = "#444654"))
    
    p_scatter <- if (isTRUE(input$sim_use_x)) { 
      ggplot(df, aes(x = x, y = y)) + 
        geom_point(color = "white") + labs(x = "x") + 
        theme_minimal() + 
        theme(
          panel.background = element_blank(), 
          plot.background = element_rect(fill = "#202123"), 
          axis.title = element_text(color = "white", size = 12), 
          axis.text = element_text(color = "white", size = 8), 
          panel.grid.major = element_line(color = "#444654"), 
          panel.grid.minor = element_line(color = "#444654")) 
    } else NULL
    
    p_box <- if (isTRUE(input$sim_use_grp)) { 
      ggplot(df, aes(x = grp, y = y)) + 
        geom_boxplot(fill = "white", color = "grey") + 
        labs(x = "Group") + 
        theme_minimal() + 
        theme(
          panel.background = element_blank(), 
          plot.background = element_rect(fill = "#202123"), 
          axis.title = element_text(color = "white", size = 12), 
          axis.text = element_text(color = "white", size = 8), 
          panel.grid.major = element_line(color = "#444654"), 
          panel.grid.minor = element_line(color = "#444654")) 
    } else NULL
    
    p_unobs_linear <- if (isTRUE(input$sim_unobs_linear) && ("z" %in% names(df))) { 
      ggplot(df, aes(x = z, y = y)) + 
        geom_point(color = "white") + 
        labs(x = "Unobserved linear term") + 
        theme_minimal() + 
        theme(
          panel.background = element_blank(), 
          plot.background = element_rect(fill = "#202123"), 
          axis.title = element_text(color = "white", size = 12), 
          axis.text = element_text(color = "white", size = 8), 
          panel.grid.major = element_line(color = "#444654"), 
          panel.grid.minor = element_line(color = "#444654")) 
    } else NULL
    
    p_unobs_cat <- if (isTRUE(input$sim_unobs_cat) && ("unobs_cat" %in% names(df))) { 
      ggplot(df, aes(x = unobs_cat, y = y)) + 
        geom_boxplot(fill = "white", color = "grey") + 
        labs(x = "Unobserved categorical effect") + 
        theme_minimal() + 
        theme(
          panel.background = element_blank(), 
          plot.background = element_rect(fill = "#202123"), 
          axis.title = element_text(color = "white", size = 12), 
          axis.text = element_text(color = "white", size = 8), 
          panel.grid.major = element_line(color = "#444654"), 
          panel.grid.minor = element_line(color = "#444654")) 
    } else NULL
    
    p_unobs_nonlinear <- if (isTRUE(input$sim_unobs_nonlinear) && ("v" %in% names(df))) { 
      ggplot(df, aes(x = v, y = y)) + 
        geom_point(color = "white") + 
        labs(x = "Unobserved non-linear effect") + 
        theme_minimal() + 
        theme(
          panel.background = element_blank(), 
          plot.background = element_rect(fill = "#202123"), 
          axis.title = element_text(color = "white", size = 12), 
          axis.text = element_text(color = "white", size = 8), 
          panel.grid.major = element_line(color = "#444654"), 
          panel.grid.minor = element_line(color = "#444654")) 
    } else NULL
    
    plot_list <- list(p_hist) 
    
    if (!is.null(p_scatter)) 
      plot_list <- c(plot_list, list(p_scatter)) 
    if (!is.null(p_box)) 
      plot_list <- c(plot_list, list(p_box)) 
    if (!is.null(p_unobs_linear)) 
      plot_list <- c(plot_list, list(p_unobs_linear)) 
    if (!is.null(p_unobs_cat)) 
      plot_list <- c(plot_list, list(p_unobs_cat)) 
    if (!is.null(p_unobs_nonlinear)) 
      plot_list <- c(plot_list, list(p_unobs_nonlinear))
    
    combined_plot <- wrap_plots(plot_list, ncol = 2) + 
      plot_annotation(
        theme = theme(plot.background = element_rect(fill = "#202123", color = "#202123"))
      )
    
    combined_plot
    
  })
  
  # Data table --------------------------------------------------------------
  
  output$rawData <- DT::renderDataTable({ 
    df <- simData() 
    cols <- "y"
    if (isTRUE(input$sim_use_x)) cols <- c(cols, "x")
    if (isTRUE(input$sim_use_grp)) cols <- c(cols, "grp")
    if (isTRUE(input$sim_unobs_linear)) cols <- c(cols, "z")
    if (isTRUE(input$sim_unobs_cat)) cols <- c(cols, "unobs_cat")
    if (isTRUE(input$sim_unobs_nonlinear)) cols <- c(cols, "v")
    
    DT::datatable(df[, cols, drop = FALSE], options = list(pageLength = 10)) |> 
      DT::formatStyle( columns = cols, backgroundColor = "#202123", color = "white", border = "1px solid grey" ) 
  })
  
}

# Run App -------------------------------------------------------------
shinyApp(ui = ui, server = server)
