library(shiny)
library(ggplot2)
library(tweedie)
library(shinyWidgets)
library(LaplacesDemon)
library(extraDistr)
library(shinydashboard)

# Default parameter values
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

# Default x-axis interval for visualization
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

# List of discrete distributions for which we want as.integer(x) in PMF
discrete_dists <- c("Poisson", "Binomial", "Bernoulli", "ZIP", "Negative Binomial")

# Mapping from distribution names to density (or PMF) functions
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

# Mapping from distribution names to RNG functions
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

# Helper: extract parameters for a given parameter set and distribution
getParams <- function(dist, i, input) {
  switch(dist,
         Normal = list(mean = input[[paste0("mean_", i)]],
                       sd = input[[paste0("sd_", i)]]),
         Poisson = list(lambda = input[[paste0("lambda_", i)]]),
         Uniform = list(min = input[[paste0("min_", i)]],
                        max = input[[paste0("max_", i)]]),
         Beta = list(alpha = input[[paste0("alpha_", i)]],
                     beta = input[[paste0("beta_", i)]]),
         `Student's T` = list(df = input[[paste0("df_", i)]]),
         Exponential = list(rate = input[[paste0("rate_", i)]]),
         Gamma = list(shape = input[[paste0("shape_", i)]],
                      rate = input[[paste0("rate_", i)]]),
         `Log-Normal` = list(meanlog = input[[paste0("meanlog_", i)]],
                             sdlog = input[[paste0("sdlog_", i)]]),
         `Half-Cauchy` = list(scale = input[[paste0("scale_", i)]]),
         Tweedie = list(p = input[[paste0("p_", i)]],
                        mu = input[[paste0("mu_", i)]],
                        phi = input[[paste0("phi_", i)]]),
         Wald = list(mu = input[[paste0("mu_", i)]],
                     lambda = input[[paste0("lambda_", i)]]),
         Binomial = list(size = input[[paste0("size_", i)]],
                         prob = input[[paste0("prob_", i)]]),
         Bernoulli = list(p = input[[paste0("p_", i)]]),
         ZIP = list(lambda = input[[paste0("lambda_", i)]],
                    pi = input[[paste0("pi_", i)]]),
         `Negative Binomial` = list(size = input[[paste0("size_", i)]],
                                    mu = input[[paste0("mu_", i)]])
  )
}

# Helper: generate UI for distribution-specific parameter inputs for one parameter set
paramInputsForDist <- function(dist, i, defaults) {
  switch(dist,
         Normal = tagList(
           numericInput(paste0("mean_", i), "Mean (μ):", value = defaults$mean, step = 0.1),
           numericInput(paste0("sd_", i), "Standard Deviation (σ):", value = defaults$sd, step = 0.1)
         ),
         Poisson = tagList(
           numericInput(paste0("lambda_", i), "Lambda (λ):", value = defaults$lambda, step = 1)
         ),
         Uniform = tagList(
           numericInput(paste0("min_", i), "Minimum (a):", value = defaults$min, step = 0.1),
           numericInput(paste0("max_", i), "Maximum (b):", value = defaults$max, step = 0.1)
         ),
         Beta = tagList(
           numericInput(paste0("alpha_", i), "Shape α:", value = defaults$alpha, step = 0.1),
           numericInput(paste0("beta_", i), "Shape β:", value = defaults$beta, step = 0.1)
         ),
         `Student's T` = tagList(
           numericInput(paste0("df_", i), "Degrees of Freedom (ν):", value = defaults$df, step = 1)
         ),
         Exponential = tagList(
           numericInput(paste0("rate_", i), "Rate (λ):", value = defaults$rate, step = 0.1)
         ),
         Gamma = tagList(
           numericInput(paste0("shape_", i), "Shape (α):", value = defaults$shape, step = 0.1),
           numericInput(paste0("rate_", i), "Rate (λ):", value = defaults$rate, step = 0.1)
         ),
         `Log-Normal` = tagList(
           numericInput(paste0("meanlog_", i), "Log-mean (μ):", value = defaults$meanlog, step = 0.1),
           numericInput(paste0("sdlog_", i), "Log-standard Deviation (σ):", value = defaults$sdlog, step = 0.1)
         ),
         `Half-Cauchy` = tagList(
           numericInput(paste0("scale_", i), "Scale (α):", value = defaults$scale, step = 0.1)
         ),
         Tweedie = tagList(
           numericInput(paste0("p_", i), "Power (p):", value = defaults$p, step = 0.1),
           numericInput(paste0("mu_", i), "Mean (μ):", value = defaults$mu, step = 0.1),
           numericInput(paste0("phi_", i), "Dispersion (φ):", value = defaults$phi, step = 0.1)
         ),
         Wald = tagList(
           numericInput(paste0("mu_", i), "Mean (μ):", value = defaults$mu, step = 0.1),
           numericInput(paste0("lambda_", i), "Lambda (λ):", value = defaults$lambda, step = 0.1)
         ),
         Binomial = tagList(
           numericInput(paste0("size_", i), "Number of Trials (n):", value = defaults$size, step = 1),
           numericInput(paste0("prob_", i), "Success Probability (p):", value = defaults$prob, min = 0, max = 1, step = 0.1)
         ),
         Bernoulli = tagList(
           numericInput(paste0("p_", i), "Probability (p):", value = defaults$p, min = 0, max = 1, step = 0.1)
         ),
         ZIP = tagList(
           numericInput(paste0("lambda_", i), "Lambda (λ):", value = defaults$lambda, step = 0.1),
           numericInput(paste0("pi_", i), "Zero Inflation Probability (π):", value = defaults$pi, min = 0, max = 1, step = 0.1)
         ),
         `Negative Binomial` = tagList(
           numericInput(paste0("size_", i), "Dispersion Parameter (n):", value = defaults$size, min = 0, step = 1),
           numericInput(paste0("mu_", i), "Mean (μ):", value = defaults$mu, min = 0, step = 0.1)
         )
  )
}

# UI definition
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
                 label = "Select distribution",
                 choices = list(
                   "Continuous real" = c("Normal", sort(c("Uniform", "Beta", "Student's T", "Half-Cauchy"))),
                   "Continuous positive" = c("Exponential", "Tweedie", "Gamma", "Log-Normal", "Wald"),
                   "Discrete" = sort(c("Poisson", "Binomial", "Bernoulli", "ZIP", "Negative Binomial"))
                 ),
                 options = list(`live-search` = TRUE)
               ),
               tags$hr(style = "border-color: #00A68A;"),
               h4("X-axis interval for visualization"),
               numericInput("xmin", "Minimum x-value:", value = range_defaults[["Normal"]]$xmin, step = 0.5),
               numericInput("xmax", "Maximum x-value:", value = range_defaults[["Normal"]]$xmax, step = 0.5),
               numericInput("nobs", "Number of random values:", value = 100, min = 1, max = 5000, step = 25),
               tags$hr(style = "border-color: #00A68A;"),
               h4("Want to compare different parameter values?"),
               fluidRow(
                 column(6, actionButton("addSet", "Add Parameter Set")),
                 column(6, actionButton("removeSet", "Remove Parameter Set"))
               ),
               br(),
               uiOutput("setCountDisplay"),
               tags$hr(style = "border-color: #00A68A;"),
               h4("Distribution specific parameters"),
               uiOutput("distributionParams"),
               actionButton("resetDefaults", "Reset to Defaults")
             ),
             mainPanel(
               fluidRow(
                 column(6,
                        div(id = "myTextOutput", textOutput("distName")),
                        plotOutput("densityPlot"),
                        br(),
                        plotOutput("randomPlot"),
                 ),
                 column(6,
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
                 label = "Select multiple distributions",
                 choices = list(
                   "Continuous real" = c("Normal", sort(c("Uniform", "Beta", "Student's T", "Half-Cauchy"))),
                   "Continuous positive" = c("Exponential", "Tweedie", "Gamma", "Log-Normal", "Wald"),
                   "Discrete" = sort(c("Poisson", "Binomial", "Bernoulli", "ZIP", "Negative Binomial"))
                 ),
                 multiple = TRUE,
                 selected = c("Normal", "Beta"),
                 options = list(`live-search` = TRUE)
               ),
               h4("X-axis interval for visualization"),
               numericInput("xminmulti", "Minimum x-value:", value = -10, step = 0.5),
               numericInput("xmaxmulti", "Maximum x-value:", value = 10, step = 0.5),
               uiOutput("paramInputs")
             ),
             mainPanel(
               h4("Multi-distribution Comparison Plot"),
               plotOutput("densityPlotMulti"),
               h6("Densities are standardized (peak = 1)."),
               tags$hr(style = "border-color: #00A68A;"),
               p("This app was created by Deon Roos. The code is available on GitHub:"),
               tags$a(href = "https://github.com/DeonRoos/Statistical_Distributions", "GitHub Repo")
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

# Server logic
server <- function(input, output, session) {
  
  # Reactive value for number of parameter sets (min 1, max 10)
  parameterSetCount <- reactiveVal(1)
  
  observeEvent(input$addSet, {
    if (parameterSetCount() < 10)
      parameterSetCount(parameterSetCount() + 1)
  })
  
  observeEvent(input$removeSet, {
    if (parameterSetCount() > 1)
      parameterSetCount(parameterSetCount() - 1)
  })
  
  output$setCountDisplay <- renderUI({
    tags$p(strong("Current number of Parameter Sets:"), parameterSetCount())
  })
  
  # Generate distribution-specific parameter inputs for the Explore tab
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
  
  output$densityPlot <- renderPlot({
    dist <- input$distribution
    xmin <- input$xmin
    xmax <- input$xmax
    count <- parameterSetCount()
    all_dfs <- lapply(seq_len(count), function(i) {
      p <- getParams(dist, i, input)
      x <- if (dist %in% discrete_dists) {
        # Create a sequence with 100 points between integer-min and integer-max
        seq(ceiling(xmin), floor(xmax), length.out = 100)
      } else {
        seq(xmin, xmax, length.out = 100)
      }
      # For discrete distributions, call density function with as.integer(x)
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
  
  
  output$distName <- renderText({
    paste("The", input$distribution, "distribution")
  })
  
  # Generate and render the random values histogram
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
      geom_histogram(position = "dodge", binwidth = if(dist %in% discrete_dists) 1 else NULL, alpha = 0.3) +
      scale_x_continuous(limits = c(xmin, xmax)) +
      xlab("Realised values (x)") +
      ylab("Frequency") +
      scale_color_brewer(palette = "Set2") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(panel.background = element_blank(),
            plot.background = element_rect(fill = "#202123"),
            plot.title = element_text(color = "white", size = 16),
            axis.text = element_text(color = "white", size = 12),
            axis.title = element_text(color = "white", size = 14),
            legend.title = element_text(color = "white", size = 12),
            legend.text = element_text(color = "white", size = 10),
            panel.grid.major = element_line(color = "#444654"),
            panel.grid.minor = element_line(color = "#444654"))
  })
  
  # Update x-axis interval when distribution is changed
  observeEvent(input$distribution, {
    dist <- input$distribution
    rd <- range_defaults[[dist]]
    updateNumericInput(session, "xmin", value = rd$xmin)
    updateNumericInput(session, "xmax", value = rd$xmax)
  })
  
  # Reset parameters to defaults for the Explore tab
  observeEvent(input$resetDefaults, {
    dist <- input$distribution
    defaults <- default_params[[dist]]
    rd <- range_defaults[[dist]]
    updateNumericInput(session, "xmin", value = rd$xmin)
    updateNumericInput(session, "xmax", value = rd$xmax)
    count <- parameterSetCount()
    for(i in seq_len(count)) {
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
  
  # Render rich HTML brief summary for each distribution
  output$distributionText <- renderUI({
    distribution <- input$distribution
    summary_text <- switch(distribution,
                           "Normal" = HTML("
        <b>The Normal Distribution (Gaussian):</b><br>
        The Normal distribution is one of the most fundamental probability distributions in statistics. It is defined by a mathematical formula that approximates a wide variety of datasets—ranging from human traits (like height and weight) to test scores. Unlike a uniform distribution, which treats all outcomes equally, the Normal follows a distinct “bell curve” pattern.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Bell-Shaped and Symmetric:</b> Values tend to cluster around the mean, forming a symmetric curve. As you move away from the mean, values become increasingly rare.</li>
          <li><b>Controlled by the Standard Deviation:</b> The spread is determined by the standard deviation. Smaller values yield a steep, narrow curve; larger ones produce a flat, wide curve.</li>
          <li><b>Range:</b> Covers all real numbers from negative infinity to positive infinity.</li>
        </ul>
        <b>When to Consider:</b> For continuous data that cluster around a central value and can extend in both directions.
      "),
      "Poisson" = HTML("
        <b>The Poisson Distribution:</b><br>
        The Poisson distribution is used to model the number of times an event occurs in a fixed interval when events occur independently at a constant rate.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Discrete Count Data:</b> Outputs are non-negative integers.</li>
          <li><b>Mean Equals Variance:</b> The average rate of events is equal to the variance.</li>
          <li><b>Unbounded Above:</b> Counts start at zero but can extend indefinitely.</li>
        </ul>
        <b>When to Consider:</b> Ideal for modeling counts of rare events, such as emails per hour or customer arrivals.
      "),
      "Uniform" = HTML("
        <b>The Uniform Distribution:</b><br>
        The Uniform distribution gives every value in a specified interval an equal chance of occurring, resulting in a flat, rectangular shape.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Equal Likelihood:</b> All outcomes within the range are equally probable.</li>
          <li><b>Constant Density:</b> The probability density is uniform across the interval.</li>
          <li><b>Defined Range:</b> Only values within two endpoints are possible.</li>
        </ul>
        <b>When to Consider:</b> When there is no reason to favor any outcome over another within a given range.
      "),
      "Beta" = HTML("
        <b>The Beta Distribution:</b><br>
        The Beta distribution is defined on the interval [0, 1] and is excellent for modeling probabilities or proportions.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Bounded:</b> Values lie strictly between 0 and 1.</li>
          <li><b>Flexible Shape:</b> Two parameters allow the distribution to take on various forms (e.g., U-shaped, uniform, bell-shaped).</li>
        </ul>
        <b>When to Consider:</b> Ideal for modeling conversion rates, proportions, or any metric naturally restricted to [0, 1].
      "),
      "Student's T" = HTML("
        <b>The Student's T Distribution:</b><br>
        The Student's T distribution is similar to the Normal distribution but has heavier tails, making it crucial when sample sizes are small or variance is uncertain.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavy Tails:</b> Increases the likelihood of extreme values compared to the Normal distribution.</li>
          <li><b>Degrees of Freedom:</b> As sample size increases, it approaches the Normal distribution.</li>
        </ul>
        <b>When to Consider:</b> Useful for small samples or when there is uncertainty about the population variance.
      "),
      "Binomial" = HTML("
        <b>The Binomial Distribution:</b><br>
        The Binomial distribution models the number of successes in a fixed number of independent trials with the same probability of success.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Discrete and Limited:</b> Generates integer outcomes from 0 up to the number of trials.</li>
          <li><b>Fixed Number of Trials:</b> The total number of experiments is predetermined.</li>
        </ul>
        <b>When to Consider:</b> For binary (yes/no) outcomes in a fixed number of independent trials.
      "),
      "Exponential" = HTML("
        <b>The Exponential Distribution:</b><br>
        The Exponential distribution models the time between successive random events in a continuous, memoryless process.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Memoryless:</b> Future event probabilities do not depend on past events.</li>
          <li><b>Continuous &amp; Non-negative:</b> Defined for x ≥ 0.</li>
        </ul>
        <b>When to Consider:</b> For modeling waiting times or durations until an event occurs.
      "),
      "Gamma" = HTML("
        <b>The Gamma Distribution:</b><br>
        The Gamma distribution generalizes the Exponential to model the waiting time until multiple events occur, defined by shape and rate parameters.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Continuous &amp; Non-negative:</b> Suitable for modeling time or duration.</li>
          <li><b>Flexible Shape:</b> Two parameters allow for various forms of skewed data.</li>
        </ul>
        <b>When to Consider:</b> When the time until an event depends on the occurrence of several events.
      "),
      "Log-Normal" = HTML("
        <b>The Log-Normal Distribution:</b><br>
        The Log-Normal distribution applies when the logarithm of the data follows a Normal distribution. It is ideal for strictly positive, skewed data.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Positively Skewed:</b> Most values are low with a long tail to the right.</li>
          <li><b>Multiplicative Processes:</b> Often emerges from the product of many independent factors.</li>
        </ul>
        <b>When to Consider:</b> For phenomena such as incomes, stock prices, or biological measurements that cannot be negative.
      "),
      "Half-Cauchy" = HTML("
        <b>The Half-Cauchy Distribution:</b><br>
        The Half-Cauchy distribution is defined only for positive values and is known for its heavy tails. It is frequently used as a prior for scale parameters in Bayesian analysis.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavy Tails:</b> Can accommodate occasional extreme values.</li>
          <li><b>Restricted to Positive Values:</b> Suitable only for non-negative measurements.</li>
        </ul>
        <b>When to Consider:</b> When modeling a scale parameter where data may have outliers.
      "),
      "Tweedie" = HTML("
        <b>The Tweedie Distribution:</b><br>
        The Tweedie distribution is a flexible family that combines elements of both discrete and continuous models, particularly useful when data have many zeros alongside positive values.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Compound Nature:</b> Blends features of Poisson (for zero counts) and Gamma (for positive values).</li>
          <li><b>Handles Excess Zeros:</b> Specifically designed for data with a surplus of zeros.</li>
        </ul>
        <b>When to Consider:</b> For modeling data like insurance claims or rainfall intensities where zeros are common.
      "),
      "Wald" = HTML("
        <b>The Wald Distribution (Inverse Gaussian):</b><br>
        The Wald distribution (or Inverse Gaussian) models positive, skewed data, often used in reliability and response time analysis.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Positively Skewed:</b> Emphasizes lower values with a long right tail.</li>
          <li><b>Time-to-Event Focus:</b> Suitable for modeling the duration until an event occurs.</li>
        </ul>
        <b>When to Consider:</b> When dealing with strictly positive data that exhibit a strong skew, such as reaction times.
      "),
      "ZIP" = HTML("
        <b>The Zero-Inflated Poisson (ZIP) Distribution:</b><br>
        The ZIP distribution extends the Poisson to account for an excess number of zero counts by combining a point mass at zero with a standard Poisson for positive counts.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Excess Zeros:</b> Explicitly models a greater probability for zero outcomes.</li>
          <li><b>Discrete:</b> Suitable for count data.</li>
        </ul>
        <b>When to Consider:</b> For count data with more zeros than a standard Poisson model would predict.
      "),
      "Bernoulli" = HTML("
        <b>The Bernoulli Distribution:</b><br>
        The Bernoulli distribution represents the simplest binary experiment with two outcomes: success (1) or failure (0).<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Binary Outcome:</b> Only two possible results.</li>
          <li><b>Foundational:</b> Forms the basis for more complex models like the Binomial distribution.</li>
        </ul>
        <b>When to Consider:</b> For modeling the outcome of a single yes/no or success/failure experiment.
      "),
      "Negative Binomial" = HTML("
        <b>The Negative Binomial Distribution:</b><br>
        The Negative Binomial distribution is used for overdispersed count data, where the variance exceeds the mean. It models the number of failures before a predetermined number of successes occur.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Overdispersion:</b> Captures extra variability not explained by a Poisson model.</li>
          <li><b>Discrete:</b> Produces integer outcomes with flexible variability.</li>
        </ul>
        <b>When to Consider:</b> For count data that exhibit more variability than a Poisson model can capture.
      ")
    )
    summary_text
  })
  
  # Render distribution notation
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
  
  # Render PDF/PMF description
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
  
  # Compare tab: revert to original parameter inputs (one per distribution selected)
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
  
  # Compare tab: density plot
  output$densityPlotMulti <- renderPlot({
    selected_distributions <- input$selectedDistributions
    if (is.null(selected_distributions) || length(selected_distributions) == 0) {
      return(NULL)
    }
    df_density <- data.frame(x = numeric(0), density = numeric(0), distribution = character(0), stringsAsFactors = FALSE)
    for (dist in selected_distributions) {
      params <- default_params[[dist]]
      param_values <- reactiveValuesToList(input)
      # extract parameter inputs that end with the distribution name
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
      ggtitle("Multiple Distribution Density Plot") +
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
  
  output$setCountDisplay <- renderUI({
    tags$p(strong("Current number of Parameter Sets:"), parameterSetCount())
  })
  
  output$distName <- renderText({
    paste("The", input$distribution, "distribution")
  })
  
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
        The Student's T distribution resembles the Normal distribution but with heavier tails, making it valuable when sample sizes are small or when variance is uncertain.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavier Tails:</b> More likelihood for extreme values than the Normal distribution.</li>
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
        <b>When to Consider:</b> For variables such as incomes, stock prices, or biological measurements that cannot be negative.
      "),
      "Half-Cauchy" = HTML("
        <b>The Half-Cauchy Distribution:</b><br>
        The Half-Cauchy distribution is defined for positive values and has heavy tails, making it popular as a prior for scale parameters in Bayesian analysis.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Heavy Tails:</b> Accommodates occasional extreme observations.</li>
          <li><b>Only Positive:</b> Suitable for non-negative data.</li>
        </ul>
        <b>When to Consider:</b> When uncertainty in a scale parameter is high and outliers may be present.
      "),
      "Tweedie" = HTML("
        <b>The Tweedie Distribution:</b><br>
        The Tweedie distribution is a flexible family that can combine discrete and continuous outcomes—especially useful when data contain many zeros along with positive values.<br><br>
        <b>Key Characteristics:</b>
        <ul>
          <li><b>Compound Structure:</b> Merges features of the Poisson (for zeros/counts) with the Gamma (for continuous values).</li>
          <li><b>Handles Excess Zeros:</b> Explicitly accounts for a surplus of zero outcomes.</li>
        </ul>
        <b>When to Consider:</b> For data such as insurance claims or rainfall intensities where zeros are common.
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
        <b>When to Consider:</b> For modeling a single trial outcome.
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
  
  # Render distribution notation
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
  
  # Render PDF/PMF description for the Explore tab
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
  
  output$distributionEG <- renderText({
    distribution <- input$distribution
    text <- switch(distribution,
                   "Normal" = "Examples: Plant height, exam scores, IQ scores, blood pressure readings.",
                   "Poisson" = "Examples: Number of phone calls per hour, customer arrivals, manufacturing defects.",
                   "Uniform" = "Examples: Random lottery numbers, random sampling within a fixed range.",
                   "Beta" = "Examples: Conversion rates, proportion of defective items, survey proportions.",
                   "Student's T" = "Examples: Test scores from small samples, reaction times, or small-sample measurements.",
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
}

shinyApp(ui = ui, server = server)
