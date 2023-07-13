library(shiny)
library(ggplot2)
library(tweedie)
library(shinyWidgets)
library(LaplacesDemon)
library(extraDistr)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      /* Global styles */
      body {
        background-color: #202123 !important;
        color: white;
      }

      /* Main panel styles */
      .mainPanel {
        background-color: #202123 !important;
      }

      /* Text color styles */
      h4, h6, p, .form-control-static, .text-output {
        color: white;
      }
      ")
    )
  ),
  titlePanel("Statistical Distributions"),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      style = "background-color: #444654;
      border-radius: 10px;
        border: 2px solid #00A68A;
        box-shadow: 0 0 10px 5px rgba(0, 166, 138, 0.3);",
      pickerInput(
        inputId = "distribution",
        label = "Select distribution",
        choices = list(
          Continuous = c("Normal", "Uniform", "Beta", "T", "Exponential", "Gamma", "Log-Normal", "Half-Cauchy", "Tweedie", "Wald"),
          Discrete = c("Poisson", "Binomial", "Bernoulli", "ZIP")
        ),
        options = list(
          `live-search` = TRUE
        )
      ),
      tags$hr(style = "border-color: white;"),
      h4("Range of possible values"),
      h6("Below you may specify the minimum and maximum values within which you would like to know the probability density mass"),
      numericInput("xmin", "Minimum value to view:", value = -10),
      numericInput("xmax", "Maximum value to view:", value = 10),
      tags$hr(style = "border-color: white;"),
      h4("Select the number of parameter combinations you wish to be shown simultaneously"),
      numericInput("numCombinations", "Number of combinations:", value = 1, min = 1, max = 10),
      tags$hr(style = "border-color: white;"),
      h4("Distribution specific parameters"),
      uiOutput("distributionParams")
    ),
    mainPanel(
      id = "main",
      h4("Summary of distribution"),
      textOutput("distributionText"),
      br(),
      h4("Examples of data generated from distribution"),
      textOutput("distributionEG"),
      br(),
      plotOutput("densityPlot"),
      br(),
      h4("Distributrion details"),
      h6("Notation:"),
      uiOutput("distributionNote"),
      h6("where the probability mass function is given as:"),
      uiOutput("distributionPMF"),
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Generate distribution-specific parameter inputs
  output$distributionParams <- renderUI({
    distribution <- input$distribution
    numCombinations <- input$numCombinations

    params_ui <- lapply(1:numCombinations, function(i) {
      tagList(
        h4(paste0("Combination ", i)),
        switch(distribution,
          "Normal" = tagList(
            numericInput(paste0("mean_", i), withMathJax(helpText("Mean (\\(\\mu\\)):")), value = 0),
            numericInput(paste0("sd_", i), withMathJax(helpText("Standard Deviation (\\(\\sigma\\)):")), value = 1)
          ),
          "Poisson" = tagList(
            numericInput(paste0("lambda_", i), "Lambda:", value = 3, step = 1),
            h6("where the probability mass function is given as:")
          ),
          "Uniform" = tagList(
            numericInput(paste0("min_", i), withMathJax(helpText("Minimum (\\(a\\)):")), value = -3),
            numericInput(paste0("max_", i), withMathJax(helpText("Maximum (\\(b\\)):")), value = 3)
          ),
          "Beta" = tagList(
            numericInput(paste0("alpha_", i), withMathJax(helpText("Shape 1 (\\(\\alpha\\)):")), value = 3),
            numericInput(paste0("beta_", i), withMathJax(helpText("Shape 2 (\\(\\beta\\)):")), value = 2)
          ),
          "T" = tagList(
            numericInput(paste0("df_", i), withMathJax(helpText("Degrees of freedom (\\(\\nu\\)):")), value = 2)
          ),
          "Binomial" = tagList(
            numericInput(paste0("size_", i), withMathJax(helpText("Number of trials (\\(n\\)):")), value = 10),
            numericInput(paste0("prob_", i), withMathJax(helpText("Success probability (\\(p\\)):")), value = 0.7, min = 0, max = 1, step = 0.1)
          ),
          "Exponential" = tagList(
            numericInput(paste0("rate_", i), withMathJax(helpText("Rate (\\(\\lambda\\)):")), value = 2)
          ),
          "Gamma" = tagList(
            numericInput(paste0("shape_", i), withMathJax(helpText("Rate (\\(\\alpha\\)):")), value = 2),
            numericInput(paste0("rate_gamma_", i), withMathJax(helpText("Rate (\\(\\beta\\)):")), value = 1)
          ),
          "Log-Normal" = tagList(
            numericInput(paste0("meanlog_", i), withMathJax(helpText("Log mean (\\(\\mu\\)):")), value = 0),
            numericInput(paste0("sdlog_", i), withMathJax(helpText("Log standard deviation (\\(\\sigma\\)):")), value = 1)
          ),
          "Half-Cauchy" = tagList(
            numericInput(paste0("scale_", i), withMathJax(helpText("Scale (\\(\\alpha\\)):")), value = 1)
          ),
          "Tweedie" = tagList(
            numericInput(paste0("p_", i), withMathJax(helpText("Power (\\(p\\)):")), value = 2),
            numericInput(paste0("mu_", i), withMathJax(helpText("Mean (\\(\\mu\\)):")), value = 3),
            numericInput(paste0("phi_", i), withMathJax(helpText("Variance (\\(\\phi\\)):")), value = 3)
          ),
          "Wald" = tagList(
            numericInput(paste0("mu_", i), withMathJax(helpText("Mean (\\(\\mu\\)):")), value = 3),
            numericInput(paste0("lambda_", i), withMathJax(helpText("Shape (\\(\\lambda\\)):")), value = 3)
          ),
          "ZIP" = tagList(
            numericInput(paste0("lambda_", i), withMathJax(helpText("Rate (\\(\\lambda\\)):")), value = 1),
            numericInput(paste0("pi_", i), withMathJax(helpText("Probability (\\(\\pi\\)):")), value = 0.2, min = 0, max = 1, step = 0.1)
          ),
          "Bernoulli" = tagList(
            numericInput(paste0("p_", i), withMathJax(helpText("Probability (\\(p\\)):")), value = 1, min = 0, max = 1, step = 0.1)
          )
        )
      )
    })

    do.call(tagList, params_ui)
  })

  output$distributionPMF <- renderUI({
    distribution <- input$distribution

    note <- switch(distribution,
      "Normal" = tagList(
        withMathJax(paste0("$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{1}{2}(\\frac{x-\\mu}{\\sigma})^2}$$"))
      ),
      "Poisson" = tagList(
        withMathJax(paste0("$$f(x) = \\frac{\\lambda^ke^{-\\lambda}}{k!}$$"))
      ),
      "Uniform" = tagList(
        withMathJax(paste0("$$f(x)=\\frac{1}{b-a}$$"))
      ),
      "Beta" = tagList(
        withMathJax(paste0("$$f(x) = \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}x^{\\alpha-1}(1-x)^{\\beta-1}$$"))
      ),
      "T" = tagList(
        withMathJax(paste0("$$f(x) = \\frac{\\Gamma(\\frac{\\nu + 1}{2})}{\\sqrt{\\nu\\pi}\\Gamma(\\frac{\\nu}{2})}(1+\\frac{x^2}{\\nu})^{-\\frac{\\nu+1}{2}}$$"))
      ),
      "Binomial" = tagList(
        withMathJax(paste0("$$f(x) = \\binom{n}{k}p^k(1-p)^{n-k}$$"))
      ),
      "Exponential" = tagList(
        withMathJax(paste0("$$f(x) = \\lambda e^{-\\lambda x}$$"))
      ),
      "Gamma" = tagList(
        withMathJax(paste0("$$f(x) = \\frac{1}{\\beta^\\alpha \\Gamma(\\alpha)}x^{\\alpha-1}e^{\\frac{-x}{\\beta}}$$"))
      ),
      "Log-Normal" = tagList(
        withMathJax(paste0("$$f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma x}}e^{\\frac{-(log(x)-\\mu)^2}{2\\sigma^2}}$$"))
      ),
      "Half-Cauchy" = tagList(
        withMathJax(paste0("$$f(x) = \\frac{2 \\alpha}{\\pi(x^2+\\alpha^2)}$$"))
      ),
      "Tweedie" = tagList(
        withMathJax(paste0("$$f(x) = h(\\sigma^2, x)exp[\\frac{px - A(p)}{\\sigma^2}]$$")),
      ),
      "Wald" = tagList(
        withMathJax(paste0("$$f(x) = \\sqrt{\\frac{\\lambda}{2\\pi x^2}}exp(\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x}$$"))
      ),
      "ZIP" = tagList(
        withMathJax(
          helpText("$$f(x)=\\begin{cases}
                               \\pi + (1 - \\pi) e^{-\\lambda},  & x = 0 \\\\
                               (1 - \\pi)\\frac{\\lambda^xe^{-\\lambda}}{x!}, & x > 0
                             \\end{cases}\\!$$")
        )
      ),
      "Bernoulli" = tagList(
        withMathJax(paste0("$$f(x) = pl + (1-p)(1-k)$$"))
      )
    )

    do.call(tagList, note)
  })

  # Generate and render the density plot
  output$densityPlot <- renderPlot({
    distribution <- input$distribution
    xmin <- input$xmin
    xmax <- input$xmax

    plot_data <- lapply(1:input$numCombinations, function(i) {
      switch(distribution,
        "Normal" = {
          x <- seq(xmin, xmax, length.out = 100)
          dnorm(x, mean = input[[paste0("mean_", i)]], sd = input[[paste0("sd_", i)]])
        },
        "Poisson" = {
          x <- seq(xmin, xmax, length.out = 100)
          dpois(as.integer(x), lambda = input[[paste0("lambda_", i)]])
        },
        "Uniform" = {
          x <- seq(xmin, xmax, length.out = 100)
          dunif(x, min = input[[paste0("min_", i)]], max = input[[paste0("max_", i)]])
        },
        "Beta" = {
          x <- seq(xmin, xmax, length.out = 100)
          dbeta(x, shape1 = input[[paste0("alpha_", i)]], shape2 = input[[paste0("beta_", i)]])
        },
        "T" = {
          x <- seq(xmin, xmax, length.out = 100)
          dt(x, df = input[[paste0("df_", i)]])
        },
        "Binomial" = {
          x <- seq(xmin, xmax, length.out = 100)
          dbinom(as.integer(x), size = input[[paste0("size_", i)]], prob = input[[paste0("prob_", i)]])
        },
        "Exponential" = {
          x <- seq(xmin, xmax, length.out = 100)
          dexp(x, rate = input[[paste0("rate_", i)]])
        },
        "Gamma" = {
          x <- seq(xmin, xmax, length.out = 100)
          dgamma(x, shape = input[[paste0("shape_", i)]], rate = input[[paste0("rate_gamma_", i)]])
        },
        "Log-Normal" = {
          x <- seq(xmin, xmax, length.out = 100)
          dlnorm(x, meanlog = input[[paste0("meanlog_", i)]], sdlog = input[[paste0("sdlog_", i)]])
        },
        "Half-Cauchy" = {
          x <- seq(xmin, xmax, length.out = 100)
          dhalfcauchy(x, scale = input[[paste0("scale_", i)]])
        },
        "Tweedie" = {
          x <- seq(xmin, xmax, length.out = 100)
          dtweedie(x, p = input[[paste0("p_", i)]], mu = input[[paste0("mu_", i)]], phi = input[[paste0("phi_", i)]])
        },
        "Wald" = {
          x <- seq(xmin, xmax, length.out = 100)
          dwald(x, mu = input[[paste0("mu_", i)]], lambda = input[[paste0("lambda_", i)]])
        },
        "ZIP" = {
          x <- seq(xmin, xmax, length.out = 100)
          dzip(as.integer(x), lambda = input[[paste0("lambda_", i)]], pi = input[[paste0("pi_", i)]])
        },
        "Bernoulli" = {
          x <- seq(xmin, xmax, length.out = 100)
          dbern(as.integer(x), p = input[[paste0("p_", i)]])
        }
      )
    })

    data <- data.frame(x = seq(xmin, xmax, length.out = 100), density = unlist(plot_data))
    data$combination <- rep(1:input$numCombinations, each = 100)

    ggplot(data, aes(x = x, y = density, group = combination, color = as.factor(combination))) +
      geom_line(size = 1) +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") +
      # scale_y_continuous(labels = scales::percent) +
      ggtitle(paste(input$distribution, "Distribution")) +
      xlab("Value generated from current distribution") +
      ylab("Density of value in hypothetical data") +
      guides(color = guide_legend(title = "Combination")) +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#202123"),
        plot.title = element_text(color = "white", size = 16),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 14),
        legend.title = element_text(color = "white", size = 12),
        legend.text = element_text(color = "white", size = 10),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "#444654"),
        panel.grid.minor = element_line(color = "#444654")
      )
  })

  # Generate and render distribution-specific text
  output$distributionText <- renderText({
    distribution <- input$distribution

    text <- switch(distribution,
      "Normal" = "The Normal (or Gaussian) distribution is a continuous probability distribution for real-valued random variables.",
      "Poisson" = "The Poisson distribution is a discrete probability distribution for count data.",
      "Uniform" = "The Uniform distribution is a bounded continuous probability distribution.",
      "Beta" = "The Beta distribution is a continuous probability distribution defined on the interval [0,1].",
      "T" = "The T distribution is a continuous probability distribution similar to the Normal distribution but with greater variation at the extremes.",
      "Binomial" = "The Binomial distribution is a discrete probability distribution for binary outcomes with a fixed number of trials.",
      "Exponential" = "The Exponential distribution is a continuous probability distribution for the time between events in a Poisson process.",
      "Gamma" = "The Gamma distribution is a continuous probability distribution often used for modeling waiting times or duration until an event.",
      "Log-Normal" = "The Log-Normal distribution is a continuous probability distribution for positive variables whose logarithm follows a normal distribution.",
      "Half-Cauchy" = "The Half-Cauchy distribution is a continuous probability distribution with heavy tails.",
      "Tweedie" = "The Tweedie distribution is a continuous probability distribution often used for modeling data with excess zeros and overdispersion.",
      "Wald" = "The Wald distribution, also known as the Inverse Gaussian distribution, is a continuous probability distribution used for modeling positive variables with skewed distributions.",
      "ZIP" = "The Zero-Inflated Poisson (ZIP) distribution is a discrete probability distribution that combines a Poisson distribution with excess zeros. It is often used to model count data with an excess number of zero values.",
      "Bernoulli" = "The Bernoulli distribution is a discrete probability distribution that represents a special case of a Binomial whereby there is a single trial (rather than > 1)."
    )

    text
  })

  output$distributionEG <- renderText({
    distribution <- input$distribution

    text <- switch(distribution,
      "Normal" = "Weight, change in weight over time, wing length.",
      "Poisson" = "Number of offspring, number of animals, number of plant seeds.",
      "Uniform" = "Random number generation, lottery tickets, selection between alternatives.",
      "Beta" = "Proportions, probabilities, mixture models.",
      "T" = "Student's t-test, regression, hypothesis testing for small samples.",
      "Binomial" = "Number of success in a fixed number of trials, coin tosses.",
      "Exponential" = "Time between events, waiting times, survival analysis.",
      "Gamma" = "Insurance claims, rainfall, duration until failure.",
      "Log-Normal" = "Income, stock prices, financial data.",
      "Half-Cauchy" = "Scale parameters, Bayesian inference, extreme value analysis.",
      "Tweedie" = "Insurance claims, financial data, ecological data.",
      "Wald" = "Estimating the precision of an estimate, modeling travel times.",
      "ZIP" = "Disease counts, ecological data, count data with excess zeros.",
      "Bernoulli" = "Binary outcomes, success/failure, coin tosses."
    )

    text
  })

  # Generate and render distribution-specific text
  output$distributionNote <- renderUI({
    distribution <- input$distribution

    note <- switch(distribution,
      "Normal" = tagList(
        withMathJax(paste0("$$\\mathcal{N}(\\mu, \\sigma^2)$$"))
      ),
      "Poisson" = tagList(
        withMathJax(paste0("$$Pois(\\lambda)$$"))
      ),
      "Uniform" = tagList(
        withMathJax(paste0("$$\\mathcal{U}(a, b)$$"))
      ),
      "Beta" = tagList(
        withMathJax(paste0("$$Beta(\\alpha, \\beta)$$"))
      ),
      "T" = tagList(
        withMathJax(paste0("$$T(\\nu)$$"))
      ),
      "Binomial" = tagList(
        withMathJax(paste0("$$Binom(n,p)$$"))
      ),
      "Exponential" = tagList(
        withMathJax(paste0("$$Exp(\\lambda)$$"))
      ),
      "Gamma" = tagList(
        withMathJax(paste0("$$Gamma(\\alpha, \\beta)$$"))
      ),
      "Log-Normal" = tagList(
        withMathJax(paste0("$$lognormal(\\mu, \\sigma^2)$$"))
      ),
      "Half-Cauchy" = tagList(
        withMathJax(paste0("$$\\mathcal{HC}(\\alpha)$$"))
      ),
      "Tweedie" = tagList(
        withMathJax(paste0("$$TW_p(\\mu, \\lambda)$$")),
      ),
      "Wald" = tagList(
        withMathJax(paste0("$$\\mathcal{IG}(\\mu, \\lambda)$$")),
        h6("Note that the Wald distribution is often referred to as the Inverse Gaussian distribution (hence IG in the notation).")
      ),
      "ZIP" = tagList(
        withMathJax(paste0("$$ZIP(\\lambda, p)$$"))
      ),
      "Bernoulli" = tagList(
        withMathJax(paste0("$$Bern(p)$$"))
      )
    )

    do.call(tagList, note)
  })



  observeEvent(input$distribution, {
    distribution <- input$distribution

    # Set default values based on the distribution
    default_values <- switch(distribution,
      "Normal" = list(xmin = -10, xmax = 10),
      "Uniform" = list(xmin = -5, xmax = 5),
      "Poisson" = list(xmin = 0, xmax = 10),
      "Beta" = list(xmin = 0, xmax = 1),
      "T" = list(xmin = 0, xmax = 10),
      "Binomial" = list(xmin = 0, xmax = 10),
      "Exponential" = list(xmin = 0, xmax = 10),
      "Gamma" = list(xmin = 0, xmax = 10),
      "Log-Normal" = list(xmin = 0, xmax = 10),
      "Half-Cauchy" = list(xmin = -10, xmax = 10),
      "Tweedie" = list(xmin = 0.1, xmax = 10),
      "Wald" = list(xmin = 0, xmax = 10),
      "ZIP" = list(xmin = 0, xmax = 10),
      "Bernoulli" = list(xmin = 0, xmax = 1),
      NULL # Default case (if no distribution is selected)
    )

    # Update the default values of numericInput
    updateNumericInput(session, "xmin", value = default_values$xmin)
    updateNumericInput(session, "xmax", value = default_values$xmax)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
