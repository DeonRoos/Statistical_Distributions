# Load required libraries
library(shiny)          # For creating Shiny app
library(ggplot2)        # For plotting
library(tweedie)        # For Tweedie distribution
library(shinyWidgets)   # For enhanced UI elements
library(LaplacesDemon)  # For additional probability distributions
library(extraDistr)     # For additional probability distributions
library(shinydashboard) # For icons

# Define distributions and their default parameter values
distributions <- c("Normal", "Poisson", "Uniform", "Beta", "Student's T", "Exponential", "Gamma", "Log-Normal", "Half-Cauchy", "Tweedie", "Wald", "Binomial", "Bernoulli", "ZIP", "Negative Binomial")
default_params <- list(
  Normal = list(mean = 0, sd = 1),
  Poisson = list(lambda = 1),
  Uniform = list(min = 0, max = 1),
  Beta = list(alpha = 1, beta = 1),
  `Student's T` = list(df = 1),
  Exponential = list(rate = 1),
  Gamma = list(shape = 1, rate = 1),
  `Log-Normal` = list(meanlog = 0, sdlog = 1),
  `Half-Cauchy` = list(scale = 1),
  Tweedie = list(power = 1, mu = 1, phi = 1),
  Wald = list(mu = 1, lambda = 1),
  Binomial = list(size = 1, prob = 0.5),
  Bernoulli = list(prob = 0.5),
  ZIP = list(lambda = 1, pi = 0.5),
  `Negative Binomial` = list(size = 1, prob = 0.5)
)


# Define UI
ui <- navbarPage(
  title = "Explore Statistical Distributions",
  # Add custom styles to the head section of the HTML document
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
      
      /* Custom CSS styles */
      .navbar .navbar-brand {
          font-size: 24px;
          color: #FFFFFF !important;
        }
      
      /* Navigation bar styles */
      .navbar {
        background-color: #444654;
        color: #00A68A; 
        font-weight: bold;
        border: 2px solid #00A68A;
        box-shadow: 0 0 10px 5px rgba(0, 166, 138, 0.3);
        font-size: 20px;
      }

      /* Navigation bar styles */
      .navbar .nav > li > a:hover,
      .navbar .nav > li > a:focus {
        background-color: #00A68A;
        color: #FFFFFF !important;
        border: 2px solid #00A68A;
        box-shadow: 0 0 10px 5px rgba(0, 166, 138, 0.3);
      }

      /* Navigation bar styles */
      .navbar .nav .active > a,
      .navbar .nav .active > a:hover,
      .navbar .nav .active > a:focus {
        background-color: #00A68A;
        color: #FFFFFF !important;
        border: 2px solid #00A68A;
        box-shadow: 0 0 10px 5px rgba(0, 166, 138, 0.3);
      }
      ")
    )
  ),
  
  # Home tab
  tabPanel(title = NULL,
           icon = icon("home"),
           # Sidebar layout
           sidebarLayout(
             # Sidebar panel
             sidebarPanel(
               id = "sidebar",
               style = "
        background: linear-gradient(#444654, #3F3D39);
        /* background-color: #444654; */
        border-radius: 10px;
        border: 2px solid #00A68A;
        box-shadow: 0 0 10px 5px rgba(0, 166, 138, 0.3);",
        
        # Picker input to select the distribution
        pickerInput(
          inputId = "distribution",
          label = "Select distribution",
          choices = list(
            Continuous = c("Normal", sort(c("Uniform", "Beta", "Student's T", "Exponential", "Gamma", "Log-Normal", "Half-Cauchy", "Wald"))),
            Discrete = sort(c("Poisson", "Binomial", "Bernoulli", "ZIP", "Tweedie", "Negative Binomial"))
          ),
          options = list(
            `live-search` = TRUE
          )
        ),
        
        # Horizontal line
        tags$hr(style = "border-color: #00A68A;"),
        
        # Range of possible values
        h4("Range of candidate values"),
        numericInput("xmin", "Minimum X candidate:", value = -10, step = 0.5),
        numericInput("xmax", "Maximum X candidate:", value = 10), step = 0.5,
        numericInput("nobs", "Number of values to generate:", value = 100, min = 1, max = 5000, step = 25),
        
        # Horizontal line
        tags$hr(style = "border-color: #00A68A;"),
        
        # Number of parameter combinations
        h4("Number of parameter combinations"),
        numericInput("numCombinations", "Number of combinations:", value = 1, min = 1, max = 5),
        
        # Horizontal line
        tags$hr(style = "border-color: #00A68A;"),
        
        # Distribution specific parameters
        h4("Distribution specific parameters"),
        uiOutput("distributionParams"),
        
             ),
        
        # Main panel
        mainPanel(
          id = "main",
          
          # Style for text output
          tags$style("#myTextOutput { font-size: 30px; color: #00A68A; font-weight: bold; }"),
          
          # Fluid row for plots and summary
          fluidRow(
            column(
              # Text output for distribution name
              div(id = "myTextOutput", textOutput("distName")),
              width = 6,
              
              # Plot for probability density function
              plotOutput("densityPlot"),
              h6("Fig. 1: The probability density function visualized, whereby values which could be generated (x-axis) occur at different densities (y-axis) given the parameter values supplied."),
              br(),
              tags$hr(style = "border-color: #00A68A;"),
              br(),
              
              # Plot for randomly generated values
              plotOutput("randomPlot"),
              h6("Fig. 2: Randomly generated values based on the PDF, the number of observations requested, and the specific parameter values supplied.")
            ),
            column(
              width = 6,
              
              # Brief summary of the distribution
              br(),
              br(),
              h4("Brief summary"),
              textOutput("distributionText"),
              br(),
              
              # Detailed information about the distribution
              textOutput("detailed"),
              br(),
              h6("Notation:"),
              uiOutput("distributionNote"),
              h6("where the probability density function is given as:"),
              uiOutput("distributionPDF"),
              br(),
              tags$hr(style = "border-color: #00A68A;"),
              
              # Examples of data generated from the distribution
              h4("Examples of data generated from this distribution"),
              textOutput("distributionEG")
            )
          )
        )
           )),
  
  # Multi distribution comparisons
  tabPanel(title = "Compare",
           icon = icon("line-chart"),
           sidebarLayout(
             sidebarPanel(
               id = "sidebar",
               style = "
        background: linear-gradient(#444654, #3F3D39);
        /* background-color: #444654; */
        border-radius: 10px;
        border: 2px solid #00A68A;
        box-shadow: 0 0 10px 5px rgba(0, 166, 138, 0.3);",
        pickerInput(
          inputId = "selectedDistributions",
          label = "Select distribution",
          choices = list(
            Continuous = c("Normal", sort(c("Uniform", "Beta", "Student's T", "Exponential", "Gamma", "Log-Normal", "Half-Cauchy", "Wald"))),
            Discrete = sort(c("Poisson", "Binomial", "Bernoulli", "Tweedie", "ZIP", "Negative Binomial"))
          ),
          multiple = TRUE,
          selected = "Normal",
          options = list(
            `live-search` = TRUE
          )
        ),
        # Range of possible values
        h4("Range of candidate values"),
        numericInput("xminmulti", "Minimum X candidate:", value = -10, step = 0.5),
        numericInput("xmaxmulti", "Maximum X candidate:", value = 10), step = 0.5,
        uiOutput("paramInputs")
             ),
        mainPanel(
          h4("Plot to allow comparisons of different distributions"),
          plotOutput("densityPlotMulti")
        )
           )
  ),
  
  # About tab
  tabPanel(title = NULL,
           icon = icon("info-circle"),
           # App guidance
           h2("App guidance"),
           h4("The app allows users to visualize the selected distribution by 
              displaying the PDF plot, which shows the density of values that 
              can be generated based on the chosen parameters. To use the app, 
            select a distribution. Once selected, the default parameter values will be updated 
            but these can be changed. To do so, enter your desired parameter values in the relevant 
            input boxes which will be labelled according to the given distribution 
            (e.g. the Poisson distribution will only have the rate parameter (lambda).
            Once done, the PDF or PMF will be updated to reflect your chosen value(s).
            Further, below Fig. 1, Fig. 2 shows a number of random values generated
            according to your specified parameter values. If you wish, you may change 
            the number of random values to be generated by entering your value in
            the respective input box."),
           br(),
           h4("Additionally, if you want to compare PDF/PMF of the same distribution
            with different parameter values, you are able to select the 'Number of
            combination' you would like to compare. The maximum is number you are able
            to compare is 10. Once selected, the new parameter set will be identical 
            and so will overlap each other. Simply change the parameter values of
            a particular combination to see the effect."),
           br(),
           h4("In the app you may specify the minimum and maximum candidate values 
            within which you would like to know the probability density/mass function, 
            as well as select the number of observations to randomly generate from 
            the current distribution"),
           
           tags$hr(style = "border-color: #00A68A;"),
           
           h2("Caveats"),
           h4("The app contains only a selection of distributions and omits a 
              few that I would have liked to include but could not figure out
              how to, specifically the Categorical and Multinomial distributions, 
              both of which I use regularly, but visualising them in a format
              that works within the current structure of the app is difficult 
            (and likely requires entirely distinct code for both distributions)."),
           br(),
           h4("Additionally, I am not entirely familar with some distributions, so
            some may not be annotated correctly in their respective notation or
            PDF/PMF. This is especially true for the Tweedie distribution. If you 
            are familiar with the distribution and can advise on this, please get 
            in touch using the github link below. Note that for coding simplicity, 
            I refer to both in the app as 'PDF' though this is not always correct."),
           
           tags$hr(style = "border-color: #00A68A;"),
           
           # Footer text and link to GitHub repository
           p("This app was created by Deon Roos. The code is available from the repository linked below"),
           tags$a(href = "https://github.com/DeonRoos/Statistical_Distributions", "GitHub Repo")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Generate parameter inputs for selected distributions
  output$paramInputs <- renderUI({
    selected_distributions <- input$selectedDistributions
    param_inputs <- lapply(selected_distributions, function(dist) {
      params <- default_params[[dist]]
      param_list <- lapply(names(params), function(param) {
        value <- params[[param]]
        numericInput(paste0(param, "_", dist), param, value = value)
      })
      tagList(
        h4(dist),
        param_list
      )
    })
    do.call(tagList, param_inputs)
  })
  
  # Generate and render the density plot
  output$densityPlotMulti <- renderPlot({
    selected_distributions <- input$selectedDistributions
    
    if (is.null(selected_distributions) || length(selected_distributions) == 0) {
      return(NULL)
    }
    
    df_density <- data.frame(x = numeric(0), density = numeric(0), distribution = character(0), stringsAsFactors = FALSE)
    
    for (dist in selected_distributions) {
      params <- default_params[[dist]]
      param_values <- reactiveValuesToList(input)
      param_values <- param_values[grep(paste0("^.*", dist, "$"), names(param_values))]
      
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
                                dtweedie(as.integer(x), power = param_values$power, mu = param_values$mu, phi = param_values$phi)
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
                                dbern(as.integer(x), prob = param_values$prob)
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
                              if (!is.null(param_values$size) && !is.null(param_values$prob)) {
                                dnbinom(as.integer(x), size = param_values$size, prob = param_values$prob)
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
    
    ggplot(df_density, aes(x, density, color = distribution, group = as.factor(distribution))) +
      geom_line(size = 1) +
      ggtitle("Multiple Distribution Density Plot") +
      xlab("Possible values from the current distribution") +
      ylab("Standardised density") +
      labs(caption = "The y-axis is standardised to allow easier comparison between distributions.") +
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
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "#444654"),
        panel.grid.minor = element_line(color = "#444654")
      )
  })
  
  # Get name of selected distribution
  output$distName <- renderText({
    name <- paste("The", input$distribution, "distribution")
    name
  })
  
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
                 h6("where the probability density function is given as:")
               ),
               "Uniform" = tagList(
                 numericInput(paste0("min_", i), withMathJax(helpText("Minimum (\\(a\\)):")), value = -3),
                 numericInput(paste0("max_", i), withMathJax(helpText("Maximum (\\(b\\)):")), value = 3)
               ),
               "Beta" = tagList(
                 numericInput(paste0("alpha_", i), withMathJax(helpText("Shape 1 (\\(\\alpha\\)):")), value = 3),
                 numericInput(paste0("beta_", i), withMathJax(helpText("Shape 2 (\\(\\beta\\)):")), value = 2)
               ),
               "Student's T" = tagList(
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
                 numericInput(paste0("shape_", i), withMathJax(helpText("Shape (\\(\\alpha\\)):")), value = 2),
                 numericInput(paste0("rate_gamma_", i), withMathJax(helpText("Scale (\\(\\beta\\)):")), value = 1)
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
                 numericInput(paste0("p_", i), withMathJax(helpText("Probability (\\(p\\)):")), value = 0.5, min = 0, max = 1, step = 0.1)
               ),
               "Negative Binomial" = tagList(
                 numericInput(paste0("size_", i), withMathJax(helpText("Size (\\(n\\)):")), value = 1, min = 0),
                 numericInput(paste0("mu_", i), withMathJax(helpText("Mean (\\(\\mu\\)):")), value = 3, min = 0)
               )
        )
      )
    })
    
    do.call(tagList, params_ui)
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
             "Student's T" = {
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
             },
             "Negative Binomial" = {
               x <- seq(xmin, xmax, length.out = 100)
               dnbinom(as.integer(x), mu = input[[paste0("mu_", i)]], size = input[[paste0("size_", i)]])
             }
      )
    })
    
    # Create a data frame for the plot
    data <- data.frame(x = seq(xmin, xmax, length.out = 100), density = unlist(plot_data))
    data$combination <- rep(1:input$numCombinations, each = 100)
    
    # Create and customize the ggplot density plot
    ggplot(data, aes(x = x, y = density, group = combination, color = as.factor(combination))) +
      geom_line(size = 1) +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") +
      scale_y_continuous(limits = c(0, NA)) +
      ggtitle(paste("Probability Density Function for the", input$distribution, "distribution")) +
      xlab("Possible values from the current distribution") +
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
  
  # Observe changes in the selected distribution
  observeEvent(input$distribution, {
    distribution <- input$distribution
    
    # Set default values based on the distribution
    default_values <- switch(distribution,
                             "Normal" = list(xmin = -10, xmax = 10),
                             "Uniform" = list(xmin = -5, xmax = 5),
                             "Poisson" = list(xmin = 0, xmax = 10),
                             "Beta" = list(xmin = 0, xmax = 1),
                             "Student's T" = list(xmin = -10, xmax = 10),
                             "Binomial" = list(xmin = -1, xmax = 10),
                             "Exponential" = list(xmin = 0, xmax = 10),
                             "Gamma" = list(xmin = 0, xmax = 10),
                             "Log-Normal" = list(xmin = 0, xmax = 10),
                             "Half-Cauchy" = list(xmin = 0, xmax = 10),
                             "Tweedie" = list(xmin = 0, xmax = 10),
                             "Wald" = list(xmin = 0, xmax = 10),
                             "ZIP" = list(xmin = -1, xmax = 10),
                             "Bernoulli" = list(xmin = -0.1, xmax = 1.1),
                             "Negative Binomial" = list(xmin = 0, xmax = 10),
                             NULL # Default case (if no distribution is selected)
    )
    
    # Update the default values of numericInput
    updateNumericInput(session, "xmin", value = default_values$xmin)
    updateNumericInput(session, "xmax", value = default_values$xmax)
  })
  
  # Generate and render the RNG plot
  output$randomPlot <- renderPlot({
    xmin <- input$xmin
    xmax <- input$xmax
    distribution <- input$distribution
    nobs <- input$nobs
    
    plot_data <- lapply(1:input$numCombinations, function(i) {
      switch(distribution,
             "Normal" = {
               rnorm(nobs, mean = input[[paste0("mean_", i)]], sd = input[[paste0("sd_", i)]])
             },
             "Poisson" = {
               rpois(nobs, lambda = input[[paste0("lambda_", i)]])
             },
             "Uniform" = {
               runif(nobs, min = input[[paste0("min_", i)]], max = input[[paste0("max_", i)]])
             },
             "Beta" = {
               rbeta(nobs, shape1 = input[[paste0("alpha_", i)]], shape2 = input[[paste0("beta_", i)]])
             },
             "Student's T" = {
               rt(nobs, df = input[[paste0("df_", i)]])
             },
             "Binomial" = {
               rbinom(nobs, size = input[[paste0("size_", i)]], prob = input[[paste0("prob_", i)]])
             },
             "Exponential" = {
               rexp(nobs, rate = input[[paste0("rate_", i)]])
             },
             "Gamma" = {
               rgamma(nobs, shape = input[[paste0("shape_", i)]], rate = input[[paste0("rate_gamma_", i)]])
             },
             "Log-Normal" = {
               rlnorm(nobs, meanlog = input[[paste0("meanlog_", i)]], sdlog = input[[paste0("sdlog_", i)]])
             },
             "Half-Cauchy" = {
               rhalfcauchy(nobs, scale = input[[paste0("scale_", i)]])
             },
             "Tweedie" = {
               rtweedie(nobs, p = input[[paste0("p_", i)]], mu = input[[paste0("mu_", i)]], phi = input[[paste0("phi_", i)]])
             },
             "Wald" = {
               rwald(nobs, mu = input[[paste0("mu_", i)]], lambda = input[[paste0("lambda_", i)]])
             },
             "ZIP" = {
               rzip(nobs, lambda = input[[paste0("lambda_", i)]], pi = input[[paste0("pi_", i)]])
             },
             "Bernoulli" = {
               rbern(nobs, p = input[[paste0("p_", i)]])
             },
             "Negative Binomial" = {
               rnbinom(nobs, size = input[[paste0("size_", i)]], mu = input[[paste0("mu_", i)]])
             }
      )
    })
    
    # Create a data frame for the plot
    data <- data.frame(values = unlist(plot_data))
    data$combination <- rep(1:input$numCombinations, each = nobs)
    
    # Create and customize the ggplot random plot
    ggplot(data, aes(x = values, group = combination, color = as.factor(combination), fill = as.factor(combination))) +
      geom_histogram(size = 1, position = position_dodge(), alpha = 0.3) +
      theme_minimal() +
      scale_color_brewer(palette = "Set2") +
      scale_fill_brewer(palette = "Set2") +
      scale_x_continuous(limits = c(xmin, xmax)) +
      ggtitle(paste(input$nobs, "randomly generated values from the", input$distribution, "distribution:")) +
      xlab("Randomly generated values from current distribution") +
      ylab("Frequency") +
      guides(color = guide_legend(title = "Combination"), fill = guide_legend(title = "Combination")) +
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
  
  # Create "detailed" descriptions of distributions
  output$detailed <- renderText({
    distribution <- input$distribution
    
    text <- switch(distribution,
                   "Normal" = paste(
                     "The Normal distribution, also known as the Gaussian distribution, is one 
                   of the most important and widely used probability distributions in statistics.
                   It is often referred to as the 'bell curve' due to its characteristic symmetric
                   shape resembling a bell. In simple terms, the Normal distribution describes a
                   pattern that many natural phenomena tend to follow. It is commonly observed
                   in real-world measurements such as heights, weights, test scores, and other
                   quantities. The key features of the Normal distribution are as follows:
                   Symmetry: The distribution is symmetric around its center, which is called the mean.
                   This means that the values on the left and right sides of the mean are balanced.
                   Bell-shaped: The distribution has a characteristic bell shape, where the highest
                   point (peak) is at the mean. The values gradually decrease as you move away from the mean.
                   Mean and Standard Deviation: The mean of the Normal distribution represents the average
                   or typical value, while the standard deviation measures the spread or variability of the data.
                   The larger the standard deviation, the more spread out the data points are."
                   ),
                   "Poisson" = paste(
                     "The Poisson distribution is a probability distribution that is commonly used
                   to model the occurrence of rare events. It is often used when we want to
                   estimate the number of events that are likely to happen within a specific time
                   period or in a particular area. The key features of the Poisson distribution
                   are as follows: Counting Events: The Poisson distribution is used to describe the number of
                   events that occur in a fixed interval of time or space. These events are assumed
                   to be independent and occur at a constant average rate. Rare Events: The 
                   Poisson distribution is suitable for modeling rare events
                   where the average rate of occurrence is small, but the total number of possible
                   events is large. Mean and Variance: The mean of the Poisson distribution is equal to the average
                   rate of events, while the variance is also equal to the average rate. This means
                   that the spread of the distribution increases with higher event rates."
                   ),
                   "Uniform" = paste(
                     "The Uniform distribution is a simple and straightforward probability distribution
                   that models situations where all outcomes are equally likely. It creates a
                   rectangular shape with constant probability density throughout its range.
                   In this distribution, every possible outcome within a specific range has an
                   equal chance of occurring, similar to rolling a fair, six-sided die.
                   The Uniform distribution is useful when there is no inherent bias or preference
                   for certain outcomes, and it is often used as a baseline for comparisons
                   with other distributions. Overall, it represents a situation where all outcomes
                   have an equal probability within a fixed range."
                   ),
                   "Beta" = paste(
                     "The Beta distribution is a probability distribution that is commonly used
                   to model random variables that have values between 0 and 1. It is often
                   used to represent probabilities or proportions. The key features of the
                   Beta distribution are as follows:
                   Values Between 0 and 1: The Beta distribution is defined on the interval
                   (0, 1), which makes it suitable for modeling proportions or probabilities.
                   The distribution is characterized by two shape parameters, often denoted
                   as alpha and beta.
                   Flexible Shape: The Beta distribution can take on a variety of shapes,
                   ranging from U-shaped to J-shaped, depending on the values of its shape
                   parameters. This flexibility allows it to model a wide range of data patterns.
                   Uniform and Symmetric: When the shape parameters of the Beta distribution
                   are equal and greater than 1, the distribution becomes symmetric and resembles
                   a uniform distribution. This means that all values between 0 and 1 are equally
                   likely to occur.
                   Applications: The Beta distribution is widely used in areas such as Bayesian
                   statistics, quality control, modeling proportions in survey sampling, and
                   A/B testing.
                   It's important to note that the Beta distribution can represent a wide range
                   of shapes and behaviors, making it a versatile tool in statistical modeling."
                   ),
                   "Student's T" = paste(
                     "The Student's T distribution, also known as the T distribution, is a probability
                   distribution that is commonly used for statistical inference when the sample
                   size is small or when the population standard deviation is unknown. It is
                   pseudonym 'Student'. The key features of the Student's T distribution are as follows:
                   Similar to the Normal Distribution: The Student's T distribution resembles
                   the Normal distribution in shape, but it has heavier tails. This means that
                   it allows for more extreme values compared to the Normal distribution.
                   Degree of Freedom: The shape of the T distribution is determined by its
                   'degrees of freedom' parameter, denoted as 'df'. The degrees of freedom
                   reflect the size of the sample used to estimate the population parameters.
                   As the degrees of freedom increase, the T distribution approaches the Normal
                   distribution.
                   Widely Used in Hypothesis Testing: The T distribution plays a crucial role
                   in hypothesis testing, particularly when the sample size is small. It is used
                   to calculate confidence intervals and perform t-tests to compare means between
                   groups or to test if a sample mean differs from a known value.
                   Applications: The T distribution is widely used in various fields, including
                   medical research, social sciences, finance, and quality control.
                   The Student's T distribution is an important tool in statistical analysis,
                   especially when working with small sample sizes or uncertain population
                   parameters."
                   ),
                   "Binomial" = paste(
                     "The Binomial distribution is a probability distribution that is commonly used
                   to model the number of 'successes' in a fixed number of 'trials'. It is
                   applicable when there are only two possible outcomes for each trial, typically
                   referred to as 'success' and 'failure'. The key features of the Binomial
                   distribution are as follows:
                   Fixed Number of Trials: The Binomial distribution models a fixed number of
                   independent and identical trials, where each trial can result in one of two
                   outcomes.
                   Probability of Success: The distribution is characterized by the probability
                   of success, denoted as 'p'. This represents the likelihood of observing a success
                   on each individual trial.
                   Number of Successes: The Binomial distribution describes the probability of
                   obtaining a specific number of successes, denoted as 'k', out of the fixed number
                   of trials.
                   Independent Trials: The trials in a Binomial distribution are assumed to be
                   independent, meaning that the outcome of one trial does not influence the
                   outcome of another.
                   Applications: The Binomial distribution is commonly used to model various
                   real-world scenarios, such as the number of successful sales out of a fixed
                   number of attempts, the number of defective items in a production batch, or the
                   number of correct answers on a multiple-choice test.
                   The Binomial distribution provides a useful framework for understanding and
                   predicting the occurrence of binary outcomes in situations involving a fixed
                   number of trials."
                   ),
                   "Exponential" = paste(
                     "The Exponential distribution is a probability distribution that is often used
                   to model the time between events that occur randomly and independently at a
                   constant average rate. It is particularly useful for understanding the
                   occurrence of events in continuous time. The key features of the Exponential
                   distribution are as follows:
                   Constant Rate: The distribution is characterized by a constant rate parameter,
                   often denoted as 'λ' (lambda). This parameter represents the average number of
                   events that occur per unit of time. The higher the rate, the more frequent the
                   events.
                   Memoryless Property: The Exponential distribution has a unique property called
                   the 'memoryless property'. This means that the probability of an event occurring
                   in the next time interval does not depend on how much time has already passed.
                   In other words, the distribution does not 'remember' when the last event occurred.
                   Applications: The Exponential distribution is commonly used to model the time
                   between occurrences of events, such as the time between phone calls at a call
                   center, the time between arrivals of customers at a service counter, or the time
                   between successive radioactive decays.
                   The Exponential distribution provides a simple and intuitive way to model the
                   waiting times between events that happen randomly and independently over time."
                   ),
                   "Gamma" = paste(
                     "The Gamma distribution is a probability distribution that is often used to
                   model the time it takes for a certain number of events to occur. It is
                   commonly used to analyze the waiting times for a series of events to happen.
                   The key features of the Gamma distribution are as follows:
                   Shape and Scale: The distribution is characterized by two parameters: shape
                   and scale. The shape parameter determines the shape of the distribution curve,
                   while the scale parameter determines the spread or variability of the data.
                   These parameters allow the Gamma distribution to be flexible in representing
                   various patterns of event occurrences.
                   Applications: The Gamma distribution is widely applied in various fields,
                   such as reliability analysis, queueing theory, insurance modeling, and
                   actuarial science. It is often used to model the time until a series of
                   independent events occur, such as the lifetime of a machine, the duration
                   between phone calls, or the inter-arrival times of customers at a store.
                   Relationship to Other Distributions: The Exponential distribution is a
                   special case of the Gamma distribution when the shape parameter is 1. This
                   means that the Exponential distribution represents the time until the first
                   event occurs, while the Gamma distribution can represent the time until the
                   kth event occurs.
                   The Gamma distribution provides a flexible framework for modeling the time
                   until a certain number of events happen, allowing for a wide range of
                   applications in various fields."
                   ),
                   "Log-Normal" = paste(
                     "The Log-Normal distribution is a probability distribution that is often used 
                   to model variables that are positive and skewed. It is commonly used to analyze 
                   data that exhibits exponential growth or multiplicative effects. The key features 
                   of the Log-Normal distribution are as follows:
                   Shape and Scale: The distribution is characterized by two parameters: the mean and 
                   the standard deviation of the natural logarithm of the data. The mean determines 
                   the location of the distribution, while the standard deviation determines the spread 
                   or variability of the data. These parameters allow the Log-Normal distribution to 
                   capture a wide range of positive skewed patterns.
                   Applications: The Log-Normal distribution is commonly used to model phenomena in 
                   various fields, such as finance, economics, biology, and environmental sciences. 
                   It is often used to describe variables that are product or ratio of several factors, 
                   such as stock prices, income distributions, population sizes, and response times.
                   Relationship to Other Distributions: The Log-Normal distribution is related to the 
                   Normal distribution. If a variable follows a Log-Normal distribution, its natural 
                   logarithm will follow a Normal distribution. This relationship makes the Log-Normal 
                   distribution useful for transforming skewed data into a more symmetric form.
                   The Log-Normal distribution provides a flexible framework for modeling positively 
                   skewed data that exhibit exponential growth or multiplicative effects. It is widely 
                   used in various fields to analyze and describe phenomena that follow such patterns."
                   ),
                   "Half-Cauchy" = paste(
                     "The Half-Cauchy distribution is a probability distribution that is often used to 
                   model positive variables with heavy tails. It is derived from the Cauchy distribution, 
                   which is known for its wide tails and lack of moments. The key features of the Half-Cauchy 
                   distribution are as follows:
                   Shape and Scale: The distribution is characterized by a single parameter, the scale 
                   parameter, which controls the spread or variability of the data. The Half-Cauchy 
                   distribution has a shape similar to the Cauchy distribution but is restricted to positive 
                   values.
                   Heavy Tails: The Half-Cauchy distribution has heavy tails, which means that it allows for 
                   the occurrence of extreme values that are relatively far from the center of the distribution. 
                   This property makes it useful for modeling variables with occasional extreme observations.
                   Applications: The Half-Cauchy distribution is commonly used as a prior distribution in 
                   Bayesian statistics, particularly when modeling scale parameters. It can also be employed 
                   in various fields such as finance, psychology, and environmental sciences to describe 
                   variables that exhibit heavy-tailed behavior.
                   The Half-Cauchy distribution provides a flexible framework for modeling positively skewed 
                   variables with heavy tails. It is commonly used in statistical analyses and Bayesian 
                   modeling to capture extreme observations and accommodate uncertainty in the scale parameter."
                   ),
                   "Tweedie" = paste(
                     "The Tweedie distribution is a probability distribution that is used to model 
                   non-negative continuous random variables with varying levels of skewness and 
                   dispersion. It is named after the British statistician Maurice Tweedie. The key 
                   features of the Tweedie distribution are as follows:
                   Skewness and Dispersion: The Tweedie distribution allows for modeling variables 
                   that exhibit both skewness (asymmetric shape) and dispersion (variation in the 
                   spread of values). It provides a flexible framework to capture different levels 
                   of skewness and dispersion in the data.
                   Compound Poisson-Gamma Structure: The Tweedie distribution is a compound distribution 
                   that combines the properties of the Poisson and Gamma distributions. The Poisson 
                   component determines the frequency of events, while the Gamma component models the 
                   severity or size of each event.
                   Applications: The Tweedie distribution finds applications in various fields such 
                   as insurance, actuarial science, ecology, and finance. It is particularly useful 
                   for modeling data with excess zeros and overdispersion, where the variation in 
                   the data exceeds what would be expected from a Poisson or Gaussian distribution.
                   The Tweedie distribution provides a versatile tool for modeling non-negative data 
                   that exhibit both skewness and dispersion. Its flexibility and ability to handle 
                   excess zeros make it valuable in statistical analysis and predictive modeling."
                   ),
                   "Wald" = paste(
                     "The Wald distribution, also known as the Inverse Gaussian distribution, is a 
                   probability distribution that is often used to model positive continuous random 
                   variables. It is named after the British statistician Abraham Wald. The key 
                   features of the Wald distribution are as follows:
                   Shape and Skewness: The Wald distribution is characterized by a shape that 
                   resembles an inverted bell curve. It is a positively skewed distribution, meaning 
                   that it has a long tail on the right-hand side.
                   Modeling Rates and Time: The Wald distribution is commonly used for modeling 
                   rates or time until an event occurs. It is often employed in survival analysis 
                   to model the time to failure or in econometrics to model the duration of certain 
                   events.
                   Relationship to Normal Distribution: The Wald distribution is related to the Normal 
                   distribution through its limiting behavior. In certain cases, when the sample size 
                   is large, the Wald distribution can approximate a Normal distribution.
                   Applications: The Wald distribution finds applications in various fields including 
                   finance, biology, engineering, and physics. It is particularly useful in situations 
                   where the data exhibit skewness and the distribution of the data is better described 
                   by a long-tailed distribution than a Normal distribution.
                   The Wald distribution provides a flexible and skewed probability distribution for 
                   modeling positive continuous variables. Its shape and relationship to the Normal 
                   distribution make it suitable for various statistical analyses and modeling tasks."
                   ),
                   "ZIP" = paste(
                     "The Zero-Inflated Poisson (ZIP) distribution is a probability distribution that 
                   is used to model count data with excessive zeros. It is commonly encountered when 
                   analyzing data that exhibit both an excess of zeros and a typical Poisson distribution 
                   for the non-zero counts.
                   The key features of the Zero-Inflated Poisson distribution are as follows:
                   Excess Zero Probability: The ZIP distribution accounts for the excess zeros by 
                   incorporating an additional component that models the probability of observing zero 
                   counts. This component is separate from the Poisson distribution component that models 
                   the non-zero counts.
                   Two Components: The ZIP distribution consists of two components: a probability mass 
                   at zero (representing excess zeros) and a Poisson distribution (representing non-zero 
                   counts). The probability mass at zero component allows for a higher likelihood of zero
                   counts than what would be expected from a standard Poisson distribution.
                   Applications: The ZIP distribution is commonly used in various fields such as 
                   epidemiology, ecology, finance, and insurance. It is particularly suitable for modeling 
                   count data that exhibit excessive zeros, such as insurance claims, species abundance, 
                   and disease occurrence data.
                   The Zero-Inflated Poisson distribution provides a flexible and versatile model for count 
                   data that contains an excess of zeros. By incorporating both the probability of zero 
                   counts and the Poisson distribution for non-zero counts, it allows for a more accurate 
                   representation of the underlying data generating process."
                   ),
                   "Bernoulli" = paste(
                     "The Bernoulli distribution is a probability distribution that models an experiment with 
                   two possible outcomes: success and failure. It is named after Jacob Bernoulli, a Swiss 
                   mathematician who introduced the concept of the Bernoulli trials.
                   The key features of the Bernoulli distribution are as follows:
                   Binary Outcome: The Bernoulli distribution represents a random variable that can take on 
                   one of two values, typically labeled as 1 (success) and 0 (failure). It is often used to 
                   model events that have only two possible outcomes, such as coin flips, yes/no questions, 
                   or the success/failure of a single trial.
                   Probability of Success: The Bernoulli distribution is characterized by a single parameter, 
                   usually denoted as 'p', which represents the probability of success. The probability of 
                   failure is simply 1 minus the probability of success.
                   Independent Trials: The Bernoulli distribution assumes that each trial is independent of 
                   the others. This means that the outcome of one trial does not affect the outcome of any 
                   other trial.
                   Applications: The Bernoulli distribution is commonly used in various areas such as statistics, 
                   probability theory, and machine learning. It serves as the building block for more complex 
                   distributions and models, such as the binomial distribution and logistic regression.
                   The Bernoulli distribution provides a simple and intuitive way to model binary events with 
                   known probabilities. It is a fundamental distribution that forms the basis for many statistical 
                   analyses and models."
                   ),
                   "Negative Binomial" = paste(
                     "The Negative Binomial distribution is a probability distribution that models the number of 
                   trials required to achieve a certain number of successes, where each trial has a fixed 
                   probability of success. An alternative parametrization (often used in ecology) uses the mean, 
                   size, and the dispersion parameter, where the probability is size/(size + mu).
                   The key features of the Negative Binomial distribution are as follows:
                   Counting Successes: The Negative Binomial distribution is used to count the number of failures 
                   that occur before a specified number of successes are achieved. It models situations where the 
                   number of trials needed to reach the desired number of successes is uncertain.
                   Probability of Success: The Negative Binomial distribution is characterized by two parameters: 
                   the probability of success (usually denoted as 'p') and the number of successes needed (usually 
                   denoted as 'r'). The probability of failure is simply 1 minus the probability of success.
                   Overdispersion: Unlike the Poisson distribution, which assumes that the variance is equal to the 
                   mean, the Negative Binomial distribution allows for overdispersion, meaning that the variance can 
                   be larger than the mean. This makes it a suitable choice for data that exhibit extra variation or 
                   clustering beyond what is expected under a Poisson distribution.
                   Applications: The Negative Binomial distribution is commonly used in various fields, such as 
                   biology, epidemiology, economics, and social sciences. It is used to model events that involve a 
                   certain number of successes with uncertain timing or to analyze data with overdispersion.
                   The Negative Binomial distribution provides a flexible and versatile tool for modeling and analyzing 
                   count data with varying rates of success. It allows for capturing the uncertainty in the number of 
                   trials required to achieve a specific number of successes and accounts for overdispersion in the data."
                   )
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
                   "Student's T" = tagList(
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
                     withMathJax(paste0("$$TW_p(\\mu, \\phi)$$")),
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
                   ),
                   "Negative Binomial" = tagList(
                     withMathJax(paste0("$$NB(\\mu, n)$$")),
                   )
    )
    
    do.call(tagList, note)
  })
  
  # Generate and render distribution-specific text
  output$distributionText <- renderText({
    distribution <- input$distribution
    
    text <- switch(distribution,
                   "Normal" = "The Normal (or Gaussian) distribution is a continuous probability distribution for real-valued random variables.",
                   "Poisson" = "The Poisson distribution is a discrete probability distribution for positive data.",
                   "Uniform" = "The Uniform distribution is a bounded continuous probability distribution.",
                   "Beta" = "The Beta distribution is a continuous probability distribution defined on the interval [0,1].",
                   "Student's T" = "The Student's T distribution is a continuous probability distribution similar to the Normal distribution but with greater variation at the extremes.",
                   "Binomial" = "The Binomial distribution is a discrete probability distribution for binary outcomes with a fixed number of trials.",
                   "Exponential" = "The Exponential distribution is a continuous probability distribution for the time between events in a Poisson process.",
                   "Gamma" = "The Gamma distribution is a continuous probability distribution often used for modeling waiting times or duration until an event.",
                   "Log-Normal" = "The Log-Normal distribution is a continuous probability distribution for positive variables whose logarithm follows a normal distribution.",
                   "Half-Cauchy" = "The Half-Cauchy distribution is a continuous probability distribution with heavy tails.",
                   "Tweedie" = "The Tweedie distribution is a continuous probability distribution often used for modeling data with excess zeros and overdispersion.",
                   "Wald" = "The Wald distribution, also known as the Inverse Gaussian distribution, is a continuous probability distribution used for modeling positive variables with skewed distributions.",
                   "ZIP" = "The Zero-Inflated Poisson (ZIP) distribution is a discrete probability distribution that combines a Poisson distribution with excess zeros. It is often used to model count data with an excess number of zero values.",
                   "Bernoulli" = "The Bernoulli distribution is a discrete probability distribution that represents a special case of a Binomial whereby there is a single trial (rather than > 1).",
                   "Negative Binomial" = "The Negative Binomial distribution is a discrete probability distribution that can either represent the number of failures or the number of trials."
    )
    
    text
  })
  
  # Create example data for distributions
  output$distributionEG <- renderText({
    distribution <- input$distribution
    
    text <- switch(distribution,
                   "Normal" = "Plant height | Animal Body Mass | Exam Scores | IQ Scores | Blood Pressure",
                   "Poisson" = "Number of fish caught in a lake | Number of bird sightings in a day | Number of customer arrivals at a store | Number of phone calls received in an hour | Number of defects in a production line",
                   "Uniform" = "Randomly generated lottery numbers | Randomly selected time intervals | Randomly generated test scores",
                   "Beta" = "Proportion of seeds germinating in different soil types | Proportion of individuals exhibiting a certain behavior in a population |
                   Conversion rates of website visitors | Proportion of defective items in a manufacturing process | Likelihood of customer satisfaction ratings",
                   "Student's T" = "Body length of fish species A | Height of tree species B | Test scores of students in a math exam | Reaction times of participants in a cognitive task | Weight of randomly selected packages in a delivery service",
                   "Binomial" = "Number of successful pollinations | Number of infected individuals | Number of heads in coin flips | Number of defective products | Number of students passing a test",
                   "Exponential" = "Time between tree seed dispersal | Time between insect emergence events | Time until a light bulb burns out | Time until a customer arrives at a store | Time until a package is delivered",
                   "Gamma" = "Growth rate of plant populations | Lifespan of mice | Time until a car battery needs replacement | Time until the next earthquake | Time until a computer hard drive fails",
                   "Log-Normal" = "Height of trees | Lifespan of birds | Wealth distribution in a population | Time until a website receives a new visitor | Size of fish in a lake",
                   "Half-Cauchy" = "Tree trunk diameter | Wingspan of birds | Reaction times in a population | Length of rivers | Duration of animal vocalizations",
                   "Tweedie" = "Species abundance in a habitat | Rainfall intensity | Number of insurance claims | Household electricity consumption | Traffic accidents per day",
                   "Wald" = "Impala height | Reaction time in a cognitive task | Time until equipment failure | Blood pressure measurement | Distance between foraging locations",
                   "ZIP" = "Tree species count in a forest | Number of bird sightings in a day | Number of software bugs in a program | Number of car accidents at an intersection | Number of customer complaints in a week",
                   "Bernoulli" = "Rainfall occurrence in a day | Presence of a particular species in a habitat | Success of a marketing campaign (conversion or not) | Customer churn (lost or retained) | Occurrence of a disease in a population",
                   "Negative Binomial" = "Bird species richness in a forest | Number of individuals in a population sample | Number of customer complaints received in a day | Time until a machine fails | Number of goals scored in a football match"
    )
    
    text
  })
  
  # Create description of PDF/PMF functions
  output$distributionPDF <- renderUI({
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
                   "Student's T" = tagList(
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
                     withMathJax(paste0("$$f(x) = p^{x}(1-p)^{1-x} $$"))
                   ),
                   "Negative Binomial" = tagList(
                     withMathJax(paste0("$$f(x) = \\frac{\\Gamma(x-n)}{\\Gamma(n)x!}p^n(1-p)^x$$"))
                   )
    )
    
    do.call(tagList, note)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
