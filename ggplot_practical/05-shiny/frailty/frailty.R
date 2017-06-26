library(shiny)

# population hazard of binary exponential frailty model
Frailty <- function (x, z1, z2, p, b) {
  # Wienke (2010): Frailty Models in Survival Analysis. p. 65.
  # x: age/time
  # z1: frailty factor of group 1
  # z2: frailty factor of group 2
  # p: share of group 1 at x = 0
  # b: rate parameter of exponential distribution
  
  M0 = b*x # exponential cumulative baseline hazard
  num = p*z1*exp(-z1*M0) + (1-p)*z2*exp(-z2*M0)
  den = p*exp(-z1*M0) + (1-p)*exp(-z2*M0)
  
  return(num/den*b)
}

# share of population 1 of binary exponential frailty model
# Wienke (2010): Frailty Models in Survival Analysis. p. 67.
FrailtyShare <- function (x, z1, z2, p, b) {
  M0 = b*x
  
  num = p*exp(-z1*M0)
  den = p*exp(-z1*M0) + (1-p)*exp(-z2*M0)

  return(num/den)
}

# user interface
ui <- fluidPage(
  titlePanel("Heterogeneity's Ruses"),
  sidebarLayout(
    sidebarPanel(sliderInput(inputId = "z1", label = "z1", min = 1, max = 100, value = 1),
                 sliderInput(inputId = "z2", label = "z2", min = 1, max = 100, value = 10),
                 sliderInput(inputId = "b", label = "b", min = 0.001, max = 0.3, value = 0.14),
                 sliderInput(inputId = "p", label = "p", min = 0, max = 1, value = 0.5)
    ),
    mainPanel(
      fluidRow(column(6, plotOutput(outputId = "plot1")),
               column(6, plotOutput(outputId = "plot2")))
    )
  )
)
# server code, e.g. R calculations
server <- function(input, output) {
  x <- seq(0,1, 0.01); n <- length(x)
  mx <- reactive(Frailty(x, z1 = input$z1, z2 = input$z2, p = input$p, b = input$b))
  px <- reactive(FrailtyShare(x, z1 = input$z1, z2 = input$z2, p = input$p, b = input$b))
  output$plot1 <- renderPlot({
    plot(x = x, y = mx(), type = "l", log = "y",
         xlim = c(0,1),
         ylim = c(0.001, 10),  col = "red", lwd = 2)
    lines(x = x, y = rep(input$b*input$z1, n), lty = 2)
    lines(x = x, y = rep(input$b*input$z2, n), lty = 3)
  }, width = 600, height = 400)
  output$plot2 <- renderPlot({
    plot(x = x, y = px(), xlim = c(0, 1), ylim = c(0, 1), type = "l", lty = 2)
    lines(x = x, y = 1-px(), lty = 3)
  }, width = 600, height = 400)
}
# start shiny app
shinyApp(ui = ui, server = server)