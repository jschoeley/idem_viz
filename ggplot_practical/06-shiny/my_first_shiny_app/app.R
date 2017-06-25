library(shiny)
library(ggplot2)

Gompertz <- function (x, a, b) {
  a*exp(b*x)
}
x <- 0:110


# user interface
ui <- bootstrapPage(
  sliderInput(inputId = "a", label = "a", min = 0.0001, max = 0.5, value = 0.001),
  sliderInput(inputId = "b", label = "b", min = 0, max = 0.5, value = 0.14),
  plotOutput(outputId = "plot")
)
# server code, e.g. R calculations
server <- function(input, output) {
  mx <- reactive(Gompertz(x, a = input$a, b = input$b))
  output$plot <- renderPlot(
    qplot(x = x, y = mx(), ylim = c(0.0001, 1), log = "y")
  )
}
# start shiny app
shinyApp(ui = ui, server = server)