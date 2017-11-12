

ui <- fluidPage(
  numericInput("num", label = h3("Numeric input"), value = 100),
  renderPlot("plot")
)

server <- function(input, output, session) {
size <- reactive({input$num})
  output$plot <- plotOutput({ 
    rnorm(size()) %>% hist() })
}

shinyApp(ui, server)
