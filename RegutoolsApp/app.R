library(shiny)

JCarrillo

ui <- fluidPage(

  titlePanel("Regutools"),
  # Sidebar for parameters
  sliderInput("genepos", label = h3("Intervalo"), min = 2000, max = 4000000, 
                value = c(2000, 400000)),
    mainPanel(
      tableOutput("gene")
    )
  )

# Define server logic
server <- function(input, output) {
  
  a <-reactive({
    
    get_dataset(
    e_coli_regulondb,
    attributes = c("name", "strand", "posright", "product_name"),
    dataset = "GENE",
    filters = list(posright = (input$genepos)),
    interval = "posright")
  })

  output$gene <- renderTable({
    a()
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
