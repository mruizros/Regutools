#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Genomics elements of E. coli"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider2", label = "Slider Range (Coords)", min = 5000,
                        max = 10000, value = c(5000, 10000)),
            selectInput("variable", "Genomics element:",
                        c("gene" = "gene",
                          "promoter" = "promoter",
                          "-10 promoter box" = "-10 promoter box",
                          "-35 promoter box" = "-35 promoter box",
                          "Regulatory Interaction" = "Regulatory Interaction",
                          "sRNA interaction" = "sRNA interaction",
                          "terminator" = "terminator"),
                        multiple = T),

        ),

        mainPanel(
            plotOutput("Grafico"),
            tableOutput("data"))

        )
    )


server <- function(input, output) {

    output$data <- renderTable({
        library("regutools")
        library("Biostrings")

        regulondb_conn <- connect_database()

        e_coli_regulondb <- regulondb(
            database_conn = regulondb_conn,
            organism = "E.coli",
            database_version = "1",
            genome_version = "1")

        grange <- GenomicRanges::GRanges("chr", IRanges::IRanges(5000, 10000))
        Result_range = data.frame(get_dna_objects(e_coli_regulondb, grange, elements = c(input$variable)))
    }, rownames = TRUE)


    output$Grafico <- renderPlot({

        e_coli_regulondb <- regulondb(
            database_conn = regulondb_conn,
            organism = "chr",
            database_version = "1",
            genome_version = "1")

        grange <- GenomicRanges::GRanges("chr", IRanges::IRanges(5000, 10000))
        plot_dna_objects(e_coli_regulondb, grange, elements = c(input$variable))
    })
}
# Run the application
shinyApp(ui = ui, server = server)
