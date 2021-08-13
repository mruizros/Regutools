library(shiny)
library(regutools)
library("ggplot2")
library("dplyr")

##### Datos iniciales generales
regulondb_conn <- connect_database()

e_coli_regulondb <-
  regulondb(
    database_conn = regulondb_conn,
    organism = "E.coli",
    database_version = "1",
    genome_version = "1"
  )
####### Datos iniciales Pepe
e_coli_regulondb_Pchr <- regulondb(
  database_conn = regulondb_conn,
  organism = "chr",
  database_version = "1",
  genome_version = "1")


### Datos iniciales Monica
names <- get_dataset(
  e_coli_regulondb,
  attributes = c("name"),
  dataset= "GENE"
)

genes <- names$name

#################


####### Inicio aplicacion#######

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Regutools"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("genepos", label = "Genomics range:", min = 0,
                  max = 4000000, value = c(100000, 110000))
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Genes within genomic ranges",
                 fluidRow(column(12,dataTableOutput('gene')))
                 
          
        ),
        
        tabPanel("Genomics elements",
                 selectInput("variable", "Genomics element:",
                             c("gene" = "gene",
                               "promoter" = "promoter",
                               "-10 promoter box" = "-10 promoter box",
                               "-35 promoter box" = "-35 promoter box",
                               "Regulatory Interaction" = "Regulatory Interaction",
                               "sRNA interaction" = "sRNA interaction",
                               "terminator" = "terminator"),
                             multiple = T),
                 plotOutput("Grafico"),
                 fluidRow(column(12,dataTableOutput('data')))
                 
          
        ),
        
        
        tabPanel("Genes regulated by transcripts",
                 selectizeInput('genes', label= "Seleccionar genes regulados", choices= genes, multiple= TRUE),
                 radioButtons( 'ByTrans_Gene', label=h3("See by:"), 
                               choices = c(Transcription_factor="Transcription_factor", Genes_regulated="Genes_regulated"), 
                               selected = "Transcription_factor" ), 
                 fluidRow(column(12,dataTableOutput('Regulatory'))))
        
        
      ),
      
      tabPanel("Otroplot")
    )
    
  )
)




# Define server logic
server <- function(input, output) {
  
  a <-reactive({
    
    get_dataset(
    e_coli_regulondb,
    attributes = c("name"),
    dataset = "GENE",
    filters = list(posright = (input$genepos)),
    interval = "posright")
  })

  
  ###### Pestaña 1
  output$gene <- renderDataTable({
    a()
  })
  
  ####### Pestaña 2
  output$data <- renderDataTable({
    
    req(length(input$variable) > 0)
    
    start <- as.integer(input$genepos[1])
    end <- as.integer(input$genepos[2])
    grange <- GenomicRanges::GRanges("chr", IRanges::IRanges(start,end))
    
    
    Result_range <- data.frame(get_dna_objects(e_coli_regulondb, grange=grange, elements = c(input$variable)))
    
    Result_range
    
  })
  
  
  output$Grafico <- renderPlot({
    
    req(length(input$variable) > 0)
    
    start <- as.integer(input$genepos[1])
    end <- as.integer(input$genepos[2])
    grange <- GenomicRanges::GRanges("chr", IRanges::IRanges(start,end))
    
    plot_dna_objects(e_coli_regulondb_Pchr, grange=grange, elements = c(input$variable))
  })
  
  ####### Pestaña 3
  output$Regulatory <- renderDataTable({
    
    
    req( length(input$genes) > 0 )
    
    Regulators <- data.frame(get_regulatory_summary(e_coli_regulondb,
                                                    gene_regulators = get_gene_regulators(e_coli_regulondb, input$genes)
    )) 
    
   
    
    RegulatorsGene <- Regulators %>% select("Regulated_genes","TF","Percent", "Activator", "Repressor", "Dual")
    
    RegulatorsTrans <- Regulators %>% select("TF","Regulated_genes","Percent", "Activator", "Repressor", "Dual")
    
    
    
    if (input$ByTrans_Gene == "Transcription_factor") {
      RegulatorsTrans
    } else {
      RegulatorsGene
    }
    
    
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
