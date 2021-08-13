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
##################

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
    sidebarPanel(),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Genes dentro de intervalo",
                 sliderInput("genepos", label = h3("Intervalo"), min = 2000, max = 4000000, 
                             value = c(2000, 400000)),
                 tableOutput("gene")
          
        ),
        
        tabPanel("Genes regulados por transcritos",
                 selectizeInput('genes', label= "Seleccionar genes regulados", choices= genes, multiple= TRUE),
                 radioButtons( 'ByTrans_Gene', label=h3("Ver por transcrito o por genes regulados"), 
                               choices = c(Transcrito="Factor_transcripcion", Genes_regulados="Genes_regulados"), 
                               selected = "Factor_transcripcion" ), 
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

  output$gene <- renderTable({
    a()
  })
  
  output$Regulatory <- renderDataTable({
    
    
    req( length(input$genes) > 0 )
    
    Regulators <- data.frame(get_regulatory_summary(e_coli_regulondb,
                                                    gene_regulators = get_gene_regulators(e_coli_regulondb, input$genes)
    )) 
    
    
    
    RegulatorsGene <- Regulators %>% select("Regulated_genes","TF","Percent", "Activator", "Repressor", "Dual")
    
    RegulatorsTrans <- Regulators %>% select("TF","Regulated_genes","Percent", "Activator", "Repressor", "Dual")
    
    
    
    if (input$ByTrans_Gene == "Factor_transcripcion") {
      RegulatorsTrans
    } else {
      RegulatorsGene
    }
    
    
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
