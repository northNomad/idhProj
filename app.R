
# Load Packages -----------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(RColorBrewer)
  library(MAGeCKFlute)
  library(CytoExploreR)
  library(patchwork)
  library(gt)
})


#Template
ui <- fluidPage(
  h3("Gr1+ vs Gr1- ~ In Ivosidenib"),
  
  #input & plot: genes sorted by rra-score
  numericInput(inputId = "n.TopGenes", 
               label = "Select #TopGenes For Display",
               value = 5, min = 0, step = 1),
  #Label a specific gene
  textInput(inputId = "label.Gene", 
            label = "Label a specific gene(s) - separate by ';' with no space",
            value = ""),
  #Trigger by actionbutton
  actionButton(inputId = "plotUpdate", label = "Update Plot"),
  plotOutput("TopGenes"),
  
  
  
  #query / find a gene using grep
  textInput(inputId = "text.Gene",
            label = "Find the name of a gene"
  ),
  textOutput("text.Gene_grepReturn")
)

server <- function(input, output){
  #Load crispr screen result (Gr1+ vs Gr1- ~ In Ivosidenib)
  sgRNA.Summary <- MAGeCKFlute::ReadsgRRA("www/mageck/Gr1PositivevsGr1Neg.Ivo/Gr1PosvsGr1Neg.Ivo.sgrna_summary.txt")
  gene.Summary <- MAGeCKFlute::ReadRRA("www/mageck/Gr1PositivevsGr1Neg.Ivo/Gr1PosvsGr1Neg.Ivo.gene_summary.txt",
                                       score = "rra")
  gene.Summary$Rank <- rank(gene.Summary$Score)
  
  #Reactive arguments for making rra rank plot
  top <- eventReactive(input$plotUpdate, {input$n.TopGenes})
  toplabels <- eventReactive(input$plotUpdate, {input$label.Gene %>% str_split(pattern = ";") %>% unlist()})
  
  output$TopGenes <- renderPlot({
    ScatterView(gene.Summary, x = "Rank", y = "Score", label = "id", auto_cut_y = TRUE, 
                groups = c("top", "bottom"), top = top(), toplabels = toplabels(),
                group_col = c("firebrick", "navy"), ylab = "MAGeCK RRA Score")
  }
  )
  
  #find a gene using grep
  text.Gene <- reactive({input$text.Gene %>% grep(gene.Summary$id, ignore.case = TRUE, value = TRUE)})
  output$text.Gene_grepReturn <- renderText({text.Gene()})
  
}

shinyApp(ui = ui, server = server)