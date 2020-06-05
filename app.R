setwd("C:/Users/Alex Liu/Desktop/shiny/idhProject/")

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
ui <- navbarPage("mIDH Project",
# Tab1 - Crispr Screen Result ---------------------------------------------

  tabPanel("Crispr Screen Result - MAGeCK",
    withMathJax(),
    tags$style(HTML("div.MathJax_Display{text-align: left !important;}")),
    tags$style(HTML("img {border: 1; max-width: 100%; max-height: 100%;}")),
    tags$style(HTML("element.style {width: 33.33%; height:33.33%;}")),
    
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
  ),

# Tab2 - Validation of Screen ---------------------------------------------
  tabPanel("Clec5a",
    
    navlistPanel("Experiments",
      #Validation Experiment           
      tabPanel("Knockout Experiment",
        fluidRow(
          tags$div("Based on the result of our screen, we sought to validate Clec5a as a hit and investigate the consequences of its knockout at a deeper resolution."),
          tags$hr(),       
          h3(tags$strong("Hypothesis: Clec5a knockout synergizes with Ivosidenib to promote OCI-mIDH1/NPM1 differentiation")),
        ),
        fluidRow(
          column(6, h4(tags$b("Fig1-a")), imageOutput("plot_summary", inline = TRUE)),
          column(6, h4(tags$b("Fig1-b")), imageOutput("plot_clec5a_1D", inline = TRUE))
        ),
        fluidRow(tags$hr()),
        fluidRow(
          column(8, h4(tags$b("Fig1-c")), imageOutput("plot_clec5a_gr1", height = "800px")),
          column(4, 
                 tags$div(h4("Discussion: "),
                          h5("Flow cytometry confirms Clec5a knockouts' reduced expression of targeted gene (Fig1-b).",
                          "In addition, Clec5a knockouts express higher levels of myeloid differentiation marker Gr1 compared to nontarget control, both at baseline (DMSO-treated) and especially in the presence of Ivosidenib (Fig1-a,c)."),
                          tags$hr(),
                          "Fig1-a: Line connects mean %Gr1+ of experimental replicates (n=2) across DMSO and Ivosidenib treatment at day 7."
                          )
                 )
        )
      ),  
      #Cell Density Experiment  
      tabPanel("Cell Density Experiment", 
        fluidRow("Figures go here")       
      )
    )
  ),
  
    

# Tab N - Methods ---------------------------------------------------------
  tabPanel("Methods",
   
   tags$h1("Crispr/Cas9 mediated knockout via electroporation"),
   tags$b("[Cas9 NLS] = 20 uM"),
   tags$br(),
   tags$b("[sgRNA] = 100 uM"),
   tags$br(),
   tags$b("[Nuclear Factor Solution (NFS)] = Prepared at manufacture's recommendation"),
       
   
   uiOutput("nucleofection_recipe")
  )
)

server <- function(input, output){
# Tab1 --------------------------------------------------------------------

  #Load crispr screen result (Gr1+ vs Gr1- ~ In Ivosidenib)
  sgRNA_summary <- MAGeCKFlute::ReadsgRRA("www/mageck/Gr1PositivevsGr1Neg.Ivo/Gr1PosvsGr1Neg.Ivo.sgrna_summary.txt")
  gene_summary <- MAGeCKFlute::ReadRRA("www/mageck/Gr1PositivevsGr1Neg.Ivo/Gr1PosvsGr1Neg.Ivo.gene_summary.txt",
                                       score = "rra")
  gene_summary$Rank <- rank(gene_summary$Score)
  
  #Reactive arguments for making rra rank plot
  top <- eventReactive(input$plotUpdate, {input$n.TopGenes})
  toplabels <- eventReactive(input$plotUpdate, {input$label.Gene %>% str_split(pattern = ";") %>% unlist()})
  
  output$TopGenes <- renderPlot({
    ScatterView(gene_summary, x = "Rank", y = "Score", label = "id", auto_cut_y = TRUE, 
                groups = c("top", "bottom"), top = top(), toplabels = toplabels(),
                group_col = c("firebrick", "navy"), ylab = "MAGeCK RRA Score")
    }
  )
  
  #find a gene using grep
  text.Gene <- reactive({input$text.Gene %>% grep(gene_summary$id, ignore.case = TRUE, value = TRUE)})
  output$text.Gene_grepReturn <- renderText({text.Gene()})
  

# Tab2 --------------------------------------------------------------------
  output$plot_clec5a_1D <- renderImage({
    f_path <- file.path("www/images/Clec5a/plot_clec5a_1D.png")
    list(src = f_path)
  }, deleteFile = FALSE)
  
  output$plot_summary <- renderImage({
    f_path <- file.path("www/images/Clec5a/plot_clec5a_summary.png")
    list(src = f_path)
  }, deleteFile = FALSE)
  
  output$plot_clec5a_gr1 <- renderImage({
    f_path <- file.path("www/images/Clec5a/clec5a_Gr1.png")
    list(src = f_path)
  }, deleteFile = FALSE)
  
# Tab Methods --------------------------------------------------------------------
  output$nucleofection_recipe <- renderUI({
    withMathJax(helpText("Prepare RNP complex by mixing $$1.5 uL Cas9 + 2.5 uL sgRNA + 21 uL NFS$$"))
  })
  
}



shinyApp(ui = ui, server = server)
