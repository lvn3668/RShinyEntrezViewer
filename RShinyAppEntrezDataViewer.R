#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(mongolite)
library(lubridate)
library(gridExtra)
library("XML")
library("methods")
library(xml2)
library(tidyverse)
library(rvest)
library(hash)
library(heatmaply)
source("HsaggregateQueries.R")
source("HsEntrezGeneQueries.R")
source("MmEntrezGeneQueries.R")
source("MongoDBConnection.R")
source("HsaggregateQueries.R")
library(Hmisc)
# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Analysis of Entrez Gene data"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      titlePanel("Data Visualization"),
      sidebarPanel(uiOutput("Choice of box plot")),
      
      selectInput(
        inputId = 'organism',
        label = 'Choose a organism',
        choices = c('human', 'mouse'),
      ),
      
      uiOutput(outputId = 'chosenorganism'),
      selectInput(
        inputId = "ChoiceOfBoxPlot",
        label = "Choose gene function, whose distribution is to be viewed across human chromosomes",
        choices = c(
          "Protein coding genes",
          "Lysosomal genes",
          "Musculoskeletal genes"
        )
      ),
      
      uiOutput("secondSelection"),
      numericInput(
        inputId = "numberofrecordstodisplay",
        label = "Number of observations to view:",
        value = 10,
        min = 10
      ),
      
      
      #fileInput("files", "Choose File", multiple = TRUE),
      #checkboxInput("center", "Center", TRUE),
      #selectInput("cv", "cross-validation",
      #            list (none =
      #                     "none", Q2 =  "q2")),
      #checkboxInput("outliers", "Show outliers", TRUE)
      
      uiOutput("PCs"),
      
      # selectInput(
      #   "scaling",
      #   "Scale",
      #   list(
      #     none = "none",
      #     "unit variance" = "uv",
      #     pareto = "pareto"
      #   )
      # ),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("caption", container = span)),
      plotOutput("boxPlot"),
      verbatimTextOutput("summary"),
      tableOutput("humandataview"),
      tableOutput("mousedataview")
    )
  ))


# Define server logic required to draw a histogram
#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session) {
  values <- reactiveValues()
  output$chosenorganism <- renderUI({
    if (input$organism == 'human') {
      selectInput(
        inputId = c("humanqueries"),
        label = 'Choice of datasets for visualization',
        choices = c(
          "All distinct genes for species homo sapiens in Entrez",
          "Gene Symbol and Synonyms for species homo sapiens in Entrez",
          "Count of all genes for species homo sapiens in Entrez",
          "Gene Symbols, Synonyms, Feature types for homo sapines in Entrez",
          "List of all locus tags",
          "Gene Symbols and db cross references",
          "Gene Symbols and chromosomal map locations",
          "Gene Types",
          "Gene, Description and Feature type"
        )
      )
    } else if (input$organism == 'mouse') {
      selectInput(
        inputId = c("mousequeries"),
        label = 'Choice of datasets for visualization',
        choices = c(
          "All distinct genes for species mus musculus in Entrez",
          "Count of all genes for species mus musculus in Entrez",
          "Gene Symbols, Synonyms, Feature types for mus musculus in Entrez",
          "List of all locus tags",
          "Gene Symbols and db cross references",
          "Gene Symbols and chromosomal map locations",
          "Gene Types",
          "Gene, Description and Feature type"
        )
      )
    }
  })
  
  homosapiensmongodbconnection = dbconnectionhs()
  musmusculusphasingdatadbconnection = dbconnectionphasing()
  musmusculusentrezgenedbconnection = dbconnectionmm()
  
  boxpolottobedisplayed <- reactive({
    switch (
      input$ChoiceOfBoxPlot,
      "Protein coding genes" = findNumberOfProteinCodingGenesPerChromosome(homosapiensmongodbconnection),
      "Lysosomal genes" =    findNumberOflysosomalGenesPerChromosome(homosapiensmongodbconnection),
      "Musculoskeletal genes" = findNumberOfskeletalmuscleGenesPerChromosome(homosapiensmongodbconnection)
    )
  })
  
  
  
  data <- eventReactive(input$displayrecords, {
    print("Inside handler for button")
    # get the data from
    dataset <- datasetToBeDisplayed()
    print(dataset)
    df <- data.frame()
    df
  })
  
  output$summarytable <- renderDataTable({
    data()
  })
  
  # Count of protein coding genes by chromosome
  hsproteincodingclass.df <-
    findNumberOfProteinCodingGenesPerChromosome(homosapiensmongodbconnection)
  hslysosomalclass.df <-
    findNumberOflysosomalGenesPerChromosome(homosapiensmongodbconnection)
  hsskeletalmusclecodingclass.df <-
    findNumberOfskeletalmuscleGenesPerChromosome(homosapiensmongodbconnection)
  
  # NA12878phasingdatatsvformat=read.csv(
  #     "NA12878_sv_phasing.tsv",
  #     sep="\t",
  #     header=TRUE,
  #     quote="\"",
  #     comment.char="",
  #     fill=TRUE)
  # musmusculusdbconnection$insert(NA12878phasingdatatsvformat)
  
  
  
  humandatasetToBeDisplayed <- reactive({
    switch(
      input$humanqueries,
      "Count of all genes for species homo sapiens in Entrez" = HsEntrezGeneQueries(entrezhsallrecords(homosapiensmongodbconnection)),
      "All distinct genes for species homo sapiens in Entrez" = entrezhsdistinctgeneids(entrezhsallrecords(homosapiensmongodbconnection)),
      "Gene Symbol and Synonyms for species homo sapiens in Entrez" = hsSymbolsSynonymsFeaturetype(entrezhsallrecords(homosapiensmongodbconnection)),
      "Gene Symbols, Synonyms, Feature types for homo sapines in Entrez" = hsSymbolsSynonymsFeaturetype(entrezhsallrecords(homosapiensmongodbconnection)),
      "List of all locus tags" = hsLocusTags(entrezhsallrecords(homosapiensmongodbconnection)),
      "Gene Symbols and db cross references" = geneSymbolsAnddbCrossRefs(entrezhsallrecords(homosapiensmongodbconnection)),
      "Gene Symbols and chromosomal map locations" = genesymbolandmaplocation(entrezhsallrecords(homosapiensmongodbconnection)),
      "Gene Types" = extractTypeOfGene(entrezhsallrecords(homosapiensmongodbconnection)),
      "Gene, Description and Feature type" = extractdescriptionfeaturetype(entrezhsallrecords(homosapiensmongodbconnection)),
    )
  })
  
  
  mousedatasetToBeDisplayed <- reactive({
    switch(
      input$mousequeries,
      #Choice for the mouse aggregate queries
      "Count of all genes for species mus musculus in Entrez" = MmEntrezGeneQueries(entrezMmallrecords(musmusculusentrezgenedbconnection)),
      "All distinct genes for species mus musculus in Entrez" = entrezMmdistinctgeneids(entrezMmallrecords(musmusculusentrezgenedbconnection)),
      "Gene Symbols, Synonyms, Feature types for mus musculus in Entrez" = MmSymbolsSynonymsFeaturetype(entrezMmallrecords(musmusculusentrezgenedbconnection)),
      "List of all locus tags" = MmLocusTags(entrezMmallrecords(musmusculusentrezgenedbconnection)),
      "Gene Symbols and db cross references" = geneSymbolsAnddbCrossRefs(entrezMmallrecords(musmusculusentrezgenedbconnection)),
      "Gene Symbols and chromosomal map locations" = genesymbolandmaplocation(entrezMmallrecords(musmusculusentrezgenedbconnection)),
      "Gene Types" = extractTypeOfGene(entrezMmallrecords(musmusculusentrezgenedbconnection)),
      "Gene, Description and Feature type" = extractdescriptionfeaturetype(entrezMmallrecords(musmusculusentrezgenedbconnection))
    )
    
  })
  
  
  # observeEvent(
  #   {
  #     input$humanqueries
  #     input$mousequeries
  #   },
  #   {
  #     df1 <- input$humanqueries
  #     df2 <- input$mousequeries
  #   }
  # )
  
  
  # add a column to find location on chromosome and find average coordinates of that...
  # for now, modification date is place holder for average field
  
  # total genes per chromosome for mouse entrez gene data
  #mmtotalgenesperchromosome <- musmusculusdbconnection$aggregate('[{"$group":{"_id":"$mmchromosome", "count": {"$sum":1}}}]')
  
  #print("after printing total genes per chromosome")
  # x <- hsproteincodingclass.df
  # print(names(x))
  # ggplot(aes(chromosomenumber, numberofproteincodinggenesperchromosome),
  #        data = hsproteincodingclass.df) + geom_col()
  # plotdata <-
  #   reactive({
  #     ((hsproteincodingclass.df[, c("Chromosomenumber",
  #                      "Numberofproteincodinggenesperchromosome")]))
  #   })
  
  
  # output$hist <- renderPlot({
  #   # d1<-(plotData())
  #   # print(ggplot(d1, aes(x=s1)) + geom_histogram(fill = "dark green", alpha = 0.6, binwidth = 1))
  #   # print(str(d1))
  #   print(hsproteincodingclass.df)
  #   hist.data.frame(hsproteincodingclass.df)
  #   hist.data.frame(
  #     hsproteincodingclass.df,
  #     n.unique = 3,
  #     nclass = "compute",
  #     na.big = TRUE,
  #     rugs = TRUE,
  #     freq = TRUE
  #   )
  #hist(hsclass.df,  xname = "Chromosome Number", freq = TRUE, axes=TRUE, plot=TRUE, labels=TRUE, main=paste("Histogram of gene counts by chromosome"), ylim = NULL, density = NULL, include.lowest = TRUE, right = TRUE,
  #     col = NULL, border = NULL, breaks=c(1:24))
  #print(ggplot(data_numberofgenesperchromosome, aes(x=s1)) + geom_histogram(fill = "dark green", alpha = 0.6, binwidth = 1))
  
  #})
  
  
  # Show the first "n" observations ----
  output$humandataview <- renderTable({
    head(humandatasetToBeDisplayed(),
         n = input$numberofrecordstodisplay)
  },
  striped = FALSE,
  bordered = TRUE,
  spacing = "m",
  width = "100%",
  align = 'c',
  rownames = TRUE,
  colnames = TRUE, caption = "Human Data")
  
  # Show the first "n" observations ----
  output$mousedataview <- renderTable({
    head(mousedatasetToBeDisplayed(),
         n = input$numberofrecordstodisplay)
  },
  striped = FALSE,
  bordered = TRUE,
  spacing = "m",
  width = "100%",
  align = 'c',
  rownames = TRUE,
  colnames = TRUE,
  caption = "Mouse Data")
  
  output$boxPlot <- renderPlot({
    boxplotdata <- boxpolottobedisplayed()
    #print(boxplotdata)
    boxplot(
      boxplotdata$Numberofgenesperchromosome ~ boxplotdata$Chromosomenumber,
      data = boxplotdata,
      xlab = "Number of chromosomes",
      ylab = "Number of genes per chromosome",
      border = "green",
      notch = TRUE,
      varwidth = TRUE,
      main = "Box plot distribution
            of number of chosen genes per chromosome",
      col.bg = "yellow",
      col.grid = "black"
    )
  })
  
  output$caption <- renderText({
    input$caption
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
