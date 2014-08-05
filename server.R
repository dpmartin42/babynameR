require(shiny)
require(devtools)
require(ggplot2)
require(dplyr)

source("plotNames.R")
source("createTable.R")
source("nameSplitter.R")

shinyServer(
  function(input, output) {
    
    output$plot <- renderPlot({
      
      plotNames(input$name, input$sex, format(input$yearRange[1], "%Y"), format(input$yearRange[2], "%Y"))

    })
    
    output$tableBoys <- renderDataTable({
      
      as.data.frame(createTable(nameSplitter(input$name)[1], input$sex, format(input$yearRange[2], "%Y"))[[1]])

    }, options = list(aLengthMenu = c(10, 25, 50), iDisplayLength = 10))
    
    output$tableGirls <- renderDataTable({
      
      as.data.frame(createTable(nameSplitter(input$name)[1], input$sex, format(input$yearRange[2], "%Y"))[[2]])
      
    }, options = list(aLengthMenu = c(10, 25, 50), iDisplayLength = 10))
    
  }
)

