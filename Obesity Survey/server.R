library(shinyBS)
library(shiny)
library(ggplot2)
library(shinyjs)
source("obesitySurveyHelpers.R")

shinyServer(
  function(input, output, session){
    #createAlert(session, "graphError", "gError", content = "Loading, please wait...", title = "Please wait", append = FALSE)
    #REPLACED loading functions
        load("survSave.rdata")
    dataDic = lapply(obd, class)
    valsFactor = names(dataDic[dataDic == "factor"])
    #session$sendCustomMessage(type = "bsAlertClose", "gError")
    
    output$graphSidePanel <- renderUI({
      flowLayout(
        p("Currently only barplots are available, please make your selections."),
        selectInput("xVal", "X Value", valsFactor ),
        
        checkboxInput("xOmit", "Omit blanks?", value = TRUE),
        selectInput("plotFill", "Fill Value", valsFactor),
        checkboxInput("fillOmit", "Omit blanks?", value = FALSE),
        p("Additional Barplot features:"),
        checkboxInput("barProportion", "Compare proportions", value = FALSE)
      )
    })#end output$graphSidePanel
    
    output$visPlot <- renderPlot({
      validate(
        need(input$xVal, 'Please select an X Value.'),
        need(input$plotFill, 'Please select a fill value')
      )
      if(input$barProportion) position = "fill"
      else position = "stack"
      runGGPLOT(samp, input$xVal, input$plotFill , xlab = input$xVal, ylab = input$plotFill, omitNA_X = input$xOmit, omitNA_Y = input$fillOmit, position = position)
    })#end output$visPlot
    output$freqTable <- renderTable({
      validate(
        need(input$xVal, 'Please select an X Value.'),
        need(input$plotFill, 'Please select a fill value')
      )
      table(samp[,input$xVal], samp[,input$plotFill])
    })
  }
)