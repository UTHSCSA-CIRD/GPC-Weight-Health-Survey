library(shinyBS)
library(shiny)
library(ggplot2)
library(shinyjs)
source("obesitySurveyHelpers.R")


shinyServer(
  function(input, output, session){
    createAlert(session, "graphError", "gError", content = "Loading, please wait...", title = "Please wait", append = FALSE)
    #REPLACED WITH LOAD FUNCTION
    #     obd <- read.table("obesity_survey_v0.3.csv", header = TRUE, sep = "\t")
#     
#     #convert non informative race__1 race__2 titles to White/caucasian  Black/African American etc.
#     colnames(obd)[58:63] = c("White", "Black", "American_Indian", "Asian", "Other", "PrefNotAnswer")
#     
#     #Clean up in-race names for ggplot -- They're currently too long and overlapping
#     levels(obd[,58])<-c("0", "0", "White")
#     levels(obd[,59])<-c("0", "Black")
#     levels(obd[,60])<-c("0", "American_Indian")
#     levels(obd[,62])<-c("0", "Other")
#     levels(obd[,63])<-c("0", "NA")
#     
#     #Clean up willing to participate answers for ggplot -- They're currently too long and overlapping
#     levels(obd$possible_research)<-c("", "Maybe", "NA", "No", "Yes")
#     
#     #Clean pt_sex
#     levels(obd$pat_sex) = c("F", "F", "M", "M")
#     
#     # Arrange the levels for income to keep like incomes together
#     obd$income <- factor(obd$income, levels(obd$income)[c(8,2,4,3,7,5,6,1)])
#     obd$Race <- apply(obd[,58:62], 1,concatRace)
#     obd$Race <- as.factor(obd$Race)
#     obd$surv_2 <- apply(obd[,17:72], 1, surveyResponded)
#     samp = pickSample(obd, .25)
    load("survSave.rdata")
    dataDic = lapply(obd, class)
    
    session$sendCustomMessage(type = "bsAlertClose", "gError")
    
    output$graphSidePanel <- renderUI({
      vals = names(dataDic[dataDic == "factor"])
      flowLayout(
        p("Currently only barplots are available, please make your selections."),
        selectInput("xVal", "X Value", vals ),
        
        checkboxInput("xOmit", "Omit blanks?", value = TRUE),
        selectInput("plotFill", "Fill Value", vals),
        checkboxInput("fillOmit", "Omit blanks?", value = FALSE)
      )
    })#end output$graphSidePanel
    
    output$visPlot <- renderPlot({
      validate(
        need(input$xVal, 'Please select an X Value.'),
        need(input$plotFill, 'Please select a fill value')
      )
      runGGPLOT(samp, input$xVal, input$plotFill , xlab = input$xVal, ylab = input$plotFill, omitNA_X = input$xOmit, omitNA_Y = input$fillOmit)
    })#end output$visPlot
  }
)