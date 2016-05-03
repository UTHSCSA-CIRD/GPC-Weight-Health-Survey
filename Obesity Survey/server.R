library(shinyBS)
library(shiny)
library(ggplot2)
library(shinyjs)
library(e1071);
library(psy);
source("obesitySurveyHelpers.R")
source("ciRd.R")

shinyServer(
  function(input, output, session){
    #createAlert(session, "graphError", "gError", content = "Loading, please wait...", title = "Please wait", append = FALSE)
    #REPLACED loading functions
        load("survSave.rdata")
    dataDic = lapply(samp, class)
    valsFactor = names(dataDic[dataDic == "factor"])
    valsNumeric = names(dataDic[dataDic == "numeric" | dataDic == "integer" ])
    valsNonText = c(valsFactor, valsNumeric)
    #session$sendCustomMessage(type = "bsAlertClose", "gError")
    anim <- animationOptions(loop=T,interval=1500);
    output$graphSidePanel <- renderUI({
      fluidRow(
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
      out <- table(samp[,c(input$xVal,input$plotFill)]);
      if(input$xVal==input$plotFill) out else addmargins(out);
    })# END OUTPUT freqTable
    output$consetllationSide <- renderUI({
      if(input$focusedPCA){
        return({verticalLayout(
          hr(),
          p("The closer a point is to the center, the more closely the column it 
            represents is correlated with the response variable in the center. Green 
            indicates positive correlations and yellow, inverse correlations.
            the response variable."),
          hr(),
          selectInput("constResponseVar","Response Variable", valsNonText)
        )})
      }else{
        return({
          verticalLayout(
            hr(),
            p("Each point is a column from the dataset. The closer they are together the 
            stronger their positive correlation, and the closer they are to 180 degrees,
            the stronger their negative correlation. Variables 90 degrees to each other are 
            uncorrelated (independent)."),
            hr(),
            p("Use these sliders to rotate the points until they become easy to see."),
            sliderInput('constVSlider', "Y-Axis", min = 0, max = 180, value = 1, step = 5, round = 0, animate = anim),
            sliderInput('constHSlider', "X-Axis", min = 0, max = 180, value = 1, step = 5, round = 0, animate = anim),
            sliderInput('constFSlider', "Z-Axis", min = 0, max = 180, value = 1, step = 5, round = 0, animate = anim)
          )
        })
      }#end else not focused PCA
    })#END OUTPUT constellationSide 
    output$constellationPlot <- renderPlot({
      if(input$focusedPCA){
        validate(
          need(input$constResponseVar, 'Response variable must be selected for a focused PCA plot')
        )
        if(input$constSurv2RespOnly){
          pcawrap(subset(samp,s2resp=='Yes'), respvar = input$constResponseVar, pca='f'                       
                  ,contraction='Yes')
        }else{
          pcawrap(samp, respvar = input$constResponseVar,pca='f', contraction='Yes')
        }
      }else{
        validate(
          need(input$constVSlider, 'Warning slider value missing'),
          need(input$constHSlider, 'Warning slider value missing'),
          need(input$constFSlider, 'Warning slider value missing')
        )
        if(input$constSurv2RespOnly){
          pcawrap(subset(samp,s2resp=='Yes'), nbsphere=1, back=T,v=input$constVSlider,
                  h=input$constHSlider,f=input$constFSlider)
        }else{
          pcawrap(samp, nbsphere=1, back=T,v=input$constVSlider,
                  h=input$constHSlider,f=input$constFSlider)
        }
      }
    })#END PLOT constellationPlot
    output$boxPlotSide <- renderUI({
      verticalLayout(
        selectInput("boxPlotX","Discrete Variable(X)", valsFactor),
        selectInput("boxPlotY", "Continuous Variable(Y)", valsNumeric)
    )})# END UI for boxPlot Sidebar
    output$boxPlot <- renderPlot({
      validate(
        need(input$boxPlotX, 'Warning X value for boxplot missing.'),
        need(input$boxPlotY, 'Warning Y value for boxplot missing.')
      )
      ggplot(samp, aes_string(input$boxPlotX, input$boxPlotY)) +
          geom_boxplot()
    })#End plot for boxPlot
    output$violinPlotSide <- renderUI({
      verticalLayout(
        selectInput("violinPlotX","Discrete Variable(X)", valsFactor),
        selectInput("violinPlotY", "Continuous Variable(Y)", valsNumeric)
      )})# END UI for boxPlot Sidebar
    output$violinPlot <- renderPlot({
      validate(
        need(input$violinPlotX, 'Warning X value for violin plot missing.'),
        need(input$violinPlotY, 'Warning Y value for violin plot missing.')
      )
      ggplot(samp, aes_string(input$violinPlotX, input$violinPlotY)) +
        geom_violin()
    })#End plot for boxPlot
    
  }
)
