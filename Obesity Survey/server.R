library(shinyBS)
library(shiny)
library(ggplot2)
library(shinyjs)
library(e1071);
library(psy);
source("obesitySurveyHelpers.R")

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
    
    output$graphSidePanel <- renderUI({
      fluidRow(
        p("Currently only barplots are available, please make your selections."),
        selectInput("xVal", "X Value", valsNonText ),
        selectInput("yVal", "Y Value", valsNonText),
        uiOutput("subSelectionOpts")
      )
    })#end output$graphSidePanel
    output$subSelectionOpts <- renderUI({
      if(input$xVal %in% valsFactor){
        if(input$yVal %in% valsFactor){
          verticalLayout(
            p("Additional Barplot features:"),
            checkboxInput("xOmit", "Omit blanks in X?", value = TRUE),
            checkboxInput("yOmit", "Omit blanks in Y?", value = FALSE),
            radioButtons("barProportion", "Bar Plot Format", choices = c("Compare proportions", "Show actual values"), selected = "Show actual values")
          )
        }else{
          verticalLayout(
            radioButtons("boxViolin", "Which visualization?", c("Box plot", "Violin", "Points"), selected = "Box plot"),
            conditionalPanel(condition = "input.boxViolin == 'Points'", uiOutput("pointUIOpts"))#This may end disastrously...
          )
        }
      }else{
        if(input$yVal %in% valsFactor){
          verticalLayout(
            radioButtons("boxViolin", "Which visualization?", c("Box plot", "Violin", "Points"), selected = "Box plot"),
            conditionalPanel(condition = "input.boxViolin == 'Points'", uiOutput("pointUIOpts"))#This may end disastrously...
          )
        }else{
          uiOutput("pointUIOpts")
        }
      }
    })#end OUTPUT subSelectionOpts
    output$pointUIOpts <- renderUI({
      verticalLayout(
        sliderInput('widthSlide', "Point Size or Jitter Width", min = 0, max = 3, value = 0.3, step = .1, round = FALSE),
        sliderInput('alphaSlide', "Point Opacity", min = 0, max = 1, value = 0.2, step = .1, round = FALSE),
        checkboxInput('pointJitter', "Jitter the Points?")
      )
    })
    output$visPlot <- renderPlot({
      validate(
        need(input$xVal, 'Please select an X Value.'),
        need(input$yVal, 'Please select a Y value')
      )
      session$sendCustomMessage(type = "bsAlertClose", "gError")
      if(input$xVal %in% valsFactor){
        if(input$yVal %in% valsFactor){
          #factor factor
          validate(need(input$barProportion, "UI not fully generated, please wait."))
          if(input$barProportion == "Compare proportions") position = "fill"
          else position = "stack"
          runGGPLOTFF(samp, input$xVal, input$yVal , xlab = input$xVal, ylab = input$yVal, omitNA_X = input$xOmit, omitNA_Y = input$yOmit, position = position)
        }else{
          #factor number
          validate(need(input$boxViolin, "UI not fully generated, please wait."))
          if(input$boxViolin == "Points"){
            validate(
              need(input$widthSlide,"UI has not finished rendering, please wait"),
              need(input$alphaSlide,"UI has not finished rendering, please wait")
            )
            if(input$pointJitter) style = "jitter"
            else style = "point"
            runGGPLOTFN(samp,input$xVal, input$yVal, xlab = input$xVal, ylab = input$yVal, style = input$boxViolin, width = input$widthSlide, alpha = input$alphaSlide, pstyle = style)
          }else{
            runGGPLOTFN(samp,input$xVal, input$yVal, xlab = input$xVal, ylab = input$yVal, style = input$boxViolin)
          }
        }
      }else{
        if(input$yVal %in% valsFactor){
          #number, factor
          validate(need(input$boxViolin, "UI not fully generated, please wait."))
          # I, Alex shall buy Laura dinner if anybody ever encounters a problem caused by commenting out the following line:
          #createAlert(session,"graphError","gError", content ="Axis inverted to keep your distribution variable numeric.", title= "Warning", append = FALSE)
          if(input$boxViolin == "Points"){
            validate(
              need(input$widthSlide,"UI has not finished rendering, please wait"),
              need(input$alphaSlide,"UI has not finished rendering, please wait")
            )
            if(input$pointJitter) style = "jitter"
            else style = "point"
            p = runGGPLOTFN(samp,input$yVal, input$xVal, xlab = input$yVal, ylab = input$xVal, style = input$boxViolin, width = input$widthSlide, alpha = input$alphaSlide, pstyle = style)
            p = p + coord_flip()
            return(p)
          }else{
            p = runGGPLOTFN(samp,input$yVal, input$xVal, xlab = input$yVal, ylab = input$xVal, style = input$boxViolin) 
            p = p + coord_flip()
            return(p)
          }
        }else{
          validate(
            need(input$widthSlide,"UI has not finished rendering, please wait"),
            need(input$alphaSlide,"UI has not finished rendering, please wait")
          )
          if(input$pointJitter) style = "jitter"
          else style = "point"
          runGGPLOTNN(samp,input$xVal, input$yVal,input$xVal, xlab = input$xVal, ylab = input$yVal, width = input$widthSlide, alpha = input$alphaSlide, pstyle = style)
        }
      }
    })#end output$visPlot
    output$freqTable <- renderTable({
      validate(
        need(input$xVal, 'Please select an X Value.'),
        need(input$yVal, 'Please select a Y value')
      )
      if(input$xVal %in% valsFactor & input$yVal %in% valsFactor){
        out <- table(samp[,c(input$xVal,input$yVal)]);
        if(input$xVal==input$yVal) out else addmargins(out);
      }
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
            sliderInput('constVSlider', "Y-Axis", min = 0, max = 360, value = 1, step = 5, round = 0),
            sliderInput('constHSlider', "X-Axis", min = 0, max = 360, value = 1, step = 5, round = 0),
            sliderInput('constFSlider', "Z-Axis", min = 0, max = 360, value = 1, step = 5, round = 0)
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
    ### THIS CODE IS USED FOR PORTAL R SO THAT THE R SESSION ENDS WHEN THE BROWSER IS CLOSED!!
#     session$onSessionEnded(function() { 
#       stopApp()
#       q("no") 
#     })
  }
)
