require(shinyBS)
require(shiny)
require(ggplot2)
require(shinyjs)
require(e1071);
require(psy);
source("obesitySurveyHelpers.R")
source("graphHelper.R")

shinyServer(
  function(input, output, session){
    #createAlert(session, "graphError", "gError", content = "Loading, please wait...", title = "Please wait", append = FALSE)
    #REPLACED loading functions
        load("survSave.rdata")
    dataDic = lapply(samp, class)
    valsFactor = names(dataDic[dataDic == "factor"])
    valsNumeric = names(dataDic[dataDic == "numeric" | dataDic == "integer" ])
    valsNonText = c(valsFactor, valsNumeric)
    #NOTE! ggplot will only apply a shape to the first 6 levels of a factor, thus we will only show
    #factors with 6 or fewer factor levels!
    valsShape = valsFactor[sapply(valsFactor, function(x) length(levels(samp[,x])))<=6]
    
    filtered = subset(samp,s2resp=='Yes')
    #session$sendCustomMessage(type = "bsAlertClose", "gError")
    
    output$graphSidePanel <- renderUI({
      tabsetPanel(
        tabPanel("Basic",
          verticalLayout(
            shinyjs::useShinyjs(),
            inlineCSS(list(.disabled = "color:grey")),
            div(id = "filterFlagDiv",
            checkboxInput("surv2RespOnly","Only Survey 2 Respondants?")
            ),
            selectInput("xVal", "X Value", valsNonText ),
            selectInput("yVal", "Y Value", valsNonText),
            uiOutput("subSelectionOpts"),
            checkboxInput("coordFlop","Flip Coordinate Plains (X->Y, Y->X)")
          )
        ),#end basic tab
        tabPanel("Advanced",
           verticalLayout(
             shinyjs::useShinyjs(),
             a(id="toggleTheme", "Show/hide theme options", href ="#"),
             shinyjs::hidden(div(id="themeDiv",
                 textInput("titleField", "Graph Title", value = "", placeholder = "Enter the graph's title."),
                 textInput("xLab", "X-Axis Label", value = ""),
                 textInput("yLab", "Y-Axis Label", value = ""),
                 actionButton("clearTheme", "Clear Theme Form")
             )),#end div "theme
             a(id="togglePoint", "Show/hide point options", href ="#"),
             shinyjs::hidden(div(id="pointDiv",
               p("Note: These options will only have an effect on point graphs."),
               selectInput("pointColor", "Color Value", c("No color", valsNonText), selected = "No color"),
               selectInput("pointShape", "Shape Value", c("No shape", valsShape), selected = "No shape")
             ))#end div point options
           )
        )#end advanced tab
      )#end tab panel
    })#end output$graphSidePanel
    observeEvent(input$clearTheme, {
      updateTextInput(session, "titleField", value = "")
      updateTextInput(session, "xLab", value = "")
      updateTextInput(session, "yLab", value = "")
    })
    observe({
      validate(
        need(input$xVal, "")
      )
      shinyjs::onclick("toggleTheme", toggle(id = "themeDiv", anim= TRUE))
      shinyjs::onclick("togglePoint", toggle(id = "pointDiv", anim= TRUE))
      shinyjs::onclick("toggleViolin", toggle(id = "violinDiv", anim= TRUE))
    })
    
    #this observe handles the enabling and disabling of the "filter" text based on if it has
    #any effect on the graph.
    observe({
      validate(
        need(input$xVal,""),
        need(input$yVal, "")
      )
      enabled = TRUE
      if(input$xVal %in% valsNumeric){
        #feel free to make this more elegantly R
        if(all(filtered[is.finite(filtered[,input$xVal]),input$xVal] == samp[is.finite(samp[,input$xVal]),input$xVal])){
          enabled = FALSE
        }
      }
      if(input$yVal %in% valsNumeric){
        if(all(filtered[is.finite(filtered[,input$yVal]),input$yVal] == samp[is.finite(samp[,input$yVal]),input$yVal])){
          enabled = FALSE
        }
      }
      if(enabled){
        shinyjs::removeClass("filterFlagDiv", "disabled")
        shinyjs::enable("surv2RespOnly")
      }else{
        shinyjs::addClass("filterFlagDiv", "disabled")
        shinyjs::disable("surv2RespOnly")
      }
    })
    
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
            checkboxInput("xOmit", "Omit blanks in X?", value = TRUE),
            radioButtons("boxViolin", "Which visualization?", c("Box plot", "Violin", "Points"), selected = "Box plot"),
            conditionalPanel(condition = "input.boxViolin == 'Points'", uiOutput("pointUIOpts"))#This may end disastrously...
          )
        }
      }else{
        if(input$yVal %in% valsFactor){
          #Testing hot swap of x and y to force factors into X
          tmp = input$yVal
          updateSelectInput(session, "yVal", selected = input$xVal)
          updateSelectInput(session, "xVal", selected = tmp)
        }else{
          uiOutput("pointUIOpts")
        }
      }
    })#end OUTPUT subSelectionOpts
    
    output$pointUIOpts <- renderUI({
      verticalLayout(
        uiOutput("jitter"),
        sliderInput('sizeSlide', "Point Size", min = 0, max = 5, value = 1, step = .5, round = FALSE),
        sliderInput('alphaSlide', "Point Opacity", min = 0, max = 1, value = 0.2, step = .1, round = FALSE),
        if(input$xVal %in% valsNumeric){
          checkboxInput('pointJitter', "Jitter the Points?")
        }else{
          checkboxInput('pointJitter', "Jitter the Points?", value = TRUE)
        }
      )
    })
    
    output$jitter <- renderUI({
      validate(
        need(isolate(input$alphaSlide), warningRender)
      )
      if(input$pointJitter){
        #jitter
        sliderInput('widthSlide', "Jitter Width", min = 0, max = 1, value = 0.3, step = .1, round = FALSE)
      }
    })
    
    output$visPlot <- renderPlot({
      validate(
        need(input$xVal, warningRender),
        need(input$yVal, warningRender)
      )
      #This is the easiest and most adaptable way to handle the narrowing options.
      #We can expand it to an entire method later and just use pdata as the graphing
      #option instead of if this, pass a subset, just always pass pdata. or Plot-data
      #since this is pass by promise not changing the data only creates a pointer, otherwise
      #we were going to change the data anyway (unless the UI for a type of graph wasn't finished
      #rendering, but it's still better to have this here rather than in each if)
      if(input$surv2RespOnly){
        pdata = filtered
      }else{
        pdata = samp
      }
      p = NULL
      #if there is an alert message open, close it.
      session$sendCustomMessage(type = "bsAlertClose", "gError")
      if(input$xVal %in% valsFactor){
        if(input$yVal %in% valsFactor){
          #factor factor
          validate(need(input$barProportion, warningRender))
          if(input$barProportion == "Compare proportions") position = "fill"
          else position = "stack"
          p = runGGPLOTFF(pdata, input$xVal, input$yVal , omitNA_X = input$xOmit, omitNA_Y = input$yOmit, position = position)
        }else{
          #factor number
          validate(need(input$boxViolin, warningRender))
          if(input$boxViolin == "Points"){
            p = getPointPlot(pdata, input, "FN")
            }else{
            p = runGGPLOTFN(pdata,input$xVal, input$yVal, style = input$boxViolin, omitNA_X = input$xOmit)
          }
        }
      }else{#else X is numeric (Note, the issue where Y is a factor is handled by the UI render hot swapping them.)
          p = getPointPlot(pdata, input, "NN")
      }
      p = addTheme(p, input)
      if(input$coordFlop){
        p + coord_flip()
      }else{
        p
      }
      
    })#end output$visPlot
    output$summaryRegion <- renderUI({
      validate(
        need(input$xVal, warningRender),
        need(input$yVal, warningRender)
      )
      if(input$xVal %in% valsFactor & input$yVal %in% valsFactor){
        tableOutput("freqTable")
      }else{
        if(input$yVal %in% valsNumeric){
          verticalLayout(
            p("Summary"),
            tableOutput("summaryTable")#originally had the summary(lm()) after this, but I like just the summary table better- leaving the render table in case we decide to re-add it.
          )
        }
      }
    })
    output$freqTable <- renderTable({#validation done before this is called, no need to repeat
      if(input$surv2RespOnly){
        pdata = filtered
      }else{
        pdata = samp
      }
      addmargins(table(pdata[,c(input$xVal,input$yVal)]))
    })
    
    output$summaryTable <- renderTable({
      if(input$surv2RespOnly){
        pdata = filtered
      }else{
        pdata = samp
      }
      if(input$xVal %in% valsFactor){
        as.table(sapply(split(pdata[,input$yVal],pdata[,input$xVal]),fpSummary))
      }else{
        as.table(sapply(pdata[,c(input$xVal,input$yVal)],fpSummary))
      }
    })
    
    output$lmTable <- renderTable({
      if(input$surv2RespOnly){
        pdata = filtered
      }else{
        pdata = samp
      }
      summary(lm(pdata[,input$yVal] ~ pdata[,input$xVal]))
    })
    
    output$consetllationSide <- renderUI({
      if(input$focusedPCA){
        return({verticalLayout(
          hr(),
          p("The closer a point is to the center, the more closely the column it 
            represents is correlated with the response variable in the center. Green 
            indicates positive correlations and yellow, inverse correlations."),
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
            uncorrelated (independent).
            White filled circles are on the surface of the sphere that is away from the observer and the red ones are currently facing the observer."),
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
      if(exists("input$constSurv2RespOnly")){
        if(constSurv2RespOnly){
          pdata = filtered
        }else{
          pdata = samp
        }
      }else{
        pdata = samp
      }
      if(input$focusedPCA){
        validate(
          need(input$constResponseVar, warningRender)
        )
        pcawrap(pdata, respvar = input$constResponseVar,pca='f', contraction='Yes')
      }else{
        validate(
          need(input$constVSlider, warningRender),
          need(input$constHSlider, warningRender),
          need(input$constFSlider, warningRender)
        )
        pcawrap(pdata, nbsphere=1, back=T,v=input$constVSlider,
                  h=input$constHSlider,f=input$constFSlider)
        
      }
    })#END PLOT constellationPlot
    ### THIS CODE IS USED FOR PORTAL R SO THAT THE R SESSION ENDS WHEN THE BROWSER IS CLOSED!!
#      session$onSessionEnded(function() { 
#        stopApp()
#        q("no") 
#      })
  }
)
