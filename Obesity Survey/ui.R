library(shinyBS)
library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(
    titlePanel("Obesity Survey Sample Data Review"),
    tabsetPanel(
      tabPanel("Graphs", #tabpanel for the bargraph
        sidebarLayout(#BARGRAPH
          sidebarPanel(
            #Graph options
            uiOutput("graphSidePanel")
          ), #end bargraph sidebar panel
          mainPanel(
            bsAlert("graphError"),
            plotOutput("visPlot"),
            tableOutput("freqTable")
          )#end bargraph mainPanel
        )),#end sidebarLayout/TabPanel BARGRAPH
      tabPanel("Constellation",sidebarLayout(
        sidebarPanel(fluidRow( # CONSTELLATIONS
            checkboxInput("constSurv2RespOnly","Only Survey 2 Respondants?"),
            checkboxInput("focusedPCA", "Focused PCA Plot?"),
            uiOutput("consetllationSide")
        )),#end constellation sideBar
        mainPanel(
          plotOutput("constellationPlot",height="800px")
        )#end constellation mainpanel
      ))#End sidbarLayout/TabPanel CONSTELLATIONS
    
    ),#end tabsetPanel
    HTML('<footer> This is a test of the shiny app footer system.</footer>')
  )#end fluidPage
)
