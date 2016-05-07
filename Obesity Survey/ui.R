library(shinyBS)
library(shiny)
library(shinyjs)

shinyUI(
  fluidPage(
    HTML("<span style='font-size:35px'><span style ='color:orange'>T</span><span style ='color:#B2B200'>A</span><span style ='color:orange'>B</span><span style ='color:#B2B200'>S</span><span style ='color:orange'>I</span><span style='color:#B2B200'>E</span></span> <span style='font-size:15px'> <span style ='color:orange'>   Table </span><span style ='color:#B2B200'>Analyzer, </span><span style ='color:orange'>Browser,</span><span style ='color:#B2B200'> and Summarizer </span><span style ='color:orange'> for Informatics</span><span style ='color:#B2B200'> Exploration</span></span>
         <p>Clinical Informatics Research Division (CIRD), University of Texas Health Science Center at San Antonio (UTHSCSA)</h5></p>"),
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
            checkboxInput("constSurv2RespOnly","Only Survey 2 Respondents?"),
            checkboxInput("focusedPCA", "Focused PCA Plot?"),
            uiOutput("consetllationSide")
        )),#end constellation sideBar
        mainPanel(
          plotOutput("constellationPlot",height="800px")
        )#end constellation mainpanel
      ))#End sidbarLayout/TabPanel CONSTELLATIONS
    
    ),#end tabsetPanel
    HTML('<footer><center> <hr>TABSIE v1.0.2, GPL v2 2016.</br> Authors: Laura Manuel, Alex Bokov, and the CIRD team.<br/>For more information, please contact <a href="mailto:informatics@uthscsa.edu">informatics@uthscsa.edui</a></center></footer>')
  )#end fluidPage
)
