shinyUI(
  fluidPage(
    titlePanel("Obesity Survey Sample Data Review"),
    tabsetPanel(
      tabPanel("Bar Graph", #tabpanel for the bargraph
        sidebarLayout(#BARGRAPH
          sidebarPanel(
            #Graph options
            uiOutput("graphSidePanel")
          ), #end bargraph sidebar panel
          mainPanel(
    #        bsAlert("graphError"),
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
          plotOutput("constellationPlot")
        )#end constellation mainpanel
      )),#End sidbarLayout/TabPanel CONSTELLATIONS
    tabPanel("Box Plot",sidebarLayout(
      sidebarPanel( # BoxPlot
        uiOutput("boxPlotSide")
      ),#end boxPlot sideBar
      mainPanel(
        plotOutput("boxPlot")
      )#end BoxPlot mainpanel
    )),#End sidbarLayout/TabPanel BOXPLOTTAB
    tabPanel("Violin",sidebarLayout(
      sidebarPanel( # Violin
        uiOutput("violinPlotSide")
      ),#end violinPlot sideBar
      mainPanel(
        plotOutput("violinPlot")
      )#end violinPlot mainpanel
    ))#End sidbarLayout/TabPanel VIOLINPLOTTAB
    )#end tabsetPanel
  )#end fluidPage
  #
)