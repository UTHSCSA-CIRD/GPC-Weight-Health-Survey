shinyUI(
  fluidPage(
    titlePanel("Obesity Survey Sample Data Review"),
    sidebarLayout(
      sidebarPanel(
        #Graph options
        uiOutput("graphSidePanel")
      ), #end sidebar panel
      mainPanel(
        bsAlert("graphError"),
        plotOutput("visPlot")
        #graph
        #uiOutput("ui")
      )#end mainPanel
    )#end sidebarLayout
  )#end fluidPage
  #
)