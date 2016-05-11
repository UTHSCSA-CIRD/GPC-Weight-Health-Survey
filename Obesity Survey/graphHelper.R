#This file exists to keep all of the graphing options in one place and clean up the server code. 

getPointPlot <- function(pdata, input, type){
  validate(
    need(input$sizeSlide, warningRender),
    need(input$alphaSlide, warningRender)
  )
  if(input$pointJitter) {
    validate(need(input$widthSlide, warningRender))
    style = "jitter"
  }else style = "point"
  if(type =='FN')if(input$xOmit)pdata = pdata[(pdata[,x] !="0" & pdata[,x] != ""),]
  pColor = NULL
  pShape = NULL
  if(input$pointColor != "No color") pColor = input$pointColor
  if(input$pointShape != "No shape") pShape = input$pointShape
  runGGPLOTNN(pdata,input$xVal, input$yVal, width = input$widthSlide, size = input$sizeSlide, alpha = input$alphaSlide, pstyle = style, pColor = pColor, pShape = pShape)
}

runGGPLOTNN <- function(data, x, y, width = 0.3, size = 1, alpha = 0.2, pstyle = "jitter", pColor = "black", pShape = 1){
  require(ggplot2)
  p = ggplot(data = data, aes_string(x = x, y = y))
  if(!is.null(pColor))p = p + aes_string(colour = pColor)
  if(!is.null(pShape))p = p + aes_string(shape = pShape)
  if(pstyle == "jitter"){
    p = p + geom_jitter(width = width, size = size, alpha = alpha)
  }else{
    p = p+ geom_point(size = size, alpha = alpha)
  }
  p
}

runGGPLOTFF <- function(data, x, fill, omitNA_X=TRUE, omitNA_Y = FALSE, position = "stack"){
  require(ggplot2)
  if(omitNA_X){
    data = data[(data[,x] !="0" & data[,x] != ""),]
  }
  if(omitNA_Y){
    data = data[(data[,fill] !="0" & data[,fill] != ""),]
  }
  ggplot(data = data)+ 
    geom_bar(aes_string(x = x, fill = fill), position = position)+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_discrete(guide = guide_legend(reverse = TRUE))
}

runGGPLOTFN <- function(data, x, y, style = "Box plot", omitNA_X= TRUE, ...){
  require(ggplot2)
  if(omitNA_X){
    data = data[(data[,x] !="0" & data[,x] != ""),]
  }
  styleOpts = c("Box plot", "Violin")
  if(!style %in% styleOpts) stop(paste("Error, style must be one of the following: ", toString(styleOpts)))
  p = ggplot(data = data, aes_string(x = x, y = y))
  switch(style
         , "Box plot" = {p = p + geom_boxplot()} 
         , "Violin" = {p = p + geom_violin()})
  p
}
addTheme <- function(p, input){
  if(input$titleField != ""){
    p = p + ggtitle(input$titleField)
  }
  #this allows them to add a title without having to fill in the x and y labels with something other than the default
  if(input$xLab != "" | input$yLab != ""){
    p = p + labs(x = input$xLab, y = input$yLab)
  }
  p
}
