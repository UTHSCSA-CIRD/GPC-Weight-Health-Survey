runByRaceVariable <- function(data, fill, title = "", ylab = "Percent", xlab = "Race"){
  require(ggplot2)
  ggplot(data = data)+ 
    geom_bar(aes_string(x = "PreferNotAnswer", fill = fill), position = "fill")+
    geom_bar(aes_string(x = "OtherRace", fill = fill), position = "fill")+ 
    geom_bar(aes_string(x = "American_Indian", fill = fill), position = "fill")+
    geom_bar(aes_string(x = "Asian", fill = fill), position = "fill")+ 
    geom_bar(aes_string(x = "Black", fill = fill), position = "fill")+ 
    geom_bar(aes_string(x = "White", fill = fill), position = "fill")+ 
    labs(title = title, y = ylab, x = xlab) +
    theme(axis.text.x=element_text(angle=45, hjust = 1))
}
runByWilling2P <- function(data, fill, title = "", ylab = "Percent", xlab = "Willing To Participate?"){
  require(ggplot2)
  ggplot(data = data)+ 
    geom_bar(aes_string(x = "possible_research", fill = fill), position = "fill")+
    labs(title = title, y = ylab, x = xlab)
}
runGGPLOT <- function(data, x, fill, title = "", ylab = "Percent", xlab = "", omitNA_X=TRUE, omitNA_Y = FALSE, position = "stack"){
  require(ggplot2)
  if(omitNA_X){
    data = data[(data[,x] !="0" & data[,x] != ""),]
  }
  if(omitNA_Y){
    data = data[(data[,fill] !="0" & data[,fill] != ""),]
  }
  ggplot(data = data)+ 
    geom_bar(aes_string(x = x, fill = fill), position = position)+
    labs(title = title, y = ylab, x = xlab)
}
ggMosaicPlot <- function(var1, var2){
  #Code by: http://stackoverflow.com/users/2119315/edwin
  #MOSAIC PLOT
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))
  
  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2
  
  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) 
}

pickSample <- function (data, percent){
  if(percent > 1 && percent < 100) percent = percent/100
  if(percent >1) stop("Percent is not valid")
  size = nrow(data)* percent
  s = sample(nrow(data), size)
  data[s,]
}
concatRace <- function(x){
  race = ''
  if(x[1] == "White") race = paste(race , "White")
  if(x[2] == 'Black') race = paste(race , "Black")
  if(x[3] == "American_Indian") race = paste(race , "American_Indian")
  if(x[4] == "Asian") race = paste(race , "Asian")
  if(x[5] == "Other"){
    if(race == '') race = "Other"
  }
  gsub('^[ ]','',race)
}


surveyResponded <- function(a){
  #takes a single array and steps through it returning whether or not ALL values in that array are null 0 or NA
  for(r in a){
    if(!is.na(r) & !is.null(r) & r != "0" & r != "NA" & r != "" & r != "None") return (TRUE)
  }
  return (FALSE)
}

  
