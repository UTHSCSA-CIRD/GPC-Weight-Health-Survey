runByRaceVariable <- function(data, fill, title = "", ylab = "Percent", xlab = "Race"){
  require(ggplot2)
  ggplot(data = data)+ 
    geom_bar(aes_string(x = "PrefNotAnswer", fill = fill), position = "fill")+
    geom_bar(aes_string(x = "Other", fill = fill), position = "fill")+ 
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
runGGPLOT <- function(data, x, fill, title = "", ylab = "Percent", xlab = ""){
  require(ggplot2)
  ggplot(data = data)+ 
    geom_bar(aes_string(x = x, fill = fill), position = "fill")+
    labs(title = title, y = ylab, x = xlab)
}
ggMMplot <- function(var1, var2){
  #Code by: http://stackoverflow.com/users/2119315/edwin
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
  race = '0'
  if(x[1] == "White") race = "White"
  if(x[2] == 'Black') {
    if(race == '0'){
      race = "Black"
    }else{
      race = paste(race , "Black")
    }
  }
  if(x[3] == "American_Indian"){
    if(race == '0'){
      race = "American_Indian"
    }else{
      race = paste(race , "American_Indian")
    }
  }
  if(x[4] == "Asian"){
    if(race == '0'){
      race = "Asian"
    }else{
      race = paste(race , "Asian")
    }
  }
  if(x[5] == "Other"){
    if(race == '0'){
      race = "Other"
    }
  }
  race
}

surveyResponded <- function(a){
  #takes a single array and steps through it returning whether or not ALL values in that array are null 0 or NA
  for(r in a){
    if(is.na(r) || is.null(r) || r == "0" || r == "NA" || r == "" || r == "None") {
      }else {return (TRUE)}
  }
  return (FALSE)
}

  