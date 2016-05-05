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

runGGPLOT <- function(data
                      , x, fill, title = ""
                      , ylab = "Percent", xlab = ""
                      , omitNA_X = TRUE, omitNA_Y = FALSE
                      , position = "stack"
                      , isnum = c(is.numeric(data[[x]]),is.numeric(data[[fill]]))
                      , geomOpts = c('box','violin','points')
                      , width = NULL , alpha = NULL, theme = NULL){
  require(ggplot2);
  # set which type of combo plot to use
  geom_combo <- switch(match.arg(geomOpts)
                       ,box=geom_boxplot
                       ,violin=geom_violin
                       ,points={
                         if(is.null(width)) width <- 0.3;
                         if(is.null(alpha)) alpha <- 0.2;
                         geom_jitter;
                         }
                       );
  if(omitNA_X){data = data[(data[,x] !="0" & data[,x] != ""),]}
  if(omitNA_Y){data = data[(data[,fill] !="0" & data[,fill] != ""),]}
  out <- ggplot(data);
  if(all(isnum)){ # start numeric vs numeric case
    if(is.null(alpha)) alpha <- 0.3;
    out <- out + if(x==fill) { # no point in x~x scatterplot, so show distribution 
        geom_histogram(aes_string(x=x)) 
      } else {
        geom_point(aes_string(x=x,y=fill),alpha=alpha) + geom_quantile();
      };
  } # end numeric vs numeric case
  else {
    if(is.null(alpha)) alpha <- 1;
    if(!any(isnum)){ # start discrete vs discrete case
        out <- out + geom_bar(aes_string(x=x,fill=fill),position=position);
        } # discrete vs discrete case
    else if(isnum[1]){ # start x is numeric case
        ylab <- c(ylab,xlab); xlab <- ylab[1]; ylab <- ylab[2];
        out <- out + geom_combo(aes_string(x=fill,y=x),width=width,alpha=alpha) + coord_flip();
        } # end x is numeric case
    else { # start fill is numeric case
      out <- out + geom_combo(aes_string(x=x,y=fill),width=width,alpha=alpha);
      } # end fill is numeric case
    } # end case  checks 
  if(is.null(theme)) theme <- theme(axis.text.x=element_text(angle=45,hjust=1));
  out + labs(title = title, y = ylab, x = xlab) + theme;
}


#'fpSummary, a fool-proof summary in the sense that it always returns a count of NA's even if there are none.
fpSummary <- function(xx){
  out <- summary(xx);
  if(!"NA's"%in%names(out)) out["NA's"]<-0;
  out;
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

reOrderYesNo<- function(col, midAnswers= c("maybe","unsure","maybe_contact"), dontKnowAnswers = c("i do not know", "do not know"), nonAnswers = c("none", "prefernotanswer","prefer_not_answer"), blank = c("", "0"), yes = c("yes", "y", "True"), no = c("no","n","False")){
  #This method takes a factor column
  #If the column is not a factor or if the column does not contain a convertable factor it 
  #returns the column unchanged. 
  #If the column contains a changable factor it will return the reordered factors of the column. 
  #orders as: blank, nonAnswer, 
  
  #If you want to test this, here's an example with obd as the original and obdCop as the copy
  #obdCop = obd
  #for(ii in 1:75){ obdCop[,ii] = reOrderYesNo(obdCop[,ii])}
  #Test: for(ii in names(obdCop)) {if(!identical(obdCop[[ii]],obd[[ii]])) print(table(obdCop[[ii]],obd[[ii]]))}
  
  ##obd$income <- factor(obd$income, levels(obd$income)[c(8,2,4,3,7,5,6,1)])
  #uniqueness check 
  un = c(yes, no, midAnswers, dontKnowAnswers, nonAnswers, blank)
  if(length(un) != length(unique(un))) stop("Error! yes, no, midAnswers, dontKnowAnswers, nonAnswers, and blank must be unique! Overlapping values are not allowed.")
  
  #Check for not factor
  if(class(col) != "factor") return (col)
  
  #obtain the levels, if there are more than 5 levels or fewer than 3 levels return col. 
  lev <- levels(col)
  if(length(lev) > 6 || length(lev) < 2)return(col)
  len = 2
  #going to start building the dynamic call here so we aren't calling as many if statements
  
  indexes = vector()
  #blank
  blankIndex = which(!is.na(match(tolower(lev), tolower(blank))))
  if(length(blankIndex) == 1){
    indexes = c(indexes, blankIndex)
    len = len + 1
  }
  #Non Answer
  nonAnswerIndex = which(!is.na(match(tolower(lev), tolower(nonAnswers))))
  if(length(nonAnswerIndex) == 1){
    indexes = c(indexes, nonAnswerIndex)
    len = len + 1
  } 
  dontKnowIndex = which(!is.na(match(tolower(lev), tolower(dontKnowAnswers))))
  if(length(dontKnowIndex) == 1) {
    indexes = c(indexes, dontKnowIndex)
    len = len + 1
  }
  #requires a no answer
  noIndex = which(!is.na(match(tolower(lev), tolower(no))))
  if(length(noIndex) != 1) {
    return(col)
  }else{
    indexes = c(indexes, noIndex)
  }
  #get mid/maybe not yet sure answer
  midIndex = which(!is.na(match(tolower(lev), tolower(midAnswers))))
  if(length(midIndex) == 1) {
    indexes = c(indexes, midIndex)
    len = len + 1
  }
  #requires a yes answer
  yesIndex = which(!is.na(match(tolower(lev), tolower(yes))))
  if(length(yesIndex) != 1){
    return(col)
  }else{
    indexes = c(indexes, yesIndex)
  }
  #We have levels that are not captured. For now giving a warning.
  if(len != length(lev)) {
    #warning(paste("Only", len, "of", length(lev), "levels matched passed values. Returning original column."))
    return(col)
  }
  #create the dynamic change statement
  col = factor(col, levels(col)[indexes])
  return(col)
}

  
