guessnum <- function(xx,exclude='',returnval=F,tolerance=.11){
  out <- (sum(is.na(as.numeric(as.character(xx))))- sum(is.na(xx)|!xx%in%exclude))/length(xx)
  out <-c(val=out,result = out<=tolerance);
  if(returnval) out else out[2];
}

vs <- function(xx
               ,type=c('numeric','factor','logical','character','binary','multinomial','time','date','dt')
               ,ignorevs='ignorevs',...){
  # This function takes a data.frame and returns the names of columns of a 
  # particular type
  # xx        : data.frame
  # type      : string indicating what type of column names to return (the 
  #             non-standard ones are explained below)
  # ignorevs  : the name of the option setting where you store an optional 
  #             vector of column names that should never be returned (because 
  #             e.g. you know ahead of time that they are not useful for
  #             plotting or analysis)
  
  # first we define a TRUE/FALSE test function for selecting the columns
  # instead of a lot of if/else statements, we just have one switch() 
  # statement that is more readable. For more information, type ?switch
  # The first argument is match.arg, which matches the possibly partial
  # string the user typed to full argument. Think of tye `type` argument
  # to the vs() function as a multiple-choice question.
  test <- switch(match.arg(type),
                 'numeric'=is.numeric,
                 'factor'=is.factor,
                 'logical'=is.logical, # i.e. all non-missing values in this column are TRUE/FALSE
                 'character'=is.character,
                 'binary'=function(zz) length(unique(zz))==2,
                 'multinomial'=function(zz) length(unique(zz))<length(zz),
                 'time'=function(zz) inherits(zz,'POSIXt'),
                 'date'=function(zz) inherits(zz,'Date'),
                 'dt'=function(zz) inherits(zz,c('Date','POSIXt')) # i.e. either date OR time
  );
  # Then we apply the test function appropriate to the data type of to each 
  # column of xx using the `sapply()` function. What it returns, `matches` is a
  # vector of TRUE/FALSE (logical) values, with each having the same name as 
  # the column in xx that it refers to. If that column is the type being sought
  # it will have a value of TRUE, otherwise a value of FALSE.
  matches <- sapply(xx,test);
  # we return our final answer...
  return(
    setdiff( # the difference between
      # ...the names from `matches` where the corresponding value is `TRUE`
      names(matches)[matches]
      # ...and an optional environment variable that can be a vector of names
      # to ignore if they exist. To set this, you can do, e.g.
      # `option(ignorevs=c('patient_num','birth_date'))`
      ,getOption(ignorevs)));
}


binfactor<-function(xx,levs,other='other',dec=T){
  # This function takes a factor `xx` and returns it re-binned and optionally 
  # sorted by size.
  # xx    : a factor
  # levs  : Either an integer of length 1 or a vector of level names to keep.
  #         The other levels will get binned together into a single level 
  #         called...
  # other : The name of the level to which all the small levels get collapsed.
  #         `other` by default.
  # dec   : If TRUE, the levels are also re-ordered so that they are in 
  #         decreasing order of how many observations are part of each level.
  #         If FALSE, then likewise but in increasing order. Finally, NA 
  #         disables re-ordering of levels.
  if(missing(levs)) levs <- names(which.max(summary(xx))) else {
    # If levs argument not specified, just keep the most populated level and
    # bin everything else together.
    if(length(levs)==1&&!is.na(as.integer(levs))) {
      levs <- names(sort(summary(xx),dec=T)[1:levs]);
    }
    # Otherwise, if a single integer was given, take that many of the most 
    # populated levels.
  }
  # Capture the current levels, and then, if they are not already part of the
  # levels you want to keep, assign to them the default `other` name.
  newlevs <-levels(xx); newlevs[!newlevs%in%levs]<-other;
  # newlevs is a vector of character values. Now we assign it to existing 
  # levels, thus overwriting them.
  levels(xx)<-newlevs;
  # If `dec` is not set to NA we rebuild the factor with a new ordering of the
  # levels, by size.
  if(is.na(dec)) xx else factor(xx,levels=names(sort(summary(xx),dec=dec)))
}
