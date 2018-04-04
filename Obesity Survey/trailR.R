#' ---
#' title: "R functions for having an audit trail"
#' author: "Wilson, Bokov"
#' date: "03/21/2018"
#' ---
#' 
#' This is going to get called via getOption('trail',tinit())
#' The fields of trail are:
#' time = timestamp
#' type = type of information (file, rdata, info, gitstamp, seed, etc)
#' name = the name of that specific item, should be unique
#' value = Almost anything. Non atomic values should be assigned for example like this: 
#'   `trail[nrow(trail),'value'] <- list(list(mtcars))`
#'   For items of type 'file' the path will be stored here, for 'seed', the random seed, for 'gitstamp'
#'   the current branch.
#' hash = For 'file' and 'rdata' the md5 checksum of the file, for 'gitstamp' the commit hash
#' trail = For 'rdata', if a trail object exists, dump it into this field. And it can have its own trail objects
#'   so we have an arbitrarily long "family tree" of ancestor objects!
#' 
#' TODO: function to write out .trail as a JSON or XML file (to accompany 
#'       non-rdata saveouts)
#' TODO: modify tread() to check for the existance of a trail flat-file as above
#'       and read it if there is one. Then, treat it the same way
#'       that a .trail from a loaded rdata file.
#' TODO: modify tread() to see if the file being read is part of the repo and 
#'       document that fact, perhaps by the type being 'versioned-file' instead
#'       of 'file'
#' TODO: a twrite() function that is the inverse of tread() and creates an 
#'       accompanying flat-file of trail.
#' TODO: [priority] function to recursively print a .trail (to screen or to 
#'       pander)
#' TODO: wrap the repeating pattern of whichrecord, trail[whichrecord,], etc.
#'       into a function.
#' TODO: have gitstamp only do the non-production message if the non-production
#'       argument is TRUE _and_ the file isn't checked in
#' TODO: if tscript finds that the specified script is not part of the git repo
#'       then record the type as 'unversioned-script' rather than 'script'
#'       and save an md5 hash instead of a git-provided hash
#' TODO: [priority] possibly rename tscript() to tself() and have a separate 
#'       tsource() function specifically to wrap source
#' TODO: [priority] check if a MYSTERY_FILE with a matching hash already exists 
#'       and if it does, reuse the name
currentscript <- parent.frame(2)$ofile;
if(is.null(currentscript)) currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';

#' Return a commit hash (for inclusion in reports for example) after first making
#' sure all changes are committed and pushed
#' TODO: instead of auto-committing, error if uncommited changes, needs to be 
#' a deliberate process, otherwise we have tons of meaningless auto-commit
#' messages that will make future maintenance harder
gitstamp <- function(production=T,branch=T) {
  br<- if(branch) system("git rev-parse --abbrev-ref HEAD",intern=T) else NULL;
  if(production){
    if(length(gitdiff<-system("git update-index --refresh && git diff-index HEAD --",intern = T))!=0) stop(sprintf(
      "\ngit message: %s\n\nYou have uncommitted changes. Please do 'git commit' and then try again."
      ,gitdiff));
    c(br,system("git push && git log --pretty=format:'%h' -n 1",intern=T));
  } else return(c(br,'TEST_OUTPUT_DO_NOT_USE'));
}

# this creates a trail object if there isn't already one and either way 
# returns a trail object
tinit <- function(trail=getOption('trail'),...){
  if(is.null(trail)) {
    trail<-data.frame(time=Sys.time(),type='info',name='sessionInfo',value=NA,hash=NA,trail=NA,stringsAsFactors=F); 
    trail$value <- list(sessionInfo());
    options(trail=trail);
  }
  return(trail);
}

# script registering itself... adds a gitstamp and its own name to trail
tself <- function(scriptname=parent.frame(2)$ofile
                    ,trail=getOption('trail',tinit()),production=T){
  if(is.null(scriptname)) scriptname <- 'INTERACTIVE_SESSION';
  whichrecord <- nrow(trail)+1;
  trail[whichrecord,] <- c(time=NA,type='this_script',name=scriptname
                           ,gitstamp(production=production,branch=T),trail=NA);
  trail[whichrecord,'time'] <- Sys.time();
  options(trail=trail);
}

# setting and recording the random seed
tseed <- function(seed,...,trail=getOption('trail',tinit())){
  whichrecord <- nrow(trail)+1;
  seedname <- deparse(match.call()$seed);
  trail[whichrecord,] <- c(time=NA,type='seed',name=seedname,value=NA,hash=NA,trail=NA);
  trail[whichrecord,'value'] <- list(list(match.call()));
  trail[whichrecord,'time'] <- Sys.time();
  options(trail=trail);
  set.seed(seed,...);
}

tload <- function(file,envir=parent.frame()
                  ,verbose=FALSE,trail=getOption('trail',tinit()),trailobj='.trail'){
  if(trailobj %in% ls(envir,all=T)) stop(sprintf('
The object %s already exists, perhaps due to one of the trail-related functions crashing. Please try again in clean environment.'
                                         ,trailobj));
  #if(is.name(filename<-match.call()$file)) filename <- as.character(filename) else {
  #  filename <- sprintf('MYSTERY_FILE%02d',length(grep('^MYSTERY_FILE',trail[,'name']))+1);
  #}
  filename <- deparse(match.call()$file);
  filehash <- tools::md5sum(file);
  load(file,envir,verbose);
  whichrecord <- nrow(trail)+1;
  trail[whichrecord,] <- c(time=NA,type='rdata',name=filename,value=file,hash=filehash,trail=NA);
  trail[whichrecord,'time']<-Sys.time();
  if(trailobj %in% ls(envir,all=T)){
    trail[whichrecord,'trail'] <- list(list(envir[[trailobj]]));
    rm(list=trailobj,envir=envir);
  } else trail[whichrecord,'trail'] <- 'NO_TRAIL_FOUND';
  options(trail=trail);
}

tread <- function(file,readfun,...,trail=getOption('trail',tinit())){
  #if(is.name(filename<-match.call()$file)) filename <- as.character(filename) else {
  #  filename <- sprintf('MYSTERY_FILE%02d',length(grep('^MYSTERY_FILE',trail[,'name']))+1);
  #}
  filename <- deparse(match.call()$file);
  #readfunname <- as.character(sys.call()$readfun);
  filehash <- tools::md5sum(file);
  loaded <- readfun(file,...);
  whichrecord <- nrow(trail)+1;
  trail[whichrecord,]<-c(time=NA,type='file',name=filename,value=file,hash=filehash,trail=NA);
  trail[whichrecord,'time'] <- Sys.time();
  options(trail=trail);
  return(loaded);
}

tsave <- function(...,list=character(),envir=parent.frame()
                  ,trail=getOption('trail',tinit()),trailobj='.trail'){
  # add another sessionInfo() entry to trail
  whichrecord <- nrow(trail)+1;
  trail[whichrecord,]<-c(time=NA,type='info',name='sessionInfo',value=NA,hash=NA,trail=NA);
  trail[whichrecord,'time'] <- Sys.time();
  trail[whichrecord,'value'] <- list(sessionInfo());
  whichrecord <- whichrecord+1;
  trail[whichrecord,] <- c(time=NA,type='save',name='save',value=NA,hash=NA,trail=NA);
  trail[whichrecord,'time'] <- Sys.time();
  trail[whichrecord,'value'] <- list(list(match.call()));
  # put trail object in the environment
  envir[[trailobj]] <- trail;
  # save with the args as given
  save(...,list=c(trailobj,list),envir=envir);
  options(trail=trail);
  # remove the trailobj
  rm(list = trailobj,envir = envir);
}

#' A script for testing tscript();
#
# source('./trailR.R');
# currentscript <- parent.frame(2)$ofile;
# if(is.null(currentscript)) currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
# tscript(scriptname=currentscript);
# .trail <- getOption('trail');
# save(mtcars,.trail,file='junktestsave.rdata');
