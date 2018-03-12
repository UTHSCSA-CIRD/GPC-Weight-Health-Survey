survAUC<-function(Surv.rsp,Surv.rsp.new,lp,lpnew,times,time,lp0=0
                  ,nonstdSurv=c('OXS','Nagelk','XO')
                  ,...,FUNS){
  if(is.null(info<-getOption('survAUCinfo'))){
    # create a lookup table for these functions if one doesn't exist
    info<-sapply(ls(package:survAUC),get) %>% lapply(formals) %>%
      lapply(sapply,class) %>% lapply(bind_rows) %>% bind_rows(.id='fun');
    options(survAUCinfo=info);
  }
  if(!missing(FUNS)) info <- subset(info,fun %in% FUNS);
  args <- list();
  for(ii in names(match.call())[-1]) args[[ii]] <- get(ii);
  #for(ii in names(args)) if(is.language(environment()))
  # get the arguments that are capable of being missing
  # screw the formargs stuff
  #formargs <- sapply(realformals<-formals(sys.function()),is.name);
  #for(ii in names(formargs[!formargs])) args[[ii]] <- realformals[[ii]];
  #formargs<-setdiff(names(formargs[formargs]),c('FUNS','...'));
  # create a value for 'time' if it's missing but there are 'times'
  if(!'time' %in% names(args) && 'times' %in% names(args)) args$time <- max(eval(args$times));
  # make sure lp0 is the right length (seriously people? You're going to make me
  # do this in R of all languages?). BTW, Surv.rsp.new not an error-- for all
  # functions requiring lp0, there is also that departure from standard names
  # and for those three Surv.rsp.new becomes Surv.rsp
  #browser();
  if(missing(lp0)&&!missing(Surv.rsp.new)) args$lp0 <- rep_len(0,nrow(args$Surv.rsp.new));
  #if(args$lp0==1 && args$lp0==0) args$lp0 <- rep_len(0,nrow(args$Surv.rsp.new));
  # which arguments are we missing on our current invokation?
  #missargs <- setdiff(formargs,names(args));
  if(!'lp'%in%names(args)) browser();
  # find the arguments that each function can accept
  allowedargs<-setNames(apply(info[,-1],1,function(xx) names(info)[-1][!is.na(xx)])
                        ,unlist(info[,1]));
  # find the names of the non-optional arguments for each function
  nonoptargs<- sapply(info$fun,function(ii) subset(info,fun==ii),simplify=F) %>% 
    lapply(function(jj) na.omit(names(jj)[jj=='name']));
  # based on the above, here are the functions we can run with the data we have
  # for the ones that are FALSE we will return NAs
  canrun <- lapply(nonoptargs,setdiff,names(args)) %>% sapply(length) == 0;
  # and for each of the rest we will do.call along with an alist of compatible 
  # arguments. Lets first unpack that list.
  # for(ii in intersect(names(args),ls())) 
  #   args[[ii]] <- if(is.language(environment()[[ii]]))
  #     eval(args[[ii]]) else get(ii);
  # and filter it down to just the ones that are valid arguments for functions 
  # that can be run
  args2use <- sapply(names(canrun)
                     ,function(xx) if(canrun[xx])
                       args[intersect(allowedargs[[xx]],names(args))] else NA
                     ,simplify=F);
  # correct the inconsistent argument names in the functions names by the 
  # nonstdSurv variable
  for(ii in intersect(nonstdSurv,names(canrun[canrun]))) {
    args2use[[ii]][c('Surv.rsp','lp')] <- args[c('Surv.rsp.new','lpnew')];
  }
  # return output!
  invisible(sapply(names(args2use),function(xx) if(is.na(args2use[[xx]])) NA else {
    try(do.call(xx,args2use[[xx]]))}));
}



#' into the specified existing or new level. That level is listed last
#' in the resulting factor.
#' @param xx A \code{vector} (required).
#' @param topn \code{numeric} for the top how many levels to keep (optional, default =4)
#' @param binto \code{character} which new or existing level to dump the other values in
cl_bintail <- function(xx,topn=4,binto='other'){
  if(!is.factor(xx)) xx <- factor(xx);
  counts <- sort(table(xx),decreasing = T);
  if(is.numeric(binto)) binto <- names(counts)[binto];
  keep <- setdiff(names(counts)[1:min(length(counts),topn)],binto);
  droplevels(
    factor(
      ifelse(
        xx %in% keep, as.character(xx), binto
        ),levels=c(keep,binto)));
}

#' simple little percent expression
pct <- function(xx,digits=2,...) sprintf(paste0('%.',digits,'f%%'),as.numeric(xx)*100);

#' A sketch for a possible future function that converts stargazer tables into
#' a universal markdown pipe format
starkable <- function(xx,firstrowisdata=T,row.names=F,taildrop=1
                      ,hrow=2
                      ,sgoutput=F,kboutput=T,searchrep=matrix(c('V1',''),ncol=2)
                      ,...){
  output <- if(!sgoutput) capture.output else identity;
  formals(htmltab)$header=hrow;
  output(out <- stargazer(xx,type = 'html')  %>%
           htmltab %>% `[`(firstrowisdata,,drop=F) %>%
           kable(format = 'markdown',row.names=row.names,...) %>%
           head(length(.)-taildrop) %>% paste0('\n') %>%
           submulti(searchrep = searchrep));
  if(kboutput) cat(out,'\n');
  invisible(out);
}

#' Take a character vector and perform multiple search-replace 
#' operations on it.
#' @param xx A \code{vector} of type \code{character} (required)
#' @param searchrep A \code{matrix} with two columns of type \code{character} (required). The left column is the pattern and the right, the replacement.
#' @param method One of 'partial','full', or 'exact'. Controls whether to replace only the matching regexp, replace the entire value that contains a matching regexp, or replace the entire value if it's an exact match.
submulti <- function(xx,searchrep,method=c('partial','full','exact'),...){
  # if no method is specified by the user, this makes it take the first value
  # if a method is only partially written out, this completes it, and if the
  # method doesn't match any of the valid possibilities this gives an informativ
  # error message
  method<-match.arg(method);
  # rr is a sequence of integers spanning the rows of the searchrep matrix
  rr <- 1:nrow(searchrep);
  # oo will hold the result that this function will return
  oo <- xx;
  switch(method
         ,partial = {for(ii in rr)
           oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo,...)}
         ,full =    {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo,...)]<-searchrep[ii,2]}
         ,exact = {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo,fixed=T,...)]<-searchrep[ii,2]}
           #oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo,fixed = T)}
         );
  oo;
}

#' Take a data.frame and create a PL/SQL table definition for it, ready to paste into DBVis and such
#' @param xx A \code{data.frame} (required)
sql_create_table <- function(xx,tname,sqltemplate='CREATE TABLE %s (%s);'){
  mysearchrep <- cbind(c('integer','character','numeric','Date','logical','factor')
    ,c('NUMBER(32)','VARCHAR2(nn)','NUMBER(32)','DATE','NUMBER(1)','VARCHAR2(nn)'));
  if(missing(tname)) tname <- as.character(substitute(xx));
  thenames <- sapply(xx, class);
  thenames <- submulti(thenames, mysearchrep, 'exact');
  strlengthres<-apply(nsqip,2,function(xx){max(str_length(xx))});
  thenames <- apply(cbind(round(strlengthres*1.5), thenames),1,function(xx){gsub('nn',xx[1],xx[2])});
  thenames <- paste(' ',names(thenames), as.character(thenames), sep=' ', collapse= ',\n');
  thenames <- gsub('.', '_', thenames, fixed=TRUE);
  thenames <- gsub('_{1,}','_',thenames);
  thenames <- gsub('_ ',' ',thenames);
  thenames <- sprintf(sqltemplate, tname, thenames);
  cat(thenames);
  invisible(thenames);
  # TODO: collect the column names and classes as we did in console
  # TODO: use the above and submulti() to convert R classes to SQL data types
  # TODO: paste() and/or sprintf() to create the string that will be the output
}

#' Take a data.frame or character vector and a vector of grep targets and return
#' the values that match (for data.frame, column names that match). If no 
#' patterns given just returns the names
#' @param xx A \code{data.frame} or character vector (required)
#' @param patterns A character vector of regexp targets to be OR-ed
grepor <- function(xx,patterns='.') {
  if(is.list(xx)) xx <-names(xx);
  grep(paste0(patterns,collapse='|'),xx,val=T);
}

#' Take an object name \code{obj}, check to see if it  exists in environment \code{env}
#' and if it does not, run \code{expression} \code{EXPR} and assign its result to \code{obj}
#'
#' @param obj   A \code{character} string (required) naming the variable in env
#'   to which the results of running \code{EXPR} will be assigned.
#' @param EXPR  An \code{expression} to execute and assign to the object named
#'   by \code{obj} if it doesn't already exist.
#' @param env   An \code{environment} to check for the existence of \code{obj}
#'   and where to create it if it doesn't exist.
#'
#' @return Does not return anything.
#'
#' @examples `checkrun('dat3',{group_by(dat1,idn_mrn) %>% summarise_all(first)});`
checkrun <- function(obj,EXPR,env=as.environment(-1)){
  env<-env;
  if(length(ls(env,all=T,pattern=obj))==0){
    browser();
  }
}


#' Delete all the junk in your environment, for testing
clearenv <- function(env=.GlobalEnv) rm(list=setdiff(ls(all=T,envir=env),'clearenv'),envir=env);

#' Plots in the style we've been doing (continuous y, discrete x and optionally z)
#' 
#' Instead of creating new tables for 'All', just set xx=T
#' 
#' To suppress plotting of a legend (but still use fill) set fill.name=NA
#' Likewise, to suppress printing y-axis labels make yy.name=NA
#' To merely omit printing a name, set the name in question to ''
autoboxplot <- function(pdata, xx, yy, zz, subset=T
                        , type=c('box','violin')
                        , title=sprintf('%s vs %s\n by %s',xx,yy,zz)
                        , xx.name=if(xx==TRUE) 'All' else xx, xx.breaks=if(xx==TRUE) xx else unique(pdata[[xx]])
                        , xx.labels=if(xx==TRUE) '' else xx.breaks
                        , yy.name=yy, yy.labels
                        , fill.name, fill.breaks, fill.labels
                        , counts=T
                        ,...){
  subset <- substitute(subset);
  plot_type <- switch(match.arg(type)
                      ,box=geom_boxplot(coef=100)
                      ,violin=geom_violin());
  pdata <- subset(pdata,subset=eval(subset));
  if(!missing(zz)){
    if(missing(fill.name)) fill.name <- zz;
    if(missing(fill.breaks)) fill.breaks <- unique(pdata[[zz]]);
    if(missing(fill.labels)) fill.labels <- fill.breaks;
    fill <- if(is.atomic(fill.name)&&is.na(fill.name)) scale_fill_discrete(guide=F) else {
      scale_fill_discrete(name=fill.name,breaks=fill.breaks,labels=fill.labels);
    }
  }
  if(missing(yy.labels)){
    yy.labels <- if(is.na(yy.name)) scale_y_continuous(name='',labels=NULL) else {
      scale_y_continuous(name=yy.name,labels = comma);
      };
    };
  out <- ggplot(data = pdata, aes_string(x = xx, y = yy)) + plot_type + yy.labels +
    scale_x_discrete(name=xx.name,breaks=xx.breaks,labels=xx.labels) +
    labs(title=title)
  if(!missing(zz)) out <- out + aes_string(fill=zz) + fill;
  if(counts){
    ccrds<-ggplot_build(out)$layout$panel_ranges[[1]];
    ann.label <- if(xx==TRUE && missing(zz)) nrow(pdata) else if(xx==TRUE){
      paste0(table(pdata[,zz]),collapse=' \t ') } else if(missing(zz)){
        paste0(table(pdata[,xx]),collapse=' \t ') } else {
          apply(table(pdata[,c(xx,zz)]),1,function(ii) paste0(ii[!ii%in%list(0,'')],collapse=' \t '));
        }
    #ann.label <- gsub('\\b0\\b','',ann.label);
    out <- out + annotate('text',x=ccrds$x.major_source,y=ccrds$y.range[1]
                          ,label=ann.label[ccrds$x.major_source]);
  }
  attr(out,'call') <- match.call();
  out;
}

getCall.data.frame <- getCall.gg <- function(xx) attr(xx,'call');


#' From ... http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  # Make the panel
  # ncol: Number of columns of plots
  # nrow: Number of rows needed, calculated from # of cols
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots==1) print(plots[[1]]) else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} 

mktabsie <- function(data,subsets=list(Full=T),pw
                     ,vars
                     ,filepath='.'
                     ,filename='survSave.rdata'
                     ,serverTitle='TABSIE'
                     ,serverStatement=bquote(h4("Welcome to TABSIE"))){
  serverData <- sapply(subsets,function(ii) subset(data[,vars],eval(ii)),simplify=F);
  serverDataDic <- names(serverData);
  serverHash <- digest::digest(pw,algo='sha512',ascii=TRUE);
  save(serverStatement,serverData,serverDataDic,serverTitle,serverHash
       ,file=paste0(filepath,'/',filename));
}

#' Returns a vector of column names that contain data elements of a particular type
#' as specified by the user: "integer","POSIXct" "POSIXt", "numeric", "character", 
#' "factor" and "logical". 
vartype <- function(dat, ctype) {
  xx <- unlist(sapply(dat, class));
  idx <- which(xx %in% ctype);
  res <- names(xx)[idx];
  return(res)
}

#' ## Functions for RMarkdown and ggplot2
#' 
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

#' This function can be called from `stat_summary()` as the
#' `fun.data=` argument. It will cause group counts to be 
#' over-printed on a `geom_boxplot()` (probably other similar
#' plots too) if `stat_summary()` is added to it.
n_fun <- function(xx) data.frame(y=mean(quantile(xx,c(.5,.75))),label=as.character(length(xx)));

#' take a list of subset criteria and return a list of data.frames
ssply<-function(dat,...) sapply(sys.call()[-(1:2)],function(ii) subset(dat,eval(ii)),simplify=F);

#' save a named list of tables, including names
savetablelist <- function(lst,fileprefix,filesuffix=paste0(format(Sys.Date(),'%m-%d-%Y-%H_%M_%S'),'.tsv')
                            ,filepath='./',outfile=paste0(filepath,fileprefix,filesuffix)
                          ,sep='\t',row.names=F
                          ,singletabname=as.character(substitute(lst))
                          ,replaceifexists=T,...) {
  if(is.data.frame(lst)) lst<-setNames(list(lst),singletabname);
  if(replaceifexists&&file.exists(outfile)) file.remove(outfile);
  for(ii in names(lst)){
  write(ii,file=outfile,append=T);
  write.table(lst[[ii]],outfile,sep=sep,row.names=row.names,append=T);
  }
}

#' Usage: `xx<-mapnames(xx,lookup)` where lookup is a named character vector
#' the names of the elements in the character vector are what you are renaming
#' things TO and the values are what needs to be matched, i.e. what renaming things
#' FROM. If you set namesonly=T then it just returns the names, not the original
#' object.
mapnames<-function(xx,lookup,namesonly=F,...){
  xnames <- names(xx);
  idxmatch <- na.omit(match(xnames,lookup));
  newnames <- names(lookup)[idxmatch];
  if(namesonly) return(newnames);
  names(xx)[xnames %in% lookup] <- newnames;
  xx;
}

#' Forget modifying the columns to TRUE/FALSE... some of them have 'Unknown',
#' NA, who knows what else. We can just do it dynamically when we need to. 
#' This might even let us get rid of a couple of analytic columns.

#' Example of using R methods dispatch
#' 
#' The actual usage is: `truthy(foo)` and `truthy()` itself figures
#' out which method to actually dispatch.
truthy <- function(xx,...) UseMethod('truthy');
truthy.logical <- function(xx,...) xx;
truthy.factor <- function(xx,...) truthy.default(as.character(xx),...);
truthy.numeric <- function(xx,...) xx>0;
truthy.default <- function(xx,truewords=c('TRUE','true','Yes','T','Y','yes','y')
                           ,...) xx %in% truewords;
truthy.data.frame <- function(xx,...) as.data.frame(lapply(xx,truthy,...));

countfrac <- function(xx,outcomes
                      # set to TOTAL in order to do _only_ the total
                      ,groupcols='rai_range',sortby=groupcols
                      # set to 'none' if don't want to sort
                      ,dir=c('desc','asc','none')
                      ,summfns=c(n=sum,frac=mean)
                      # set to '' to disable
                      ,totalrow='Total'){
  # callback, so this function can call itself even if it's renamed
  # or nameless, for purposes of rerunning to get the totals row
  if(totalrow!='') thisfn <- sys.function();
  oo <- xx;
  # is the function being invoked for the purpose of calculating a total row?
  if(doingtotalrow <- tolower(trimws(groupcols[1]))=='total'){
    oo[[groupcols]] <- totalrow;
  };
  oo <- group_by_(oo,groupcols);
  # we coerce everything to logical, whatever it might have been
  # but leave alone columns other than those specified in the 'outcomes' variable
  oo[,outcomes] <- mutate_all(oo[,outcomes],truthy);
  # Two different summary tables, the second one applies the same set of
  # functions to everything, and the first one is just a count that doesn't 
  # rely on any one specific column. So we do them separately and then cbind()
  oo <- cbind(summarise(oo,bin_n=n()),summarise_all(oo[,c(groupcols,outcomes)],summfns)[,-1]) %>%
    # creating cumul_count, as in the current code
    mutate(cumul_count=rev(cumsum(rev(bin_n))));
  # sort, if desired, just as in the current code
  if(!doingtotalrow) oo <- switch(match.arg(dir)
                                  ,none=oo
                                  ,desc=arrange_(oo,sprintf('desc(%s)',groupcols))
                                  ,asc=arrange_(oo,groupcols));
  # now we insure that the column order is the same as the order of the groupcols
  # argument-- first the grouping column name, then the 'bin_n' (used to be called
  # rai_n, but this isn't specific to RAI, could group by anithing, so renamed)
  # cumul_count...
  oo <- oo[,c(groupcols,'bin_n','cumul_count'
              # ...and then this: it pastes the suffixes as obtained from the names 
              # of the summfns argument but orders them in the same way as they were
              # given, rather than first the first summary function and then the second
              ,c(t(outer(outcomes,names(summfns),paste,sep='_'))))];
  if(!doingtotalrow&&totalrow!='') { 
    tot <- thisfn(xx,outcomes,groupcols=totalrow,summfns=summfns,totalrow='');
    names(tot)<-names(oo);
    tot[[groupcols]] <- totalrow;
    oo <- rbind(oo,tot);
  }
  attr(oo,'call') <- match.call();
  oo;
}

#' Stack a vector to form a matrix with repeating rows, with optional 
#' column names and transformation
#'
#' @param  vv    A vector which will become the row of a matrix
#' @param  nr    Number of (identical) rows this matrix will contain
#' @param  trans An optional function that can take a matrix as its 
#'              sole argument. Useful functions include `as.data.frame()`
#'              `as_tibble()` and `as.table()`
#' @return A matrix, unless the function specified in the `trans` argument
#'         causes the output to be something else.
#' @export 
#'
#' @examples 
#' vec2m(1:10,5);
#' vec2m(1:10,5,tr=data.frame);
#' vec2m(setNames(1:12,month.name),3);
vec2m <- function(vv,nr=1,trans=identity) {
  return(trans(matrix(as.matrix(c(vv)),nrow=nr,ncol=length(vv),byrow=T
                ,dimnames=list(NULL,names(vv)))));
}

#' ### Specific to RAI-A
#' 

#' Return a tableone object formatted just the way we like it
#' @param xx     A \code{data.frame} (required).
#' @param vars   Vector of variable names to pass to \code{CreatTableOne} (optional)
#' @param ...    Named expressions from which to create the strata variable
#'               for \code{CreatTableOne} (only tested for one variable)
stratatable <- function(xx,vars=NULL,...){
  nmx <- names(xx);
  xx <- transform(xx,...);
  mystrata <- setdiff(names(xx),nmx);
  res <- tableone::CreateTableOne(data=xx, vars= vars, strata=mystrata) %>% print;
  # res[,'p'] %>%  gsub("<","", x=.) %>% trimws() %>%
  #   as.numeric() %>% p.adjust() %>% cbind(res,padj=.) %>%
  #   data.frame %>% dplyr::select(-test) -> res;
  res[,'p'] %>%  gsub("<","", x=.) %>% trimws() %>%
    as.numeric() %>% p.adjust() %>% cbind(res[,1:2],padj=.) -> res;
  return(res);
}

#' Either create a basic data dictionary object from a data frame
#' or append to an existing one if append.to is set to an existing
#' data dictionary. 
#' TODO: modularize the naming of dd columns, so we're not stuck typing
#'       dataset_column_names all the time
#' TODO: parsing of ... to get additional metadata/c_ fields.
makeddict <- function(data,...,delmissing=T,append.to){
  out <- data.frame(dataset_column_names=names(data)
                    ,name=submulti(names(data)
                                   # replace underscores and periods with spaces
                                   # and then capitalize first letter of each word
                                   ,cbind(c('_|\\.','\\b(.)'),c(' ','\\U\\1'))
                                   ,perl=T)
                    ,class=sapply(data,function(xx) class(xx)[1])
                    ,unique=sapply(data,function(xx) length(na.omit(unique(xx))))
                    ,missing=sapply(data,function(xx) sum(is.na(xx)))
                    ,stringsAsFactors = F);
  if(!missing(append.to)){
    if(delmissing){ # delete columns in old dictionary if they no longer exist
      delcols <- setdiff(append.to$dataset_column_names,out$dataset_column_names);
      append.to <- subset(append.to,!dataset_column_names %in% delcols);
    }
    newcols <- setdiff(out$dataset_column_names,append.to$dataset_column_names);
    if(length(newcols)>0) { # if there are new columns, append them
      out <- subset(out,dataset_column_names %in% newcols);
      out <- bind_rows(append.to,out);
    } else if(!identical(colnames(out),colnames(append.to))){
        out <- bind_rows(append.to[F,],out);
        }
  }
  rownames(out) <- out$dataset_column_names;
  out;
}


#' Returns a list of column names from the data dictionary for which the column
#' named in the first argument is true. The first arg can be either a string or 
#' a name. The second must be a data.frame
#'
#' @param var        Either a string or a name, of a column in `dictionary`
#' @param dat        An optional data.frame, to constrain which rows of the 
#'                   'dictionary' object get used
#' @param matchcol   Optional column that maps the rows of 'dictionary' to the rows
#'                   of a 'data.frame' of interest
#' @param retcol     Which column to return-- by default the same as used for 'matchcol'
#' @param dictionary A 'data.frame' that is used as a data dictionary. It must at 
#'                   minimum contain a column of column-names for the dataset for
#'                   which it is a data dictionary ('matchcol') and one or more 
#'                   columns each representing a _group_ of columns in the dataset, 
#'                   such that a TRUE or T value means the column whose name is 
#'                   the value of 'matchcol' is the name of a column in the data
#'                   that belongs to the group defined by the grouping column.
#'                   These grouping columns are what the argument 'var' is
#'                   supposed to refer to. We will use the convention that grouping
#'                   column names begin with 'c_' but this convention is not 
#'                   (currently) enforced programmatically.
v <- function(var,dat
              ,matchcol='dataset_column_names'
              ,retcol=matchcol
              ,dictionary=dct0) {
  # convenience function: if forgot what column names are available, call with
  # no arguments and they will be listed
  if(missing(var)) return(names(dictionary));
  # support both standard or non-standard evaluation
  var<-as.character(substitute(var));
  # if a 'dat' argument is given, restrict the output so that only results having
  # having values found in the colnames of 'dat' are returned.
  if(!missing(dat)) dictionary <- dictionary[dictionary[[matchcol]]%in%colnames(dat),];
  # TODO: Think about what to do when nothing matches... not necessarily an error
  #       condition, might just be something to warn about and move on.
  out<-unlist(dictionary[dictionary[[var]],retcol],use.names=F);
  # if something other than matchcol is returned, give it a name to make it 
  # easier to align with column names in the data
  if(retcol != matchcol){
    out<-setNames(out,dictionary[dictionary[[var]],matchcol][[1]]);
  }
  # 'na.omit()' needed because we allows the 'dictionary' object to have NAs instead
  # of FALSEs. 'c()' needed to strip na.omit metadata, so the output is a plain
  # old vector
  return(c(na.omit(out)));
  }



#This function will count the number of times a patient
#is readmitted to the hospital within a given time window
#provided by the user (default will be 30 days). Here are
#the variables:
# mat = a mx3 matrix that only has pat_id, admit_dt and
#       discharge_dt. This matrix was passed on by the 
#	'readmission_total' wrapper function.
# days = the time window beginning after
#	 the discharge date to count the 
#	 number of readmissions. This was passed on by
#        the 'readmission_total' wrapper function. 
readmit_counter <- function(one_id, m0, days)
{
  m1 <- m0 %>% filter(pat_id %in% one_id)
  m1$admit_dt <- as.Date(m1$admit_dt)
  m1$discharge_dt <- as.Date(m1$discharge_dt)
  m2 <- m1 %>% arrange(admit_dt)
  crit_date <- as.Date(m2[1,"discharge_dt"]) + as.numeric(days)
  counter <- 0
  holder <- sapply(m2$admit_dt[-1], function(ii){
    if( nrow(m1) == 1)
    { counter <- 0 }
    else
    { 
      if( as.Date(ii) < crit_date )
      { counter <- counter + 1 }
      else 
      { counter <- counter + 0 }
    }
  }    
  )
  counter2 <- sum(unlist(holder))
  return(counter2)
}

#This wrapper function will pass on a matrix (see below)
#to the 'readmit_counter' function to count the number 
#of times a patient is readmitted to the hospital within 
#a given time window provided by the user (default will 
#be 30 days). Here are the variables:
# pat_id = patient ID or MRN
# admit_dt = hospital admission date
# discharge_dt = hospital discharge date
# days = the time window beginning after
#	 the discharge date to count the 
#	 number of readmissions 
#The 'pat_id', 'admit_dt' and 'discharge_dt' variables
#will be combined in a matrix format and then passed
#on to the 'readmit_counter' function, along with the
#the 'days' variable. After calculating the number of
#readmissions, this wrapper function will return a 
#matrix with the patient IDs in the first column
#and the number of readmissions in the second column.
readmission_total <- function(pat_id, admit_dt, discharge_dt, days)
{
  d0 <- as.data.frame(cbind(pat_id, as.character(admit_dt), as.character(discharge_dt)))
  colnames(d0) <- c('pat_id', 'admit_dt', 'discharge_dt')
  thepatient <- unique(d0$pat_id)
  theresult <- c()
  counter <- sapply(thepatient, function(ii) {readmit_counter(ii, d0, days)})
  theresult <- as.data.frame(cbind(as.character(thepatient), counter))
  return(theresult)
}

#This function will reformats the summary tables the way that Dr. Shireman wants them:
renamecol <- function(xx) {
  rai_range <- "Total"
  rai_n <- sum(xx$rai_n)
  cumul_count <- xx$cumul_count[nrow(xx)]
  died_n <- sum(xx$died_n)
  #this is how Dr. Shireman wanted the
  #summary fraction reported:
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that died (in this case)
  died_frac <- sum(xx$died_n)/cumul_count
  comp_n <- sum(xx$comp_n)
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that had
  #complications (in this case)
  comp_frac <- sum(xx$comp_n)/cumul_count
  cd4_n <- sum(xx$cd4_n)
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that had
  #Clavien-Dindo Grade 4 complications (in this case)
  cd4_frac <- sum(xx$cd4_n)/cumul_count
  readmsn_n <- sum(xx$readmsn_n)
  #the total number of people in cumulative
  #count in each table divided by the
  #total number of people that had
  #readmissions (in this case)
  readmsn_frac <- sum(xx$readmsn_n)/cumul_count
  newtotals <- t(c(rai_range, rai_n, cumul_count
                   ,died_n, died_frac, comp_n
                   ,comp_frac, cd4_n, cd4_frac
                   ,readmsn_n, readmsn_frac))
  colnames(newtotals) <- colnames(xx)
  newxx <- rbind(xx, newtotals)
  colnames(newxx) <- thecolnames
  return(newxx)
}

#This function is the actual function that will summarize the variable
#information with the help of the 'apply' function. In order for this 
#function to work, the wrapper function 'variable_summary' will need
#to provide the data frame the user wants to summarize and the
#column names. 
summary_vector <- function(xx, df){
  #browser()
  #collecting the name of the column:
  colname <- xx
  #collecting the summary information for that column
  #(either Tukey's 5 number summary or something else): 
  description <- summary(df[,colname])
  #determining if the variable is coninuous or discrete:
  #removing the whitespace in the second row in "description":
  test <- gsub(" ", "", description[2,])
  #if "test" is "Class:character", then varialbe is discrete;
  #else, variable is continuous:
  if(test == "Class:character"){
    vartype <- "discrete"
    #counting the number of observations:
    total_count <- nrow(df[,colname])
    #counting the number of missing observations:
    na_count <- sum(is.na(df[,colname]))
    #creating a table that prints the  categories
    #the discrete variable in descending order:
    count_table <- sort(table(df[,colname], useNA = "always"), decreasing = TRUE)
    #simplifying "description" to a 1 x 1 vector object:
    description <- paste0(names(count_table), ":", count_table, collapse = "; ")
  } else{
    vartype <- "continuous"
    #counting the number of observations:
    total_count <- nrow(df[,colname])
    #counting the number of missing observations:
    na_count <- sum(is.na(df[,colname]))
    #simplifying "description" to a 1 x 1 vector object:
    description <- gsub(" ", "", description) %>% paste0(collapse="; ")
  }
  #combining the variable information into a vector:
  info <- cbind(colname, vartype, total_count, na_count, description)
}

#This function will create a variable summary table that will provide 
#the name of the variable, tells whether the variable is discrete or
#continuous, Tukey's 5 number summary for continuousvariables, and
#the number of observations for each category for discrete variables.
#All the user need for this function is the data frame (df) and the
#function will do the rest.
variable_summary <- function(df){
  #collecting the column names from the data frame:
  all_names <- names(df)
  #passing the column names to an 'sapply' function:
  theresult <- sapply(all_names, summary_vector, df)
  #reformatting the table:
  theresult <- as.data.frame(t(theresult))
  colnames(theresult) <-c('VariableName', 'VariableType', 'TotalCounts', 'NumberOfNAs', 'VariableDescription')
  return(theresult)
}


#' ### Functions for Characterizing and Simulating Data for Sharing and Testing

#' ### This will take a data.frame and simulate a random data.frame that has similar properties to the original
#'
#' Usage:
#
# foo <- faker(mtcars); 
# bar <- faker(mtcars,200);
# baz <- faker(foo);
# fakemeta <- attr(foo,'fm');
# bav <- faker(c(),fm=fakemeta);
#' In the above example, `mtcars` represents the data that is being portably simulated but none of the original
#' data lingers ins the simulated files, only the minimal information about it necessary to create more simulated
#' data. Not a substitute for common sense, though-- it's the user's responsibility to insure that none of the 
#' discrete data (which is, in effect sampled) is confidential. This is provided for informational purposes only
#' with no warrantee whatsoever, including of correctness or of suitability for any purpose. Use at own risk.
#' 
#' `discrete_cutoff` is how few discrete values there need to be in a column before it is no longer treated as
#' a continuous variable
#' `enforcerows` tells faker to keep running itself and generating more data after removing the out-of-range 
#' values until the number of rows is exactly equal to what was requested. It might run quicker but returns 
#' fewer rows than requested if set to FALSE instead of the default TRUE
#' `...` not used yet
faker<-function(dat=c(),nr=c(nrow(dat),200)[1],fm=attr(dat,'fm')
                ,discrete_cutoff=15,enforcerows=T,keepmeta=T,nafrac=1,...){
  if(is.null(nrow(dat)) && is.null(fm)) stop(
    'You need to either provide a data.frame as the first argument or set the fm argument equal to the fm attribute of a data.frame previously simulated by faker()');
  if(is.null(fm)){
    cat(' generating metadata\n');
    fm$allnames <- names(dat);
    fm$unq<-apply(dat,2,function(xx) length(unique(xx)));
    fm$nnas<-apply(dat,2,function(xx) mean(is.na(xx)));
    fm$nonmssg <- fm$nnas<nafrac;
    fm$coldiscr <- names(fm$unq[fm$unq<discrete_cutoff & fm$nonmssg]);
    dat <- data.frame(dat);
    fm$classes <- lapply(dat,class);
    fm$numtypes <- sapply(fm$classes
                          ,function(xx) any(xx %in% c('numeric','integer'
                                                      ,'logical','POSIXct','Date')));
    fm$coldiscr <- union(fm$coldiscr,names(fm$classes)[!fm$numtypes & fm$nonmssg]);
    fm$colnondsc <- setdiff(names(fm$numtypes[fm$numtypes & fm$nonmssg])
                            ,fm$coldiscr);
    datfct <- dat; 
    datfct[,fm$coldiscr]<-lapply(datfct[,fm$coldiscr],factor,exclude=NULL);
    fm$levels <- lapply(datfct[,fm$coldiscr],levels);
    datmtx <- data.matrix(datfct[,fm$nonmssg]);
    # get the centers and scales of the data, and save a scaled copy
    # Important!!! Only the scale and center go here, not actual data!
    fm$ctsc <- setNames(attributes(scdat<-scale(datmtx))[c('scaled:center','scaled:scale')],c('ct','sc'));
    # get the covariance matrix
    cv <- cov(scdat,use='pairwise.complete');
    cv[is.na(cv)] <- 0;
    fm$cv <- as.matrix(nearPD(cv)$mat);
    fm$nrdat <- nrow(datmtx);
    fm$nc <- ncol(datmtx);
    fm$digits<- apply(datmtx,2,function(xx) max(ndec(xx,trailingonly=T),na.rm=T));
    fm$ranges <- apply(datmtx,2,range,na.rm=T);
    #fm$coldiscr <- names(fm$unq[fm$unq<discrete_cutoff]);
    #fm$ranked <- lapply(dat[fm$coldiscr],function(xx) sort(unique(xx)));
    # important-- we do not put real data into fm!!!!
  } #else ranked <- fm$ranked;
  thisfun <- sys.function();
  # generate a standard normal variable and apply the covariance matrix to it, then restore the scale
  fakedat <- data.frame(unscale(matrix(rnorm(nr*fm$nc),nrow=nr) %*% 
                                  chol(fm$cv),fm$ctsc$ct,fm$ctsc$sc));
  # round them all to the right number of digits
  fakedat <- data.frame(mapply(function(aa,bb) round(aa,bb)
                               ,fakedat,fm$digits[names(fakedat)]));
  # find rows where all values fall within the ranges of their respective real-data columns
  keep<-apply(fakedat[,fm$colnondsc],1,function(xx) all(
    xx>=fm$ranges[1,fm$colnondsc]&xx<=fm$ranges[2,fm$colnondsc],na.rm=T));
  # re-assign the discrete levels
  fakedat[,fm$coldiscr] <- mapply(function(aa,bb) aa[as.numeric(cut(bb,breaks=length(aa)))]
                                  ,fm$levels[fm$coldiscr],fakedat[,fm$coldiscr]);
  # restore the columns with many/all missing values
  fakedat[,fm$allnames[!fm$nonmssg]] <- NA;
  # restore the order of the columns. Using make.names() to fix any spaces or
  # other illegal characters in the names...
  fakedat <- fakedat[,make.names(fm$allnames)];
  # ...only to put them all back in again
  names(fakedat) <- fm$allnames;
  # restore the exact column classes
  fakedat <- data.frame(mapply(function(aa,bb) {class(aa)<-bb;aa}
                               ,fakedat,fm$classes[names(fakedat)],SIMPLIFY=F));
  # drop all the values outside the range supported by the data
  fakedat <- fakedat[keep,]; nrf <- nrow(fakedat);
  # keep trying to make up the shortfall with additional sims
  if(enforcerows){
    while(nrf<nr){
      fakedat <- rbind(fakedat,thisfun(dat,nr=2*(nr-nrf),fm=fm,discrete_cutoff=discrete_cutoff,enforcerows=F));
      keep<-apply(fakedat,1,function(xx) all(xx>=fm$ranges[1,]&xx<=fm$ranges[2,]));
      fakedat <- fakedat[keep,];
      nrf <- nrow(fakedat);
    }
  }
  fakedat<-fakedat[seq_len(nr),];
  # only here do we put columns into fm-- and not the columns from the actual data, but from the fake data
  # so now the fake data object is a clean self-contained object that can generate more similar data without
  # further access to the original
  if(keepmeta) attributes(fakedat)<-c(attributes(fakedat),list(fm=fm));
  fakedat;
}

#' unscale centers and scales a vector/matrix/data.frame by the specified center and scale values, which by
#' default it attempts to extract from the attributes where the build-in `scale()` function would have put 
#' them. If there are none, it defaults to values tha will not alter the output.
unscale <- function(xx,center=attr(xx,'scaled:center'),scale=attr(xx,'scaled:scale')) if(!is.null(nrow(xx))) {
  fn<-sys.function(); t(apply(xx,1,fn,center=center,scale=scale))
} else {
  if(is.null(center)) center<-0; if(is.null(scale)) scale<-1; xx*scale+center;
};

#' counts the number of decimal places in a number or vector
#' https://stackoverflow.com/a/48480190/945039
ndec <- function(xx,places=15,trailingonly=F) {
  xx<-abs(xx);
  if(length(xx)>1) {
    fn<-sys.function();
    return(sapply(xx,fn,places=places,trailingonly=trailingonly))};
  if(is.na(xx)||is.infinite(xx)||is.nan(xx)) return(NA);
  if(xx %in% 0:9) return(!trailingonly+0);
  mtch0<-round(xx,nds <- 0:places);
  out <- nds[match(TRUE,mtch0==xx)];
  if(trailingonly) return(out);
  mtch1 <- floor(xx*10^-nds);
  pmin(out,places,na.rm=T) + pmin(nds[match(TRUE,mtch1==0)],places,na.rm=T)
}

