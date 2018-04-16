#' ---
#' title: "Global dependencies for Obesity Survey project"
#' author: "Bokov"
#' date: "04/16/2018"
#' ---

rm(list=ls(all=T));
if(file.exists('config.R')) source('config.R');
currentscript <- parent.frame(2)$ofile;
if(is.null(currentscript)) currentscript <- 'global.R';

depends <- setNames(data.frame(rbind(
   c('ObesityScript.R',NA,'s00_obesityscript.rawdata','testoutput.csv')
  ,c('prepPaper01.R','ObesityScript.R','s01_preppaper.datafile','survProcessed.rdata')
  ,c('obesityModels.R','prepPaper01.R','s02_obesitymodels.datafile','obesityPaper01.rdata')
  ,c('obesityPaper01.rmd','prepPaper01.R','s02_obesitymodels.datafile','obesityPaper01.rdata')
),stringsAsFactors = F)
,c('script','parent_script','parentdata_varname','parentdata_default'));

script_needed <- gsub('MANUALLY_RUN_',''
                      ,getOption('script_needed','obesityPaper01.rmd'));
sp <- subset(depends,script==script_needed[1]);
for( ii in seq(nrow(sp))){
  parentdata_varname <- sp[ii,'parentdata_varname'];
  parentdata_default <- sp[ii,'parentdata_default'];
  parent_script <- sp[ii,'parent_script'];
  datafile <- mget(parentdata_varname,ifnotfound = parentdata_default)[[1]];
  #browser();
  if(datafile != parentdata_default && !file.exists(datafile)) warning(sprintf('
File "%s" does not exist, will attempt to instead use "%s"'
                                             ,datafile
                                             ,datafile<-parentdata_default));
  if(!file.exists(datafile)) {
    warning(sprintf('
File "%s" does not exist, will attempt to create a fresh one via the "%s" script.'
                    ,datafile,parent_script));
    system(sprintf('R -e "options(script_needed=\'%s\');source(\'%s\');"'
           ,parent_script,currentscript));
    system(sprintf('R -e "source(\'%s\');"',parent_script));
  }
}

options(script_needed=NULL);
rm(list=ls(all=T));

# and if parentdata_default exists, set it to that
