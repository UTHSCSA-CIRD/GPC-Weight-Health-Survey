#' ---
#' title: "IRB Progress Report"
#' author: "Alex F. Bokov"
#' date: "April 26th, 2018"
#' ---
#' 
#+ include=FALSE,cache=FALSE,echo=FALSE
#require(xtable);
require(magrittr); require(dplyr); require(knitr);
require(lme4);
#require(tableone); require(broom); require(dummies); require(readr);
require(pander);
#knitr::opts_chunk$set(echo = TRUE);
options(knitr.kable.NA='-');
defaultdatafile <- 'obesityPaper01.rdata';
scriptid <- 's02_obesitymodels';
vardatafile <- paste0(scriptid,'.datafile');
assign(vardatafile,defaultdatafile);
source('functions.R');
source('trailR.R');

currentscript <- parent.frame(2)$ofile;
if(is.null(currentscript)) currentscript <- 'MANUALLY_RUN_obesityModels.R';
system(sprintf('R -e "options(script_needed=\'%s\');source(\'global.R\');"'
               ,currentscript));

#' You can use 'config.R' to override the file path specified by the defaultfile
#' variable. This allows you to bypass having to wait for it to be created each
#' time you run this script.
if(file.exists('config.R')) source('config.R');
datafile <- mget(vardatafile,ifnotfound = defaultdatafile)[[1]];
if(!file.exists(datafile)) datafile <- defaultdatafile;

tself(currentscript,production=T);
tload(datafile);

#' Race by sex breakdown, non-Hispanic.
race_by_sex_nonhisp <- with(subset(df_fortables,!Hispanic),table(Race,Sex,useNA = 'always'))[,2:1];
race_by_sex_nonhisp;
#' Sex breakdown, Hispanic
sex_hisp <- with(subset(df_fortables,Hispanic),table(Sex,useNA = 'always'))[2:1];
sex_hisp;
#' Unknown sex;
unknown_sex <- sum(is.na(df_fortables$Sex));
unknown_sex;
#' Total counts (should match table 5 in paper)
sum(race_by_sex_nonhisp)+sum(sex_hisp)+unknown_sex;
