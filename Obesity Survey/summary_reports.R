#' ---
#' title: "Obesity Descriptive Tabular Results, by Site."
#' author: "Alex F. Bokov"
#' date: "October 11, 2016"
#' ---
#+ include=FALSE,cache=FALSE,echo=FALSE
require(xtable);require(magrittr)
#knitr::opts_chunk$set(echo = TRUE);
datafile='survProcessed.rdata';
dir='/tmp/gpcob/GPC-Weight-Health-Survey/Obesity Survey/';

setwd(dir);
load(datafile);

#' These are the names of our response variables:
responses <- list(
  's1s2resp'=c('Responded to Survey 1?'),
  'invite_response_nature'=c('What was the response to Survey-1? (yes/no/bad-addr)'),
  's2resp'=c('Responded to Survey-2?'),
  'possible_research'=c('Willing to participate in research? (yes/maybe/no)'),
  'children_research'=c('Willing for children to participate in research? (yes/maybe/no)'),
  'res_talk_family'=c('Willing to talk to friends/family about research? (yes/maybe/no)'),
  'research_feeling'=c('How feel about medical information being used for research? (from "Fantastic" to "Terrible")'),
  'deid_data'=c('How feel about de-identified data being used for research? (from "Fantastic" to "Terrible")')
);
#' These are the columns for responses that are not "close-enough to yes"
nonAffirmative <- c('Other','NotGoodIdea','Terrible','PreferNotAnswer','No',
                    'Bad Address','(Missing)','Missing');

#' ## Full sampling frames
#' ### `r .ii<-'s1s2resp'; responses[[.ii]]`
#+ results="asis",echo=FALSE
.tab <- table(obd$site,obd[[.ii]]); colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                        ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');
#' ### `r .ii<-'invite_response_nature'; responses[[.ii]]`
#+ results="asis",echo=FALSE
.tab <- table(obd$site,obd[[.ii]]); colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                        ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');
#' ### `r .ii<-'s2resp'; responses[[.ii]]`
#+ results="asis",echo=FALSE
.tab <- table(obd$site,obd[[.ii]]); colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                        ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');
#' ## Survey 2 Respondents Only
#+ results="asis",echo=FALSE
for(.ii in names(responses)[-(1:3)]){
  cat('### ',responses[[.ii]],'\n');
  .tab <- table(obd[obd$s2resp=='Yes','site'],obd[obd$s2resp=='Yes',][[.ii]]);
  colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
  print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                          ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');
}

#' ## Characterizing sampling frame
#+ results="asis",echo=FALSE
obd[,c('pat_sex','BMI','s1s2resp','pat_age')] %>% 
  transform(pat_age=cut(pat_age,breaks = c(0,2,5,12,18,20,34,45,65,Inf),include.lowest = T)) %>% 
  droplevels %>% table %>% addmargins %>% ftable(col.vars = 's1s2resp') %>% 
  as.matrix %>% `[`(rowSums(.)>0,) %>% xtable %>% print(type='html');

