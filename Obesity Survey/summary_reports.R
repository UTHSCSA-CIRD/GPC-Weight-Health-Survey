#' ---
#' title: "Obesity Descriptive Tabular Results"
#' author: "Alex F. Bokov"
#' date: "October 11, 2016"
#' ---
#+ include=FALSE,cache=FALSE,echo=FALSE
require(xtable);
#knitr::opts_chunk$set(echo = TRUE);
datafile='survProcessed.rdata';
dir='/tmp/gpcob/GPC-Weight-Health-Survey/Obesity Survey/';

setwd(dir);
load(datafile);

#'
#' Who did and did not respond to survey-1, by site and patient sex?
#+ results="asis",echo=FALSE
print(xtable(with(obd,addmargins(table(site,s1s2resp:pat_sex))),digits=0),
      type='html');
#' 
#' Who did and did not respond to survey-2, by site and patient sex?
#+ results="asis",echo=FALSE
print(xtable(with(obd,addmargins(table(site,s2resp:pat_sex))),digits=0),
      type='html');

#' 
#' Among the survey-2 respondents only, research attitudes by site.
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(site,possible_research))),digits=0),
      type='html');
#'
#' ...by patient sex.
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(pat_sex,possible_research))),digits=0),
      type='html');

#'
#'  ...by respondent sex.
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(sex,possible_research))),digits=0),
      type='html');

#'
#'  ...by patient age.
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(cut(pat_age,10),possible_research))),digits=0),
      type='html');

#'
#'  ...by income
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(income,possible_research))),digits=0),
      type='html');

#'
#'  ...by insurance.
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(insurance,possible_research))),digits=0),
      type='html');

#'
#'  ...by patient BMI category
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(BMI,possible_research))),digits=0),
      type='html');

#'
#'  ...by latino origin
#+ results="asis",echo=FALSE
print(xtable(with(subset(obd,s2resp=='Yes'),addmargins(table(latino_origin,possible_research))),digits=0),
      type='html');
#' Here is how we would do the hypothesis test...
#with(subset(obd,s2resp=='Yes'),chisq.test(table(latino_origin,possible_research),simulate=T));

