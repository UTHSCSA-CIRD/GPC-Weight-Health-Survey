#' ---
#' title: "Obesity Descriptive Tabular Results, by Site."
#' author: "Alex F. Bokov"
#' date: "October 11, 2016"
#' ---
#+ include=FALSE,cache=FALSE,echo=FALSE
require(xtable);require(magrittr);
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
#' Recruitment Methods
recruitment <- c(
  CMH='email',
  KUMC='email',
  MCRF='email',
  MCW='email',
  IOWA='post',
  UMN='post',
  UNMC='email',
  UTHSCSA='post',
  UTSW='email',
  WISC='mychart'
);

racecodes_strict <- c(R01='American Indian or Alaska Native',
              R02='Asian',
              R03='Black or African American',
              R04='Native Hawaiian or Other Pacific Islander',
              R05='White',
              R06='Multiple Race',
              R07='Refuse to Answer',
              RNI='No Information',
              RUN='Unknown',
              ROT='Other');

racecodes <- c(R01='Native American',
               R02='Asian',
               R03='African American',
               R04='Other',
               R05='Caucasian',
               R06='Other',
               R07='No Answer',
               RNI='No Answer',
               RUN='No Answer',
               ROT='Other');
ethcodes <- c(Y="Yes",
              N="No",
              R="Refuse to,answer",
              NI="No information",
              UN="Unknown",
              OT="Other");
fincodes <- rep(NA,99);
fincodes[c(1:4,99,98)] <- c("Private",
                            "Medicare",
                            "Medicaid",
                            "Self-Pay",
                            "Unknown",
                            "Other");

obd$Recruitment <- obd$site;
levels(obd$Recruitment) <- recruitment[levels(obd$Recruitment)];

levels(obd$ses_race) <- racecodes[levels(obd$ses_race)];
obd$ses_race <- factor(obd$ses_race,levels=levels(obd$ses_race)[c(5,3,1,2,4,6)]);

obd$ses_finclass <- factor(fincodes[obd$ses_finclass]);
obd$ses_finclass<-factor(obd$ses_finclass,levels=levels(obd$ses_finclass)[c(5,2,1,4,3,6)]);


#' Remove the impossible BMIs that somehow made it through
obd$pat_bmi_raw[obd$pat_bmi_raw>80] <- NA;

#+ results="asis",echo=FALSE
.ii<-'s1s2resp';
#' ## Full sampling frames
#' ### `r responses[[.ii]]`
#+ results="asis",echo=FALSE
.tab <- table(obd$site,obd[[.ii]]); colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                        ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');

#+ results="asis",echo=FALSE
.ii<-'invite_response_nature';
#' ### `r responses[[.ii]]`
#+ results="asis",echo=FALSE
.tab <- table(obd$site,obd[[.ii]]); colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                        ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');

#+ results="asis",echo=FALSE
.ii<-'s2resp'; 
#' ### `r responses[[.ii]]`
#+ results="asis",echo=FALSE
.tab <- table(obd$site,obd[[.ii]]); colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                        ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');
#' ## Survey 2 Respondents Only
#+ results="asis",echo=FALSE
for(.ii in setdiff(names(responses),c('s1s2resp','s2resp','invite_response_nature'))){
  cat('### ',responses[[.ii]],'\n');
  .tab <- table(obd[obd$s2resp=='Yes','site'],obd[obd$s2resp=='Yes',][[.ii]]);
  colnames(.tab)[colnames(.tab)=='']<-'(Missing)';
  print(xtable(addmargins(addmargins(.tab),2,FUN=list(Prop=function(xx) tail(xx,1)/sum(.tab))
                          ,quiet=T),display=c('s',rep('d',ncol(.tab)+1),'f')),type='html');
}

#' ## Patient demographics of the sampling frame
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
#obd[,c('pat_sex','BMI','s1s2resp','pat_age')] %>% 
#  transform(pat_age=cut(pat_age,breaks = c(0,2,5,12,18,20,34,45,65,Inf),include.lowest = T)) %>% 
#  droplevels %>% table %>% addmargins %>% ftable(col.vars = 's1s2resp') %>% 
#  as.matrix %>% `[`(rowSums(.)>0,) %>% xtable %>% print(type='html');
by(obd,c(obd[,c('BMI','pat_sex')],list(obd$BMI=='')),FUN=function(xx) 
  data.frame(Sex=xx$pat_sex[1],Group=xx$BMI[1],`  N  `=nrow(xx),
             ` % Total `=sprintf('%.1f%%',100*nrow(xx)/nrow(obd)),
             ` Age, Median (IQR) `=do.call('sprintf',c(list('%.1f (%.1f-%.1f)'),quantile(xx$pat_age,c(.5,.25,.75),na.rm=T))),
             `  BMI %  `=do.call('sprintf',c(list('%.1f-%.1f'),range(xx$pat_bmi_pct,na.rm=T))),
             `  BMI  `=do.call('sprintf',c(list('%.1f-%.1f'),range(xx$pat_bmi_raw,na.rm=T))),
             check.names=F) %>% sapply(function(xx) gsub('Inf--Inf|0.0-0.0|NA \\(NA-NA\\)','',xx))) %>% 
  do.call('rbind',.) %>% xtable %>% print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);

#' ## Patient demographics of all respondents (both Survey-1 and direct-to-Survey-2)
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
sobd <- subset(obd,s1s2resp=='Yes');
by(sobd,c(sobd[,c('BMI','pat_sex')],list(sobd$BMI=='')),FUN=function(xx) 
  data.frame(Sex=xx$pat_sex[1],Group=xx$BMI[1],`  N  `=nrow(xx),
             ` % Total `=sprintf('%.1f%%',100*nrow(xx)/nrow(sobd)),
             ` Age, Median (IQR) `=do.call('sprintf',c(list('%.1f (%.1f-%.1f)'),quantile(xx$pat_age,c(.5,.25,.75),na.rm=T))),
             `  BMI %  `=do.call('sprintf',c(list('%.1f-%.1f'),range(xx$pat_bmi_pct,na.rm=T))),
             `  BMI  `=do.call('sprintf',c(list('%.1f-%.1f'),range(xx$pat_bmi_raw,na.rm=T))),
             check.names=F) %>% sapply(function(xx) gsub('Inf--Inf|0.0-0.0|NA \\(NA-NA\\)','',xx))) %>% 
  do.call('rbind',.) %>% xtable %>% print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);

#' ## Patient demographics of Survey-2 respondents
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
sobd <- subset(obd,s2resp=='Yes');
by(sobd,c(sobd[,c('BMI','pat_sex')],list(sobd$BMI=='')),FUN=function(xx) 
  data.frame(Sex=xx$pat_sex[1],Group=xx$BMI[1],`  N  `=nrow(xx),
             ` % Total `=sprintf('%.1f%%',100*nrow(xx)/nrow(sobd)),
             ` Age, Median (IQR) `=do.call('sprintf',c(list('%.1f (%.1f-%.1f)'),quantile(xx$pat_age,c(.5,.25,.75),na.rm=T))),
             `  BMI %  `=do.call('sprintf',c(list('%.1f-%.1f'),range(xx$pat_bmi_pct,na.rm=T))),
             `  BMI  `=do.call('sprintf',c(list('%.1f-%.1f'),range(xx$pat_bmi_raw,na.rm=T))),
             check.names=F) %>% sapply(function(xx) gsub('Inf--Inf|0.0-0.0|NA \\(NA-NA\\)','',xx))) %>% 
  do.call('rbind',.) %>% xtable %>% print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);

#' ## November/December Demographic Summaries by site
#' ### Population
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
subset(obd,T) %>% 
  split(.,`$`(.,site)) %>% sapply(function(xx) with (xx,
                                                     c(length(proj_id),'',
                                                       summary(pat_sex)[c('Male','Female')],
                                                       '','',
                                                       summary(Race)[c('White','Black','NativeAm','Asian','Other','PreferNotAnswer')],
                                                       sum(latino_origin=='Yes'),
                                                       rep('',6),
                                                       sprintf('%0.2f (%0.2f)',mean(pat_age,na.rm=T),sd(pat_age,na.rm=T)),
                                                       '',
                                                       sprintf('%0.2f (%0.2f)',mean(pat_bmi_raw,na.rm=T),sd(pat_bmi_raw,na.rm=T))
                                                     ))) %>%
  xtable %>% print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);

#' ### Responders
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
subset(obd,s1s2resp=='Yes') %>% 
  split(.,`$`(.,site)) %>% sapply(function(xx) with (xx,
                                                     c(length(proj_id),'',
                                                       summary(pat_sex)[c('Male','Female')],
                                                       '','',
                                                       summary(Race)[c('White','Black','NativeAm','Asian','Other','PreferNotAnswer')],
                                                       sum(latino_origin=='Yes'),
                                                       rep('',6),
                                                       sprintf('%0.2f (%0.2f)',mean(pat_age,na.rm=T),sd(pat_age,na.rm=T)),
                                                       '',
                                                       sprintf('%0.2f (%0.2f)',mean(pat_bmi_raw,na.rm=T),sd(pat_bmi_raw,na.rm=T))
                                                     ))) %>%
  xtable %>% print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);

#' ### Completers
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
subset(obd,s2resp=='Yes') %>% 
  split(.,`$`(.,site)) %>% sapply(function(xx) with (xx,
                                                     c(length(proj_id),'',
                                                       summary(pat_sex)[c('Male','Female')],
                                                       '','',
                                                       summary(Race)[c('White','Black','NativeAm','Asian','Other','PreferNotAnswer')],
                                                       sum(latino_origin=='Yes'),
                                                       rep('',6),
                                                       sprintf('%0.2f (%0.2f)',mean(pat_age,na.rm=T),sd(pat_age,na.rm=T)),
                                                       '',
                                                       sprintf('%0.2f (%0.2f)',mean(pat_bmi_raw,na.rm=T),sd(pat_bmi_raw,na.rm=T))
                                                       ))) %>%
  xtable %>% print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);


