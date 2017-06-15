#' ---
#' title: "Obesity Descriptive Tabular Results, by Site."
#' author: "Alex F. Bokov"
#' date: "June 14, 2017"
#' ---
#' 
#' The main purpose of this script/report is to create the tables that
#' will be used in Obesity Survey paper #1. The workflow is to render this
#' as HTML in RStudio, then click `Open in Browser`, then from the browser
#' ctrl-drag to select in a manner that preserves the tabular format,
#' paste into a word processor scrap file, and from there paste into the
#' Word table-containing document.
#' 
#+ include=FALSE,cache=FALSE,echo=FALSE
require(xtable);require(magrittr); require(dplyr);
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
#+ echo=FALSE
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
#' How race and financial class are binned/renamed
race_map <- c(`American Indian or Alaska Native`='Native American',
              Asian='Asian',
              `Black or African American`='African American',
              `Native Hawaiian or Other Pacific Islander`='Other',
              White='Caucasian',
              `Multiple Race`='Other',
              `Refuse to Answer`='No Answer',
              `No Information`='No Answer',
              Unknown='No Answer',
              Other='Other',
              ` `='No Answer');

fin_map <- c(Medicaid='Medicaid',
             Medicare='Medicare',
             ` `='Unknown',
             Other='Other',
             `Private/ Commercial`= 'Private Insurance',
             `Self-play/no insurance`= 'Self-Pay',
             Unknown='Unknown');

#' Create recruitment variable
obd$Recruitment <- obd$site;
levels(obd$Recruitment) <- recruitment[levels(obd$Recruitment)];
#' Rename/bin races and financial classes
levels(obd$ses_race) <- race_map[levels(obd$ses_race)];
obd$ses_race <- factor(obd$ses_race,levels=levels(obd$ses_race)[c(6,3,1,2,4,5)]);

levels(obd$ses_finclass) <- fin_map[levels(obd$ses_finclass)];
obd$ses_finclass<-factor(obd$ses_finclass,levels=levels(obd$ses_finclass)[c(6,2,1,5,4,3)]);

#' Remove the impossible BMIs that somehow made it through
obd$pat_bmi_raw[obd$pat_bmi_raw>80] <- NA;

#' ### Population
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
with(obd
     ,rbind(
       table(N=rep('N',length(site)),site)
       ,rep("\\\ \n",10)
       ,table(pat_sex,site)
       ,rep("\\\ \n",10)
       ,rep("\\\ \n",10)
       ,table(ses_race,site)
       ,Latino=table(ses_hispanic,site)[7,]
       ,rep("\\\ \n",10)
       ,rep("\\\ \n",10)
       ,table(ses_finclass,site))) %>% 
  cbind(` `=row.names(.),.) %>% xtable %>% 
  print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);
summarize(
  group_by(obd,site)
  ,Income = paste0(
    median(ses_income,na.rm = T)/1000
    ,' (',
    paste(quantile(ses_income,c(.25,.75),na.rm = T)/1000,collapse='-')
    ,')'
  )
  ,`Age M(SD)`= paste0(
    round(mean(pat_age,na.rm = T),2)
    ,' ('
    ,round(sd(pat_age,na.rm=T),2)
    ,')'
  )
  ,`Baseline BMI M(SD)`= paste0(
    round(mean(pat_bmi_raw,na.rm=T),2)
    ,' ('
    ,round(sd(pat_bmi_raw,na.rm=T),2)
    ,')')
) %>% t %>% data.frame(stringsAsFactors = F) %>% setNames(rep('',10)) %>%
  rbind("\\\ \n") %>% `[`(c(1,2,5,3,5,4),) %>% xtable %>% 
  print(type='html',html.table.attributes="border=1 cellspacing=3");

#' ### Responders
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
subset(obd,s1s2resp=='Yes') %>%
  with(.,rbind(
       table(N=rep('N',length(site)),site)
       ,rep("\\\ \n",10)
       ,table(pat_sex,site)
       ,rep("\\\ \n",10)
       ,rep("\\\ \n",10)
       ,table(ses_race,site)
       ,Latino=table(ses_hispanic,site)[7,]
       ,rep("\\\ \n",10)
       ,rep("\\\ \n",10)
       ,table(ses_finclass,site))) %>% 
  cbind(` `=row.names(.),.) %>% xtable %>% 
  print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);
subset(obd,s1s2resp=='Yes') %>% group_by(site) %>%
summarize(
  Income = paste0(
    median(ses_income,na.rm = T)/1000
    ,' \n(',
    paste(quantile(ses_income,c(.25,.75),na.rm = T)/1000,collapse='-')
    ,')'
  )
  ,`Age M(SD)`= paste0(
    round(mean(pat_age,na.rm = T),2)
    ,' \n('
    ,round(sd(pat_age,na.rm=T),2)
    ,')'
  )
  ,`Baseline BMI M(SD)`= paste0(
    round(mean(pat_bmi_raw,na.rm=T),2)
    ,' ('
    ,round(sd(pat_bmi_raw,na.rm=T),2)
    ,')')
) %>% t %>% data.frame(stringsAsFactors = F) %>% setNames(rep('',10)) %>%
  rbind("\\\ \n") %>% `[`(c(1,2,5,3,5,4),) %>% xtable %>% 
  print(type='html',html.table.attributes="border=1 cellspacing=3");
#' ### Completers
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
subset(obd,s2resp=='Yes') %>%
  with(.,rbind(
    table(N=rep('N',length(site)),site)
    ,rep("\\\ \n",10)
    ,table(pat_sex,site)
    ,rep("\\\ \n",10)
    ,rep("\\\ \n",10)
    ,table(ses_race,site)
    ,Latino=table(ses_hispanic,site)[7,]
    ,rep("\\\ \n",10)
    ,rep("\\\ \n",10)
    ,table(ses_finclass,site))) %>% 
  cbind(` `=row.names(.),.) %>% xtable %>% 
  print(type='html',html.table.attributes="border=1 cellspacing=3",include.rownames=F);
subset(obd,s2resp=='Yes') %>% group_by(site) %>%
  summarize(
    Income = paste0(
      median(ses_income,na.rm = T)/1000
      ,' (',
      paste(quantile(ses_income,c(.25,.75),na.rm = T)/1000,collapse='-')
      ,')'
    )
    ,`Age M(SD)`= paste0(
      round(mean(pat_age,na.rm = T),2)
      ,' ('
      ,round(sd(pat_age,na.rm=T),2)
      ,')'
    )
    ,`Baseline BMI M(SD)`= paste0(
      round(mean(pat_bmi_raw,na.rm=T),2)
      ,' ('
      ,round(sd(pat_bmi_raw,na.rm=T),2)
      ,')')
  ) %>% t %>% data.frame(stringsAsFactors = F) %>% setNames(rep('',10)) %>%
  rbind("\\\ \n") %>% `[`(c(1,2,5,3,5,4),) %>% xtable %>% 
  print(type='html',html.table.attributes="border=1 cellspacing=3");
