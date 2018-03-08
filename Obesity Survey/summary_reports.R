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
source('functions.R');

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

# obd$a_resplevel <- with(obd,factor(interaction(s1s2resp:s2resp,drop=T),exclude='',levels=c('No:No','Yes:No','Yes:Yes',NA),labels=c('Neither','Responder','Completer','NONE')));
# levels(obd$a_resplevel)[levels(obd$a_resplevel)=='NONE'] <- 'Neither';

#' Remove the impossible BMIs that somehow made it through
# moved to ObesityScript.R
#obd$pat_bmi_raw[obd$pat_bmi_raw>80] <- NA;

#' ### Create the data dictionary
dct0<-data.frame(dataset_column_names=names(obd)
                 ,class=sapply(obd,function(xx) class(xx)[1])
                 ,stringsAsFactors = F);
#' 
#' survey predictors 
#' TODO: calculate BMI from patient responses
dct0$c_survey_ppred<-dct0$dataset_column_names %in% c('latino_origin','Race'
                                                     ,'sex','age','income'
                                                     ,'insurance');
#' non-survey patient predictors
dct0$c_ppred <- dct0$dataset_column_names %in% c('ses_hispanic','ses_race'
                                                 ,'pat_sex','pat_age','ses_income'
                                                 ,'ses_finclass','BMI');
#' non-survey site predictors
#' 
dct0$c_spred <- dct0$dataset_column_names %in% c('Recruitment','a_recruitTarget');
#' 
#' outcomes
#' 
dct0$c_outcomes <- dct0$dataset_column_names %in% c('s1s2resp','s2resp');
                                                    # ,'possible_research'
                                                    # ,'research'
                                                    # ,'research_feeling'
                                                    # ,'children_research');
#' 
#' ### Overall
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
cbind(Population=with(obd,c(
  N=nrow(obd),table(pat_sex),table(ses_race),Latino=table(ses_hispanic)[[7]]
  ,table(ses_finclass)
  ,Income=paste0(median(ses_income,na.rm=T)/1000,' (',paste(quantile(ses_income,c(.25,.75),na.rm=T)/1000,collapse='-'),')')
  ,Age=paste0(round(mean(pat_age,na.rm=T),2),' (',round(sd(pat_age,na.rm=T),2),')')
  ,BMI=paste0(round(mean(pat_bmi_raw,na.rm=T),2),' (',round(sd(pat_bmi_raw,na.rm=T),2),')')
  ))
  ,Responders=with(subset(obd,s1s2resp=='Yes'),c(
    N=length(site),table(pat_sex),table(ses_race),Latino=table(ses_hispanic)[[7]]
    ,table(ses_finclass)
    ,Income=paste0(median(ses_income,na.rm=T)/1000,' (',paste(quantile(ses_income,c(.25,.75),na.rm=T)/1000,collapse='-'),')')
    ,Age=paste0(round(mean(pat_age,na.rm=T),2),' (',round(sd(pat_age,na.rm=T),2),')')
    ,BMI=paste0(round(mean(pat_bmi_raw,na.rm=T),2),' (',round(sd(pat_bmi_raw,na.rm=T),2),')')
    ))
  ,Completers=with(subset(obd,s2resp=='Yes'),c(
    N=length(site),table(pat_sex),table(ses_race),Latino=table(ses_hispanic)[[7]],table(ses_finclass)
    ,Income=paste0(median(ses_income,na.rm=T)/1000,' (',paste(quantile(ses_income,c(.25,.75),na.rm=T)/1000,collapse='-'),')')
    ,Age=paste0(round(mean(pat_age,na.rm=T),2),' (',round(sd(pat_age,na.rm=T),2),')')
    ,BMI=paste0(round(mean(pat_bmi_raw,na.rm=T),2),' (',round(sd(pat_bmi_raw,na.rm=T),2),')')
    ))
  ) %>% xtable %>% 
  print(type='html',html.table.attributes="border=1 cellspacing=3");

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

res_by_site_pd <- subset(obd,pat_age<18) %>% group_by(site,Recruitment) %>% 
  summarise(Eligible=n(),`Survey 1`=sum(na.omit(s1s2resp=='Yes'))
            ,`Survey 2`=sum(na.omit(s2resp=='Yes'))
            ,`Age (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_age,na.rm=T),sd(pat_age,na.rm=T))
            ,`BMI (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_bmi_raw,na.rm=T),sd(pat_bmi_raw,na.rm=T))
  );

res_by_site_ad <- subset(obd,pat_age>=18) %>% group_by(site,Recruitment) %>% 
  summarise(Eligible=n(),`Survey 1`=sum(na.omit(s1s2resp=='Yes'))
            ,`Survey 2`=sum(na.omit(s2resp=='Yes'))
            ,`Age (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_age,na.rm=T),sd(pat_age,na.rm=T))
            ,`BMI (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_bmi_raw,na.rm=T),sd(pat_bmi_raw,na.rm=T))
            #,`BMI (SD)`=paste0(mean(pat_bmi_raw,na.rm=T),' ',round(sd(pat_raw,na.rm=T),2))
            #,BMI=mean(pat_bmi_raw,na.rm=T),`(SD )`=sd(pat_bmi_raw,na.rm=T)
            );

res_by_site <- merge(res_by_site_pd,res_by_site_ad,all=T
                     ,by = c('site','Recruitment'),suffixes = c(' ped',' adl'));

res_by_site_pd <- res_by_site[,c(names(res_by_site)[1:2]
                              ,grep(' ped$',names(res_by_site),val=T))] %>% 
  setNames(.,names(res_by_site_pd));

res_by_site_ad <- res_by_site[,c(names(res_by_site)[1:2]
                                 ,grep(' adl$',names(res_by_site),val=T))] %>% 
  setNames(names(res_by_site_ad));
#' 
#' Adult Index Patients
#+ echo=FALSE, results='asis'
knitr::kable(res_by_site_ad,digits=2,format='markdown') %>% gsub('NA','-',.);
#' Pediatric Index Patients
#+ echo=FALSE, results='asis'
knitr::kable(res_by_site_pd,digits=2,format='markdown') %>% gsub('NA','-',.);
