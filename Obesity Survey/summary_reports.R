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
require(xtable);require(magrittr); require(dplyr); require(knitr);
require(tableone); require(broom); require(dummies); require(readr);
#knitr::opts_chunk$set(echo = TRUE);
datafile='survProcessed.rdata';
datadict='data_dictionary.tsv';
dir='/tmp/gpcob/GPC-Weight-Health-Survey/Obesity Survey/';
options(knitr.kable.NA='-');
setwd(dir);
load(datafile);
source('functions.R');
#' create our list of data objects, tables, and figures for output
tb <- list();
#' repeatability info
tb$d00.gitstamp <- gitstamp(production=F,branch=T);
#' create our test, training, and validation sets
set.seed(tb$d01.seed <- rseed);
tb$d02.rsamples <- rsamples <- split(seq_len(nrow(obd))
                                    ,sample(c('train','val','test')
                                            ,nrow(obd),rep=T,prob = c(1,1,3)));
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
univterms <- cbind(c('ses_hispanicTRUE','adultOrChildAdult','pat_sexMale'
                     ,'adultOrChildTRUE'
                     ,'a_recruitTargetPediatric','ses_finclass','site'
                     ,'ses_race','pat_age','pat_bmi_raw','pat_bmi_pct'
                     ,'ses_income','=`','`')
                   ,c("Hispanic", "Adult Patient", "Male"
                      ,"Adult Patient"
                     , "Pediatric Site", "Insurance", "Site"
                     , "Race", "Age", "BMI raw", "BMI percentile"
                     , "Income", "=Missing", ""));

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
#' 
#' ---
#' **NO MORE CHANGES TO THE `obd` OBJECT PAST THIS POINT!**
#' ---
#' 
#' ### Create the data dictionary
# dct0<-data.frame(dataset_column_names=names(obd)
#                  ,class=sapply(obd,function(xx) class(xx)[1])
#                  ,unique=sapply(obd,function(xx) length(na.omit(unique(xx))))
#                  ,missing=sapply(obd,function(xx) sum(is.na(xx)))
#                  ,stringsAsFactors = F);
dct0 <- makeddict(obd,read_tsv(datadict));
#' manually-chosen groups of columns
dct0$c_meta <- dct0$dataset_column_names %in% c('family_id','proj_id','patient_num','match_type');
dct0$c_maketf <- dct0$dataset_column_names %in% c('ses_hispanic'
                                                  ,'s2resp','s1s2resp');
dct0$c_leave2lev <- dct0$dataset_column_names %in% c('pat_sex'
                                                     ,'adultOrChild','a_recruitTarget');
#' class-based groups of columns
dct0$c_numeric <- with(dct0,class=='numeric' & !dataset_column_names %in% v(c_meta));
dct0$c_factor <- with(dct0,class=='factor' & !dataset_column_names %in% v(c_maketf));
#' discrete variables with a large number of levels
dct0$c_manylev <- with(dct0,class %in% c('character','factor') & unique > 12);
#' all survey questions
dct0$c_survey_q <- dct0$dataset_column_names %in% names(obd)[
  (match('tracker_form_complete',names(obd))+1):
    (min(grep('^ses_',names(obd)))-1)];
#' all survey questions excluding the manylev ones
dct0$c_survey_strct <- with(dct0,c_survey_q & !c_manylev);
#' survey predictors 
dct0$c_survey_ppred<-dct0$dataset_column_names %in% c('latino_origin','Race'
                                                     ,'sex','age','income'
                                                     ,'insurance');
#' non-survey patient predictors
dct0$c_ppred <- dct0$dataset_column_names %in% c('ses_hispanic','ses_race'
                                                 ,'pat_sex','pat_age','ses_income'
                                                 ,'ses_finclass','BMI'
                                                 ,'pat_bmi_raw','pat_bmi_pct');
dct0$c_ppred_num <- with(dct0,c_numeric&c_ppred);
#' non-survey site predictors
#' 
dct0$c_spred <- dct0$dataset_column_names %in% c('Recruitment','a_recruitTarget','site');
#' 
#' outcomes
#' 
dct0$c_pr_me <- grepl('^PR_Me_',dct0$dataset_column_names);
dct0$c_pr_child <- grepl('^PR_Child_',dct0$dataset_column_names)
dct0$c_outcomes <- dct0$dataset_column_names %in% c('s1s2resp','s2resp');
                                                    # ,'possible_research'
                                                    # ,'research'
                                                    # ,'research_feeling'
                                                    # ,'children_research');
dct0$c_dummycode <- with(dct0,(c_ppred|c_spred)&(c_factor)&!(c_maketf|c_leave2lev));
#' ### Create the table for preliminary univariate screening on non-survey predictors
ud_nonsrv <- cbind(truthy(obd[,v(c_maketf)]),obd[,c(v(c_leave2lev),v(c_ppred_num))]
                     ,dummy.data.frame(obd[,v(c_dummycode)]
                                       ,verbose=T,sep='='))[rsamples$train,];
#' ### survey responses about possible research
tb$d03A.pr_me <- apply(obd[,v(c_pr_me)],2,sum)/sum(truthy(obd$s1s2resp))
tb$d03B.pr_child <- apply(obd[,v(c_pr_child)],2,sum)/sum(truthy(obd$s1s2resp));
#' the full set of non free-text survey responses, summarized, not stratified
tb$d03.survey <- CreateTableOne(v(c_survey_strct),data=subset(obd,s2resp=='Yes'),test=F);
tb$t09.survey <- print(tb$d03.survey,print=F) %>% kable(format = 'markdown');
#' 
#' ### Overall
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
cbind(Population=with(obd,c(
  `**N**`=nrow(obd),table(pat_sex),`**Race**`='',table(ses_race),`**Latino**`=table(ses_hispanic)[[7]]
  ,`**Financial Class**`=' '
  ,table(ses_finclass)
  ,Income=paste0(median(ses_income,na.rm=T)/1000,' (',paste(quantile(ses_income,c(.25,.75),na.rm=T)/1000,collapse='-'),')')
  ,`**Age**`=paste0(round(mean(pat_age,na.rm=T),2),' (',round(sd(pat_age,na.rm=T),2),')')
  ,`**BMI**`=paste0(round(mean(pat_bmi_raw,na.rm=T),2),' (',round(sd(pat_bmi_raw,na.rm=T),2),')')
  ))
  ,Responders=with(subset(obd,s1s2resp=='Yes'),c(
    N=length(site),table(pat_sex),'',table(ses_race),Latino=table(ses_hispanic)[[7]]
    ,' '
    ,table(ses_finclass)
    ,Income=paste0(median(ses_income,na.rm=T)/1000,' (',paste(quantile(ses_income,c(.25,.75),na.rm=T)/1000,collapse='-'),')')
    ,Age=paste0(round(mean(pat_age,na.rm=T),2),' (',round(sd(pat_age,na.rm=T),2),')')
    ,BMI=paste0(round(mean(pat_bmi_raw,na.rm=T),2),' (',round(sd(pat_bmi_raw,na.rm=T),2),')')
    ))
  ,Completers=with(subset(obd,s2resp=='Yes'),c(
    N=length(site),table(pat_sex),'',table(ses_race),Latino=table(ses_hispanic)[[7]]
    ,' ',table(ses_finclass)
    ,Income=paste0(median(ses_income,na.rm=T)/1000,' (',paste(quantile(ses_income,c(.25,.75),na.rm=T)/1000,collapse='-'),')')
    ,Age=paste0(round(mean(pat_age,na.rm=T),2),' (',round(sd(pat_age,na.rm=T),2),')')
    ,BMI=paste0(round(mean(pat_bmi_raw,na.rm=T),2),' (',round(sd(pat_bmi_raw,na.rm=T),2),')')
    ))
  ) -> table_overall;
print(tb$t01.overall <- kable(table_overall,format='markdown'));
#+ results='asis',echo=FALSE,warning=FALSE,message=FALSE
xtable(table_overall) %>%
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

#' alt version
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
df_fortables <- transform(obd[,c(v(c_ppred),v(c_spred),v(c_outcomes))]
                          ,Responders=truthy(s1s2resp)
                          ,Completers=truthy(s2resp)
                          ,Hispanic=truthy(ses_hispanic)
                          ,Age=pat_age
                          ,Race=ses_race
                          ,Sex=pat_sex
                          ,`Financial Class`=ses_finclass
                          ,Income=ses_income);
table02_pop <- CreateTableOne(vars=c('Sex','Race','Hispanic'
                                                      ,'Financial.Class'
                                                      ,'Income','Age','BMI'
                                                      ,'Responders','Completers')
                                               ,strata = 'site'
                                               ,data=df_fortables,test=T);
table02a_byrecruit <- CreateTableOne(vars=c('Sex','Race',
                                                            'Hispanic'
                                                            ,'Financial.Class'
                                                            ,'Income','Age'
                                                            ,'BMI'
                                                            ,'Responders'
                                                            ,'Completers')
                                                     ,strata = 'Recruitment'
                                                     ,data=df_fortables,test=T);
tb$t02A.bysite <- print(table02_pop,print=F
                        ,cramVars = c('Sex','Hispanic')
                        ,nonnormal = 'Income')[,-12] %>% 
  gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% kable(format='markdown');

tb$t02B.byrec <- print(table02a_byrecruit,print=F
                       ,cramVars = c('Sex','Hispanic')
                       ,nonnormal = 'Income')[,-5] %>% 
  gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% kable(format='markdown');
  #gsub('(.*\\(%\\))' #|(^.*\\(mean \\(sd\\)\\))'
  #     ,'**\\1**',.) %>%
  #kable(format='markdown');

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
#' alt version
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
table03_resp <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial Class'
                                     ,'Income','Age','BMI'
                                     ,'Completers')
                              ,strata = 'site'
                              ,data=subset(df_fortables,Responders),test=T);
tb$t03.resp <- print(table03_resp,print=F
                     ,cramVars = c('Sex','Hispanic')
                     ,nonnormal = 'Income')[,-12] %>% 
  gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% kable(format='markdown');

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
#' alt version
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
table04_comp <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial Class'
                                      ,'Income','Age','BMI')
                               ,strata = 'site'
                               ,data=subset(df_fortables,Completers),test=T);
tb$t04.compl <- print(table04_comp,print=F
                      ,cramVars = c('Sex','Hispanic')
                      ,nonnormal = 'Income')[,-12] %>% 
  gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% kable(format='markdown');

#' ### Results by site
res_by_site_pd <- subset(obd,pat_age<=21 & a_recruitTarget=='Pediatric') %>% group_by(site,Recruitment) %>% 
  summarise(Eligible=n(),`Survey 1`=sum(na.omit(s1s2resp=='Yes'))
            ,`Survey 2`=sum(na.omit(s2resp=='Yes'))
            ,`Age (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_age,na.rm=T),sd(pat_age,na.rm=T))
            ,`BMI (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_bmi_raw,na.rm=T),sd(pat_bmi_raw,na.rm=T))
  );
res_by_site_ad <- subset(obd,pat_age>21 & a_recruitTarget=='Adult') %>% group_by(site,Recruitment) %>% 
  summarise(Eligible=n(),`Survey 1`=sum(na.omit(s1s2resp=='Yes'))
            ,`Survey 2`=sum(na.omit(s2resp=='Yes'))
            ,`Age (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_age,na.rm=T),sd(pat_age,na.rm=T))
            ,`BMI (SD)`=sprintf('%5.2f (%5.2f)',mean(pat_bmi_raw,na.rm=T),sd(pat_bmi_raw,na.rm=T))
            #,`BMI (SD)`=paste0(mean(pat_bmi_raw,na.rm=T),' ',round(sd(pat_raw,na.rm=T),2))
            #,BMI=mean(pat_bmi_raw,na.rm=T),`(SD )`=sd(pat_bmi_raw,na.rm=T)
            );

#res_by_site <- merge(res_by_site_pd,res_by_site_ad,all=T
#                     ,by = c('site','Recruitment'),suffixes = c(' ped',' adl'));

#res_by_site_pd <- res_by_site[,c(names(res_by_site)[1:2]
#                              ,grep(' ped$',names(res_by_site),val=T))] %>% 
#  setNames(.,names(res_by_site_pd));
#
#res_by_site_ad <- res_by_site[,c(names(res_by_site)[1:2]
#                                 ,grep(' adl$',names(res_by_site),val=T))] %>% 
#  setNames(names(res_by_site_ad));
#' 
#' Adult Index Patients
#+ echo=FALSE, results='asis'
tb$t05A.adbysite <- kable(res_by_site_ad,digits=2,format='markdown');
#' Pediatric Index Patients
#+ echo=FALSE, results='asis'
tb$t05B.pdbysite <- kable(res_by_site_pd,digits=2,format='markdown');

#' Coming up next... all univariate predictors
glm_s1s2null <- glm(formula = s1s2resp ~ 1, family = "binomial"
                    , data = ud_nonsrv);
#sapply(c(v(c_ppred),'Recruitment','a_recruitTarget'),function(xx) as.formula(paste0('.~',xx)),simplify=F) %>% 
#  lapply(function(xx) update(glm_s1s2null,xx)) -> glm_s1s2_univ;
#lapply(glm_s1s2_univ,tidy,conf.int=.95) %>% do.call(rbind,.) %>% kable(format='markdown',row.names=F);
#' We fit a separate logistic regression model to each numeric and binary 
#' predictor, and to *each level of* each factor predictor with >2 levels.
glm_s1s2uni <- sapply(setdiff(names(ud_nonsrv),v(c_outcomes))
                     ,function(xx) {
                       # the back-ticks below are needed because the names have
                       # been altered for readability and are not necessarily
                       # valid R object names anymore unless quoted in this
                       # manner
                       update(glm_s1s2null,as.formula(paste0('.~`',xx,'`')))
                       },simplify=F);
#' We use the `[-1,]` to drop the intercept from each result
tab_glm_s1s2uni <- lapply(glm_s1s2uni,function(xx) tidy(xx,conf.int=T)[-1,]) %>% 
  bind_rows();
#' adjust the p-values
tab_glm_s1s2uni$p.value <- p.adjust(tab_glm_s1s2uni$p.value);
#' rename the terms to human readable
tab_glm_s1s2uni$term<-submulti(tab_glm_s1s2uni$term,univterms);
#' grab the left side of the `=` containing variable names, or the whole thing
#' otherwise
tab_glm_s1s2uni$variable <- sapply(strsplit(tab_glm_s1s2uni$term,'='),`[`,1);
#' sort it
tab_glm_s1s2uni <- tab_glm_s1s2uni[order(tab_glm_s1s2uni$variable
                                         ,tab_glm_s1s2uni$p.value
                                         ,decreasing = F),];
#' create the raw kable
kab_glm_s1s2uni <- tab_glm_s1s2uni[,-ncol(tab_glm_s1s2uni)] %>%
  transform(p.value=ifelse(p.value<.001,'<0.001',round(p.value,3))) %>%
  kable(format = 'markdown',row.names=F,digits=5);
#' tweak the kable to highlight significant differences
for(ii in seq_len(nrow(tab_glm_s1s2uni))) if(tab_glm_s1s2uni[ii,'p.value']<.05){
  kab_glm_s1s2uni[ii+2]<-gsub('[ ]{1,}',' ',kab_glm_s1s2uni[ii+2]) %>%
    gsub('\\|[ ]{0,1}([A-Za-z0-9. -=]{2,})[ ]{0,1}\\|','| **\\1** |',.) %>%
    gsub('\\| ([0-9.-]{2,})\\|','| **\\1** |',.)
  };
  #   gsub('\\|$','**|',.) %>% 
  #   gsub('([A-Za-z0-9 -])\\|([A-Za-z0-9 -=])','\\1**|**\\2',.)
  # };

#'
#' Re-binning certain variables based on univariate results
obd$a_rebin_ins <- obd$ses_finclass;


tb$t06.univar <- kab_glm_s1s2uni;
write_tsv(dct0,path='data_dictionary.tsv');
save(.workenv,dct0,obd,tb,file='obesityPaper01.rdata');
