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
#' Recruitment Methods
#+ echo=FALSE
recruitment <- c(CMH='email',KUMC='email',MCRF='email',MCW='email',IOWA='post'
                 ,UMN='post',UNMC='email',UTHSCSA='post',UTSW='email'
                 ,WISC='mychart');
#+ echo=FALSE
univterms <- cbind(c('ses_hispanicTRUE','adultOrChildAdult','pat_sexMale'
                     ,'adultOrChildTRUE','a_recruitTargetPediatric'
                     ,'ses_finclass','site','ses_race','pat_age','pat_bmi_raw'
                     ,'pat_bmi_pct','ses_income','=`','`')
                   ,c("Hispanic", "Adult Patient", "Male"
                      ,"Adult Patient", "Pediatric Site"
                      , "Insurance", "Site", "Race", "Age", "BMI raw"
                      , "BMI percentile", "Income", "=Missing", ""));

#' How race and financial class are binned/renamed
#+ echo=FALSE
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

fin_map <- c(Medicaid='Medicaid',Medicare='Medicare',` `='Unknown'
             ,Other='Other',`Private/ Commercial`= 'Private Insurance',
             `Self-play/no insurance`= 'Self-Pay',Unknown='Unknown');

#' Create recruitment variable 
#' TODO: move to ObesityScript.R
obd$Recruitment <- obd$site;
levels(obd$Recruitment) <- recruitment[levels(obd$Recruitment)];
#' Rename/bin races and financial classes
#' TODO: move to ObesityScript.R
levels(obd$ses_race) <- race_map[levels(obd$ses_race)];
obd$ses_race <- factor(obd$ses_race,levels=levels(obd$ses_race)[c(6,3,1,2,4,5)]);

levels(obd$ses_finclass) <- fin_map[levels(obd$ses_finclass)];
obd$ses_finclass<-factor(obd$ses_finclass,levels=levels(obd$ses_finclass)[c(6,2,1,5,4,3)]);

for(ii in names(obd)) if(is.factor(obd[[ii]])) {
  levels(obd[[ii]])[levels(obd[[ii]])%in%c('',' ','0','1','4')] <- NA;
}
#' 
#' ---
#' **NO MORE CHANGES TO THE `obd` OBJECT PAST THIS POINT!**
#' ---
#' 
#' # Create the data dictionary
#' 
dct0 <- makeddict(obd,append.to = read_tsv(datadict));
#' ## manually-chosen groups of columns
dct0$c_meta <- dct0$dataset_column_names %in% c('family_id','proj_id','patient_num','match_type');
dct0$c_maketf <- dct0$dataset_column_names %in% c('ses_hispanic'
                                                  ,'s2resp','s1s2resp');
dct0$c_leave2lev <- dct0$dataset_column_names %in% c('pat_sex'
                                                     ,'adultOrChild','a_recruitTarget');
#' ## class-based groups of columns
dct0$c_numeric <- with(dct0,class=='numeric' & !dataset_column_names %in% v(c_meta));
dct0$c_factor <- with(dct0,class=='factor' & !dataset_column_names %in% v(c_maketf));
#' ## discrete variables with a large number of levels
dct0$c_manylev <- with(dct0,class %in% c('character','factor') & unique > 12);
dct0$c_manylev[dct0$dataset_column_names=='other_sex'] <- T;
#' ## all survey questions
dct0$c_survey_q <- dct0$dataset_column_names %in% names(obd)[
  (match('tracker_form_complete',names(obd))+1):
    (min(grep('^ses_',names(obd)))-1)];
#' ## all survey questions excluding the manylev ones
dct0$c_survey_strct <- with(dct0,c_survey_q & !c_manylev);
#' ## survey predictors 
dct0$c_survey_ppred<-dct0$dataset_column_names %in% c('latino_origin','Race'
                                                     ,'sex','age','income'
                                                     ,'insurance');
#' ## non-survey patient predictors
dct0$c_ppred <- dct0$dataset_column_names %in% c('ses_hispanic','ses_race'
                                                 ,'pat_sex','pat_age','ses_income'
                                                 ,'ses_finclass','BMI'
                                                 ,'pat_bmi_raw','pat_bmi_pct');
dct0$c_ppred_num <- with(dct0,c_numeric&c_ppred);
#' ## non-survey site predictors
#' 
dct0$c_spred <- dct0$dataset_column_names %in% c('Recruitment','a_recruitTarget','site');
#' 
#' ## outcomes
#' 
dct0$c_pr_me <- grepl('^PR_Me_',dct0$dataset_column_names);
dct0$c_pr_child <- grepl('^PR_Child_',dct0$dataset_column_names)
dct0$c_outcomes <- dct0$dataset_column_names %in% c('s1s2resp','s2resp');
                                                    # ,'possible_research'
                                                    # ,'research'
                                                    # ,'research_feeling'
                                                    # ,'children_research');
dct0$c_dummycode <- with(dct0,(c_ppred|c_spred)&(c_factor)&!(c_maketf|c_leave2lev));
#' # Create the table for preliminary univariate screening on non-survey predictors
ud_nonsrv <- cbind(truthy(obd[,v(c_maketf)]),obd[,c(v(c_leave2lev),v(c_ppred_num))]
                     ,dummy.data.frame(obd[,v(c_dummycode)]
                                       ,verbose=T,sep='='))[rsamples$train,];
#' # Prepare data structures for tables.
#' ## Table for Population
df_fortables <- transform(obd[,c(v(c_ppred),v(c_spred),v(c_outcomes))]
                          ,Responders=truthy(s1s2resp)
                          ,Completers=truthy(s2resp)
                          ,Hispanic=truthy(ses_hispanic)
                          ,Age=pat_age
                          ,Race=ses_race
                          ,Sex=pat_sex
                          ,`Financial Class`=ses_finclass
                          ,Income=ses_income/1000);
#' ### Eligiblility set
tb$dElig <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial.Class'
                                  ,'Income','Age','BMI','Responders'
                                  ,'Completers')
                           ,data=df_fortables);

tb$dRes <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial.Class'
                                     ,'Income','Age','BMI','Responders'
                                     ,'Completers')
                          ,data=subset(df_fortables,Responders));

tb$dResComp <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial.Class'
                                     ,'Income','Age','BMI','Responders'
                                     ,'Completers')
                              ,strata='Completers'
                              ,data=subset(df_fortables,Responders));

tb$dEligBySite <- CreateTableOne(vars=c('Sex','Race','Hispanic'
                                        ,'Financial.Class','Income','Age','BMI'
                                        ,'Responders','Completers')
                                 ,strata = 'site',data=df_fortables,test=T);

tb$dEligByRecrt <- CreateTableOne(vars=c('Sex','Race','Hispanic'
                                         ,'Financial.Class','Income','Age','BMI'
                                         ,'Responders','Completers')
                                  ,strata = 'Recruitment'
                                  ,data=df_fortables,test=T);
#' ### Responders
tb$dResBySite <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial.Class'
                                      ,'Income','Age','BMI'
                                      ,'Completers')
                               ,strata = 'site'
                               ,data=subset(df_fortables,Responders),test=T);
#' ### Completers
tb$dCompBySite <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial.Class'
                                      ,'Income','Age','BMI')
                               ,strata = 'site'
                               ,data=subset(df_fortables,Completers),test=T);
#' ### Adult vs Pediatric sites
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
tb$dPeds <- transform(df_fortables,BMI=pat_bmi_raw) %>% 
  subset(a_recruitTarget=='Pediatric') %>% droplevels %>% 
  CreateTableOne(c('Responders','Completers','Age','pat_bmi_raw'),strata='site'
                 ,data=.);
tb$dAdult <- transform(df_fortables,BMI=pat_bmi_raw) %>% 
  subset(a_recruitTarget=='Adult') %>% droplevels %>% 
  CreateTableOne(c('Responders','Completers','Age','pat_bmi_raw'),strata='site'
                 ,data=.);
#' ### survey responses about possible research
#' 
#' the full set of non free-text survey responses, summarized, not stratified
tb$dSurv <- subset(obd,s2resp=='Yes') %>% droplevels %>% 
  CreateTableOne(v(c_survey_strct),data=.,test=F);

#' # Create output tables
#' 
#' #### Figure 1, CONSORT diagram.
#' 
#' [placehoder]
#' 
#' #### Table 1. Survey Questions
#' 
#' [placehoder]
#' 
#' #### Table 2. Selection criteria used in i2b2 to identify cohort and data elements
#' 
#' [placehoder]
#'  
#' #### Table 3. Detailed list of site, adult/pediatric cohort,and contact method.
#' 
#' #### Table 4a. Counts, Age, and BMI: Adult Index Patient
#' 
#' #### Table 4b. Counts, Age, and BMI: Pediatric Index Patient
#' 
#' #### Table 5. Chort, Survey 1 and Survey 2 demographics.
#' 
#' #### Table 6a. Participant demographics by site for cohort [N (% by site), unless otherwise indicated]. 
#' 
#' #### Table 6b. Participant demographics by site (Responders)
#' 
#' #### Table 6c. Participant demographics by site (Completers)
#' 
#' #### Table 7. Univariate predictors of participation
#' 
#' #### Table 8. Responses to survey questions.
#' 
tb$t09.survey <- print(tb$d03.survey,print=F,noSpaces = T) %>% 
  kable(format = 'markdown');
#' attempt to make column widths non-greedy
tb$t09.survey[2] <- gsub('---{3,}','---',tb$t09.survey[2]);
#' remove extra spaces
for(ii in seq_along(tb$t09.survey)) {
  tb$t09.survey[ii] <- gsub('\\s+',' ',tb$t09.survey[ii]);
}
#' highlight the section headers
for(ii in grep('\\| +\\|$',tb$t09.survey)) {
  tb$t09.survey[ii] <- gsub('([[:alnum:])(%_]{2,})','**\\1**',tb$t09.survey[ii]);
}
for(ii in grep('=|mean \\(sd\\)',tb$t09.survey)){
  tb$t09.survey[ii] <- gsub('^\\|([^|]+)\\|','|**\\1**|',tb$t09.survey[ii]);
}
for(ii in grep('\\*{2}',tb$t09.survey[-(1:2)],invert = T)+2){
  tb$t09.survey[ii] <- gsub('^\\|','|+ ',tb$t09.survey[ii]);
}

#' 
#' ### Overall
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
#' ### Population
tb$t02A.bysite <- print(table02_pop,print=F
                        ,cramVars = c('Sex','Hispanic')
                        ,nonnormal = 'Income')[,-12] %>% 
  #gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% 
  kable(format='markdown');
tb$t02A.bysite.00 <- print(table02_pop,print=F #,cramVars = c('Sex','Hispanic')
                           ,explain=F,nonnormal = 'Income')[,-12] %>% 
  gsub('<0.001','*',.);
tb$t02A.bysite.00['Income',] <- gsub('.00','k',tb$t02A.bysite.00['Income',]);

tb$t02B.byrec <- print(table02a_byrecruit,print=F
                       ,cramVars = c('Sex','Hispanic')
                       ,nonnormal = 'Income')[,-5] %>% 
  gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% kable(format='markdown');
  #gsub('(.*\\(%\\))' #|(^.*\\(mean \\(sd\\)\\))'
  #     ,'**\\1**',.) %>%
  #kable(format='markdown');

#' ### Responders
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
tb$t03.resp <- print(table03_resp,print=F
                     ,cramVars = c('Sex','Hispanic')
                     ,nonnormal = 'Income')[,-12] %>% 
  gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% kable(format='markdown');

#' ### Completers
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
tb$t04.compl <- print(table04_comp,print=F
                      ,cramVars = c('Sex','Hispanic')
                      ,nonnormal = 'Income')[,-12] %>% 
  gsub('000\\.00','k',.) %>% gsub('<0.001','*',.) %>% kable(format='markdown');
#' 
#' ## Model fits
#' 
#' TODO: move to separate script?
#' All univariate predictors
glm_s1s2null <- glm(formula = s1s2resp ~ 1, family = "binomial"
                    , data = ud_nonsrv);
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

#' # Creating Printable Tables!
#' 
#' ### Results by site
#' Adult Index Patients
#+ echo=FALSE, results='asis'
tb$t05A.adbysite <- kable(res_by_site_ad,digits=2,format='markdown');
#' Pediatric Index Patients
#+ echo=FALSE, results='asis'
tb$t05B.pdbysite <- kable(res_by_site_pd,digits=2,format='markdown');

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
# not allowed after dct0 is created!
#obd$a_rebin_ins <- obd$ses_finclass;


tb$t06.univar <- kab_glm_s1s2uni;
write_tsv(dct0,path='data_dictionary.tsv');
save(.workenv,dct0,obd,tb,file='obesityPaper01.rdata');
