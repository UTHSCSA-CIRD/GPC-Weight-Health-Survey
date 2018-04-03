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
#' Note to self... url for box.com API token is: 
#' https://app.box.com/developers/console/app/557638
#' The package to require for box.com is boxr() to download and then use 
#' pandoc to convert back to .rmd:
#' 
#' `mv obesityPaper01.gdoc obesityPaper01.docx && pandoc --wrap=none obesityPaper01.docx -t plain -o obesityPaper01.txt && mv obesityPaper01.txt obesityPaper01.rmd`
#' 
#' ...probably in its own branch, then merge current active branch in and merge
#' it back into the currently active branch. Might get ugly with those long lines...
#' 
#+ include=FALSE,cache=FALSE,echo=FALSE
require(xtable);require(magrittr); require(dplyr); require(knitr);
require(tableone); require(broom); require(dummies); require(readr);
require(pander);
#knitr::opts_chunk$set(echo = TRUE);
datafile<-'survProcessed.rdata';
datadict<-'data_dictionary.tsv';
noticefile<-'coauthors_notice.md';
#dir<-'/tmp/gpcob/GPC-Weight-Health-Survey/Obesity Survey/';
options(knitr.kable.NA='-');
#setwd(dir);
load(datafile);
source('functions.R');
#' create our list of data objects, tables, and figures for output
tb <- list();
tb$notice <- read_file(noticefile);
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

#' Calculate BMI from survery self-reported height and weight after filtering 
#' out impossible values
obd$a_bmi <- 703*with(obd
                  ,ifelse(weight_value_lbs<1400&height_in<12*9&height_in>48
                          ,weight_value_lbs/height_in^2,NA));
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
dct0$c_survey_q <- dct0$dataset_column_names %in% c('a_bmi',names(obd)[
  (match('tracker_form_complete',names(obd))+1):
    (min(grep('^ses_',names(obd)))-1)]);
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
dct0$c_spred <- dct0$dataset_column_names %in% c('Recruitment'
                                                 ,'a_recruitTarget','site');
dct0$c_consort <- dct0$dataset_column_names %in% c('contact_type','wave'
                                                   ,'invite_response_nature'
                                                   ,'preferred_contact_method'
                                                   ,'survey_contact_method'
                                                   ,'tracker_form_complete'
                                                   ,'s1s2resp','s2resp');
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
df_unilogist <- cbind(truthy(obd[,v(c_maketf)]),obd[,c(v(c_leave2lev),v(c_ppred_num))]
                     ,dummy.data.frame(obd[,v(c_dummycode)]
                                       ,verbose=T,sep='='))[rsamples$train,];
#' # Prepare data structures for tables.
#' ## Table for Population
df_fortables <- transform(obd[,c(v(c_ppred),v(c_spred),v(c_outcomes))]
                          ,Survey.1.or.2=truthy(s1s2resp)
                          ,Survey.2=truthy(s2resp)
                          ,Hispanic=truthy(ses_hispanic)
                          ,Age=pat_age
                          ,Race=ses_race
                          ,Sex=pat_sex
                          ,`Financial Class`=ses_finclass
                          ,Income=ses_income/1000);
df_univar <- cbind(truthy(obd[,v(c_maketf)])
                   ,obd[,c(v(c_leave2lev),v(c_ppred_num))]
                   ,dummy.data.frame(obd[,v(c_dummycode)]
                                     ,verbose=T,sep='='))[rsamples$train,];
.cohortvars <- c('Sex','Race','Hispanic','Financial.Class','Income','Age','BMI');

#' ### Eligiblility set
tb$dElig <- CreateTableOne(vars=c(.cohortvars,'Survey.1.or.2','Survey.2')
                           ,data=df_fortables);

tb$dRes <- CreateTableOne(vars=c(.cohortvars,'Survey.1.or.2','Survey.2')
                          ,data=subset(df_fortables,Survey.1.or.2));

tb$dResComp <- CreateTableOne(vars=c(.cohortvars,'Survey.1.or.2','Survey.2')
                              ,strata='Survey.2'
                              ,data=subset(df_fortables,Survey.1.or.2));

tb$dEligBySite <- CreateTableOne(vars=c(.cohortvars,'Survey.1.or.2','Survey.2')
                                 ,strata = 'site',data=df_fortables,test=T);

tb$dEligByRecrt <- CreateTableOne(vars=c(.cohortvars,'Survey.1.or.2','Survey.2')
                                  ,strata = 'Recruitment'
                                  ,data=df_fortables,test=T);
#' The consort diagram
tb$dConsort <- CreateTableOne(vars=v(c_consort),strata='site',data=obd,test=F
                              ,includeNA = T);
#' Maximum, minimum, and median values between sites
tb$dEligBySiteRange <- rangetable.TableOne(tb$dEligBySite,medians='Income');
# print(tb$dEligBySite$CatTable,format='p',test=F,print=F
#                              ,showAllLevels = T) %>% 
#   rbind(cbind(level=tb$dEligBySite$MetaData$varNumerics
#                 ,sapply(tb$dEligBySite$ContTable
#                         ,function(xx) xx[,'median']))) %>%
#   apply(1,function(xx) as.data.frame(rbind(c(fivenum(as.numeric(xx))[-c(2,4)]
#                                              ,which.min(xx)
#                                              ,which.max(xx))))) %>% 
#   sapply(function(xx) mutate(xx,min=V1,med=V2,max=V3
#                              ,whichmin=names(xx)[4]
#                              ,whichmax=names(xx)[5])[,c('min','med','max'
#                                                         ,'whichmin','whichmax')]
#          ,simplify=F) %>% bind_rows;
# rownames(tb$dEligBySiteRange) <- c('n',with(varlevels(tb$dEligBySite)
#                                             ,paste0(var,ifelse(level=='','','=')
#                                                     ,level)));



#' ### Responders
tb$dResBySite <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial.Class'
                                      ,'Income','Age','BMI'
                                      ,'Survey.2')
                               ,strata = 'site'
                               ,data=subset(df_fortables,Survey.1.or.2),test=T);
#' ### Completers
tb$dCompBySite <- CreateTableOne(vars=c('Sex','Race','Hispanic','Financial.Class'
                                      ,'Income','Age','BMI')
                               ,strata = 'site'
                               ,data=subset(df_fortables,Survey.2),test=T);
#' ### Adult vs Pediatric sites
#+ results="asis",echo=FALSE,warning=FALSE,message=FALSE
tb$dPeds <- transform(df_fortables,BMI=pat_bmi_raw) %>% 
  subset(a_recruitTarget=='Pediatric') %>% droplevels %>% 
  CreateTableOne(c('Survey.1.or.2','Survey.2','Age','pat_bmi_raw'),strata='site'
                 ,data=.);
tb$dPedsRange <- rangetable.TableOne(tb$dPeds,medians = 'pat_bmi_raw');
tb$dAdult <- transform(df_fortables,BMI=pat_bmi_raw) %>% 
  subset(a_recruitTarget=='Adult') %>% droplevels %>% 
  CreateTableOne(c('Survey.1.or.2','Survey.2','Age','pat_bmi_raw'),strata='site'
                 ,data=.);
tb$dAdultRange <- rangetable.TableOne(tb$dAdult,medians = 'pat_bmi_raw');

#' ### survey responses about possible research
#' 
#' the full set of non free-text survey responses, summarized, not stratified
tb$dSurv <- subset(obd,s2resp=='Yes') %>% droplevels %>% 
  CreateTableOne(setdiff(v(c_survey_strct),c('children_research',v(c_pr_child)))
                 ,data=.,test=F);
tb$dSurvHaveKids <- subset(obd,s2resp=='Yes'&children_in_home=='Yes') %>% 
  droplevels %>% CreateTableOne(c('children_research',v(c_pr_child)),data=.,test=F);
#' ### Univariate predictors of participation
#' 
#' All univariate predictors
glm_s1s2null <- glm(formula = s1s2resp ~ 1, family = "binomial"
                    , data = df_univar);
#' We fit a separate logistic regression model to each numeric and binary 
#' predictor, and to *each level of* each factor predictor with >2 levels.
glm_s1s2uni <- sapply(setdiff(names(df_univar),v(c_outcomes))
                      ,function(xx) {
                        # the back-ticks below are needed because the names have
                        # been altered for readability and are not necessarily
                        # valid R object names anymore unless quoted in this
                        # manner
                        update(glm_s1s2null,as.formula(paste0('.~`',xx,'`')))
                      },simplify=F);
#' We use the `[-1,]` to drop the intercept from each result
tb$dUnivar <- lapply(glm_s1s2uni,function(xx) tidy(xx,conf.int=T)[-1,]) %>% 
  bind_rows();
#' adjust the p-values
tb$dUnivar$p.value <- p.adjust(tb$dUnivar$p.value);
#' rename the terms to human readable
tb$dUnivar$term<-submulti(tb$dUnivar$term,univterms);
#' grab the left side of the `=` containing variable names, or the whole thing
#' otherwise
tb$dUnivar$variable <- sapply(strsplit(tb$dUnivar$term,'='),`[`,1);
#' sort it
tb$dUnivar <- tb$dUnivar[order(tb$dUnivar$variable
                                         ,tb$dUnivar$p.value
                                         ,decreasing = F),];
rownames(tb$dUnivar) <- with(tb$dUnivar,ifelse(p.value<.05
                                               ,paste0('**',term,'**'),term));
#' # Create output tables
#' 
panderOptions('table.split.table',Inf);
panderOptions('table.emphasize.rownames',F);
#' #### Figure 1, CONSORT diagram.
#' 
#' [placeholder]
#' 
#' #### Table 1. Survey Questions
#' 
#' [placeholder]
#' 
#' #### Table 2. Selection criteria used in i2b2 to identify cohort and data elements
#' 
#' [placeholder]
#'  
#' #### Table 3. Detailed list of site, adult/pediatric cohort,and contact method.
tb$t03.sitemethod <- df_fortables[order(df_fortables$a_recruitTarget,df_fortables$Recruitment)
                                  ,c('site','a_recruitTarget','Recruitment')] %>% 
  unique %>% t %>% submulti(cbind(c('Adult','Pediatric','mychart','post','email')
                                  ,c('A','P','Patient\nPortal','USPS','Email')));
dimnames(tb$t03.sitemethod) <- list(c('','Cohort Makeup^1^','Contact Method^2^')
                                    ,tb$t03.sitemethod[1,]);
tb$t03.sitemethod <- pander_return(tb$t03.sitemethod[-1,]
,caption='Table 3: Detailed list of site, adult/pediatric cohort, and contact method.\n
^1^ Cohort makeup: A = adult only, P = pediatric only.\n
^2^ Contact method: USPS = United States Postal Service, Email = electronic mail on file, Portal = patient portal feature of the electronic medical record system. '
                                   ,caption.prefix=':') %>% paste0('\n');
#cat(tb$t03.sitemethod);
#' #### Table 4a. Counts, Age, and BMI: Adult Index Patient
tb$t04a.adultsites <- print(tb$dAdult,printToggle = F) %>% t %>% head(-2) %>%
  pander_return(caption='Table 4a: Counts, Age, and BMI: Adult Index Patients'
                ,caption.prefix=':') %>% paste0('\n');
#' #### Table 4b. Counts, Age, and BMI: Pediatric Index Patient
tb$t04b.pedsites <- print(tb$dPeds,printToggle = F) %>% t %>% head(-2) %>%
  pander_return(caption='Table 4b: Counts, Age, and BMI: Pediatric Index Patients'
                ,caption.prefix=':') %>% paste0('\n');
#' #### Table 5. Cohort, Survey 1 and Survey 2 demographics.
tb$t05.eligible <- lapply(tb[c('dElig','dRes','dResComp')]
                          ,print,printToggle=F) %>%
  with(cbind(dElig,dRes,dResComp[,c('TRUE','p')] )) %>% invisible %>% 
  capture.output(pander.TableOne(.,p.skip=c('Survey.1.or.2 = TRUE (%)','Survey.2 = TRUE (%)')
                ,caption='Table 5: Cohort, Survey 1 and Survey 2 demographics'
                ,cren.fn=function(cc,...) c('Cohort','Survey 1 or 2'
                                            ,'Survey 2','p')
                ,keep.line.breaks=T)) %>% 
  paste0('\n');
# This is a hack: something in the above pipeline is causing a copy of the 
# not-yet-formatted TableOne output to show up in the results with the 
# formatted output appended to it. Hopefully this only happens when matrices 
# are manually passed to pander.TableOne and this is the only time we have to
# manually remove rows from the object:
tb$t05.eligible <- with(tb,t05.eligible[(grep('^-+\n$',t05.eligible)[1]):length(t05.eligible)]);

#' #### Table 6a. Participant demographics by site for cohort [N (% by site), unless otherwise indicated]. 
tb$t06a.eligBySite <- pander_return(tb$dEligBySite
                                    ,cren.fn=function(cc,...) gsub('Financial\\.Class','Financial Class',cc)
                                    ,caption='Table 6a: Participant demographics by site (Cohort)') %>%
  paste0('\n');
#' #### Table 6b. Participant demographics by site (Responders)
# tb$t06b.resBySite <- print(tb$dResBySite,printToggle = F);
# tb$t06b.resBySite[,'p'] <- ifelse(tb$t06b.resBySite[,'p']=='<0.001','*'
#                                   ,ifelse(tb$t06b.resBySite[,'p']=='','','NS'));
# tb$t06b.resBySite <- pander_return(tb$t06b.resBySite[,-ncol(tb$t06b.resBySite)]
#                                     ,row.names=gsub('^([^ ].*)','**\\1**'
#                                                     ,rownames(tb$t06b.resBySite)) %>%
#                                       gsub('^   ','&nbsp;&nbsp;&nbsp;',.)
#                                     ,justify=paste0('l',repChar('r',ncol(tb$t06b.resBySite)-1))
#                                     ,caption='Table 6b: Participant demographics by site (Responders)') %>% 
#   paste0('\n');
tb$t06b.resBySite <- pander_return(tb$dResBySite
                                   ,caption='Table 6b: Participant demographics by site (Responders)') %>%
  paste0('\n');
#' #### Table 6c. Participant demographics by site (Completers)
# tb$t06c.compBySite <- print(tb$dCompBySite,printToggle = F);
# tb$t06c.compBySite[,'p'] <- ifelse(tb$t06c.compBySite[,'p']=='<0.001','*'
#                                   ,ifelse(tb$t06c.compBySite[,'p']=='','','NS'));
# tb$t06c.compBySite <- pander_return(tb$t06c.compBySite[,-ncol(tb$t06c.compBySite)]
#                                     ,row.names=gsub('^([^ ].*)','**\\1**'
#                                                     ,rownames(tb$t06c.compBySite)) %>%
#                                       gsub('^   ','&nbsp;&nbsp;&nbsp;',.)
#                                     ,justify=paste0('l',repChar('r',ncol(tb$t06c.compBySite)-1))
#                                     ,caption='Table 6c: Participant demographics by site (Completers)') %>% 
tb$t06c.compBySite <- pander_return(tb$dCompBySite
                                   ,caption='Table 6c: Participant demographics by site (Completers)') %>%
  paste0('\n');
#' #### Table 7. Univariate predictors of participation
#' 
# tb$t07.univar <- tb$dUnivar[,c('estimate','std.error','statistic'
#                                ,'conf.low','conf.high','p.value')] %>% 
tb$t07.univar <- tb$dUnivar[,c('estimate','conf.low','conf.high','p.value')] %>%
  transform(estimate=exp(estimate)
            ,conf.low=exp(conf.low),conf.high=exp(conf.high)
            ,p.value=add.significance.stars(p.value)) %>% 
  pander_return(digits=5,justify=paste0('l',repChar('r',ncol(.)))
                ,caption='Table 7: Preliminary assessment of variables via univariate logistic regression.') %>%
  paste0('\n');
#' 
#' #### Table 8. Responses to survey questions.
tb$t08A.survresp <- print(tb$dSurv,printToggle=F) %>% 
  pander_return(row.names=gsub('^([^ ].*)','**\\1**',rownames(.)) %>% 
                  gsub('^   ','&nbsp;&nbsp;&nbsp;',.)
                ,justify=paste0('l',repChar('r',ncol(.)))
                ,caption='Table 8a: Survey responses.') %>% paste0('\n');

tb$t08B.survrespkids <- print(tb$dSurvHaveKids,printToggle=F) %>%
  pander_return(row.names=gsub('^([^ ].*)','**\\1**',rownames(.)) %>%
                  gsub('^   ','&nbsp;&nbsp;&nbsp;',.)
                ,justify=paste0('l',repChar('r',ncol(.)))
                ,caption='Table 8b: Survey responses regarding children.') %>% 
  paste0('\n');
# tb$t08B.survrespkids <- pander_return(tb$dSurvHaveKids

#' ## Supplementary
#' 
#' #### Table S1. Cohort, by recruitment method.
#' 
tb$t0S1.eligByRecruit <- pander_return(tb$dEligByRecrt
                                    ,cren.fn=function(cc,...) gsub('Financial\\.Class','Financial Class',cc)
                                    ,caption='Table S1: Participant demographics by recruitment method (Cohort)') %>%
  paste0('\n');
#'
tb$t0S2.consort <- pander_return(tb$dConsort,caption='Table S2: Variables for consort diagram.'
                                 ,explain=F,format='f') %>% 
  paste0('\n');
#' 
#' ## Save everything...
vars_level_names <- merge(dct0,varlevels(obd)
                          ,by.x='dataset_column_names',by.y='var'
                          ,all=T,sort=F) %>% 
  subset(!dataset_column_names %in% c(v(c_meta),'preferred_contact_method'
                                      ,'Race.bak01','state','contact_type'
                                      ,'wave','survey_contact_method'
                                      ,'tracker_form_complete')) %>% 
  transform(variable=dataset_column_names,description=NA) %>% 
  select(c('variable','level','description','class','unique','missing'));
write_tsv(vars_level_names,path='vars_level_names.tsv',na='');
write_tsv(dct0,path='data_dictionary.tsv');
write_tsv(obd[,v(c_consort)],path='LDS_consortdata_obesity.tsv');
save(.workenv,dct0,obd,tb,file='obesityPaper01.rdata');
