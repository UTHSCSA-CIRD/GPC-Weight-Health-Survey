#' ---
#' title: "Obesity Model Fitting"
#' author: "Alex F. Bokov"
#' date: "April 4th, 2018"
#' ---
#' 
#+ include=FALSE,cache=FALSE,echo=FALSE
#require(xtable);
require(magrittr); require(dplyr); require(knitr);
require(lme4);
#require(tableone); require(broom); require(dummies); require(readr);
require(pander);
#knitr::opts_chunk$set(echo = TRUE);
datafile<-'obesityPaper01.rdata';
#datadict<-'data_dictionary.tsv';
#noticefile<-'coauthors_notice.md';
#dir<-'/tmp/gpcob/GPC-Weight-Health-Survey/Obesity Survey/';
options(knitr.kable.NA='-');
#setwd(dir);
source('functions.R');
source('trailR.R');
currentscript <- parent.frame(2)$ofile;
if(is.null(currentscript)) currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(currentscript,production=T);
tload(datafile);

#' Our basic, additive GLMM model with site as random effect
glmer_s1s2 <- glmer(s1s2resp ~ ses_hispanic + pat_sex + pat_age + pat_bmi_raw 
                    + ses_income + ses_race + ses_finclass + Recruitment 
                    + (1|site),family=binomial(link='logit'),data=df_unilogist);
#' Then it gets real slow...
#glmer_s1s2_2 <- update(glmer,.~(.)^2);
#' One issue is missing data-- all of Iowa's incomes are missing. We need to 
#' either infer them or drop Iowa from this type of analysis.
# foo<-df_unilogist[rownames(df_unilogist) %in% rownames(na.omit(df_unilogist[,c('pat_sex','pat_age','pat_bmi_raw','ses_income','ses_race','ses_finclass')])),] %>% droplevels
#' The above at least gets rid of that issue. Also, though step, stepAIC, and 
#' lmerTest do not work with glmer, drop1 does.
#' 
#' Tried glmmLasso, and it fit something, but no standard
#' errors nor z.values, no idea if I'm using it correctly
#' 
#' I guess I just need to find a fast computer that I won't
#' need for a while, fit a ~(.)^2 formula, and let it grind, 
#' then come back and repeatedly do drop1() on it...
#' # 
 
