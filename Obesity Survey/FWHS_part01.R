#' ---
#' title: "GPC Family Height and Weight Survey, Pairwise Comparisons"
#' author: "Laura Manuel and Alex Bokov, UTHSCSA"
#' date: "May 2nd, 2016"
#' ---

#+ echo=FALSE,message=FALSE
knitr::opts_chunk$set(echo=F);
library(ggplot2,quietly = T);
source('ciRd.R');
source('obesitySurveyHelpers.R');
load('survSave.rdata');
resps <- c('possible_research','deid_data','children_research','research_feeling');
resp <- resps[4];
preds <- setdiff(names(samp),c('weight_value_kg','s1s2resp'));
var_groups <- list(
  uninterpetable=c('contact_type','match_type')
  ,possibly_inconsistently_used=c('preferred_contact_method','survey_contact_method','tracker_form_complete')
  ,intepretable=c('research')
)
factors <- vs(samp,'f');
responded <- subset(samp,s1s2resp=='Yes');
presurveyvars <- c('site','contact_type','state','match_type'
                   ,'pat_age','pat_bmi_raw','pat_bmi_pct','pat_sex'
                   ,'invite_response_nature
                   ','s2resp','surv_2','s1s2resp');

#'# Covariates of `r resp` among survey respondents.
#+ results='asis'
for(jj in preds) {
  cat("\n\n##",jj," vs ", resp);
  print(runGGPLOT(responded,jj,resp,xlab=jj,ylab=resp,geomOpts = 'p',omitNA_X = F));
  if(jj%in%factors&&jj!=resp)
    print(runGGPLOT(responded,jj,resp,position = 'fill',ylab='Fraction',xlab=jj,omitNA_X = F));
}
