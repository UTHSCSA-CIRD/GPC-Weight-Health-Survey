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
resps <- c('possible_research','deid_data','children_research','research_feeling','res_talk_family','site');
resp <- resps[6];
preds <- setdiff(names(samp),c('weight_value_kg','s1s2resp','s2resp','surv_2'));
#'# Site Deviations
#'## All pre-survey answers missing
#'Wisc
#'## invite_response_nature
#'Only UTHSCSA, Iowa, CMH, and MCRF reported these
#'## preferred_contact_method
#'Only UTHSCSA, Iowa, and CMH reported these
#'## survey_contact_method
#'Only UTHSCSA and Iowa reported these
#'## tracker_form_complete
#'Only UTHSCSA, Iowa, MCW (or whoever goes after Iowa), and CMH reported these
#'CMH reported *only* the complete ones.
#'Wisc did not even have this field
#'## children_in_home
#'Wisc and UNMC got a reasonable amount of yes-es
#'## BMI
#'Wisc, UNMC, and MCW missing this 
#'## pat_bmi_pct
#'Wisc, UNMC, MCW missing this
var_groups <- list(
  uninterpetable=c('contact_type','match_type')
  ,very_unbalanced=c('Race')
  ,not_interesting=c('state','White','Black','American_Indian','Asian','OtherRace','PreferNotAnswer')
  ,possibly_inconsistently_used=c('preferred_contact_method','survey_contact_method','tracker_form_complete')
  ,intepretable_diff=c('research','research_feeling','children_research','deid_data','res_talk_family'
                  ,'invite_response_nature'
                  ,'PR_Me_DependsAbout','PR_Me_If_Spec','PR_Me_Time','PR_Me_Doctor_Op','PR_Me_Compensation','PR_Me_Involve_Child','PR_Me_Other'
                  ,'PR_Child_DependsAbout','PR_Child_If_Spec','PR_Child_TiChild','PR_Child_Doctor_Op','PR_Child_Compensation','PR_Child_Involve_Child','PR_Child_Other'
                  ,'site'
                  ,'cancer_anytype_self'
                  ,'latino_origin','insurance','education','household','language','income','pat_age'
                  ,'BMI'
                  )
  ,interpretable_nodiff=c('cancer_anytype','sex','race','pat_sex')
  ,what_does_var_mean=c('height_req','weight_req')
);
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
  #print(runGGPLOT(responded,jj,resp,xlab=jj,ylab=resp,geomOpts = 'p',omitNA_X = F));
  print(runGGPLOT(responded,resp,jj,xlab=resp,ylab=jj,geomOpts = 'p',omitNA_X = F)); # the site version
  if(jj%in%factors&&jj!=resp)
    #print(runGGPLOT(responded,jj,resp,position = 'fill',ylab='Fraction',xlab=jj,omitNA_X = F));
    print(runGGPLOT(responded,resp,jj,position = 'fill',ylab='Fraction',xlab=resp,omitNA_X = F)); # the site version
}
