#' ---
#' title: "GPC Family Height and Weight Survey, Part-1: Possible Research"
#' author: "Laura Manuel and Alex Bokov, UTHSCSA"
#' date: "May 2nd, 2016"
#' ---

#+ echo=FALSE
opts_chunk$set(echo=F);
source('ciRd.R');
source('obesitySurveyHelpers.R');
load('survSave.rdata');
resp <- 'deid_data';
preds <- setdiff(names(samp),c('weight_value_kg'));
factors <- vs(samp,'f');
responded <- subset(samp,s1s2resp=='Yes');
presurveyvars <- c('site','contact_type','state','match_type'
                   ,'pat_age','pat_bmi_raw','pat_bmi_pct','pat_sex'
                   ,'invite_response_nature
                   ','s2resp','surv_2','s1s2resp');

#'# Characteristics of survey respondents.
#+ results='asis'
for(jj in preds) {
  cat("\n\n##",jj," vs ", resp); cat('\n');
  print(runGGPLOT(responded,jj,resp,xlab=jj,ylab=resp,geomOpts = 'v'));
  if(jj%in%factors&&jj!=resp)
    print(runGGPLOT(responded,jj,resp,position = 'fill',ylab='Fraction',xlab=jj));
}
