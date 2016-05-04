#' ---
#' title: "GPC Family Height and Weight Survey, Part-1: Possible Research"
#' author: "Laura Manuel and Alex Bokov, UTHSCSA"
#' date: "May 2nd, 2016"
#' ---

print(getwd());
#+ echo=FALSE
source('ciRd.R');
source('obesitySurveyHelpers.R');
if(!all(c(require(party),require(rpart),require(psy)))){
  install.packages(c('party','rpart','psy'));
}
load('survSave.rdata');
resp <- 'possible_research';
preds <- names(samp);
factors <- vs(samp,'f');
responded <- subset(samp,s1s2resp=='Yes');
presurveyvars <- c('site','contact_type','state','match_type','pat_age','pat_bmi_raw','pat_bmi_pct','pat_sex','invite_response_nature','s2resp','surv_2','s1s2resp');

#'# Characteristics of survey respondents.
#+ results='asis',echo=FALSE
for(jj in preds) {
  cat("\n\n##",jj," vs ", resp); cat('\n');
  print(runGGPLOT(responded,jj,resp,xlab=jj,ylab=resp,geomOpts = 'v'));
  if(jj%in%factors&&jj!=resp)
    print(runGGPLOT(responded,jj,resp,position = 'fill',ylab='Fraction',xlab=jj));
}
