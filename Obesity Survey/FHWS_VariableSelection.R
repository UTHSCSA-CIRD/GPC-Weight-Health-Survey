#' ---
#' title: "GPC Family Height and Weight Survey, Variable Selection"
#' author: "Laura Manuel and Alex Bokov, UTHSCSA"
#' date: "May 2nd, 2016"
#' ---

#+ echo=FALSE
source('ciRd.R');
source('obesitySurveyHelpers.R');
if(!all(c(require(party),require(rpart),require(psy)))){
  install.packages(c('party','rpart','psy'));
}
load('survSave.rdata');
resps <- c('possible_research','children_research','deid_data','research_feeling');
preds <- names(samp);
factors <- vs(samp,'f');
responded <- subset(samp,s1s2resp=='Yes');
presurveyvars <- c('site','contact_type','state','match_type','pat_age','pat_bmi_raw','pat_bmi_pct','pat_sex','invite_response_nature','s2resp','surv_2','s1s2resp');

#'# All Variables
#'## Willingness to participate, among survey-2 respondents.
#+ fig.width=10, fig.height=10, echo=FALSE
for(ii in resps) 
  pcawrap(responded,respvar = ii,pca='f',contraction='Yes');

pcawrap(responded);
heatmap(cor(nprep(responded)),symm = T);

#'## Proportion of each question answered
#+ fig.height=10,echo=FALSE,fig.width=10
mar.backup <- par()$mar
par(mar=c(15,4,4,2)+0.1);
plot(sapply(samp,function(xx) mean(!xx%in%c('','0')&!is.na(xx)))
     ,type='h',xaxt='n'
     ,xlab='',ylab='Fraction Answered');
axis(side=1,at=1:67,labels=names(samp),las=2);
par(mar=mar.backup);

#'## Answered first or second survey
#+ fig.width=10, fig.height=10, echo=FALSE
presurvey<-samp[,presurveyvars];
pcawrap(presurvey,'s1s2resp',pca='f',contraction='Yes');
heatmap(cor(nprep(presurvey)),symm = T);

#'# Characteristics of survey respondents.
#+ results='asis',echo=FALSE
for(ii in resps) for(jj in preds[c(5:10,58:63)]) {
  cat("\n\n##",jj," vs ",ii); cat('\n');
  print(runGGPLOT(responded,jj,ii,xlab=jj,ylab=ii,geomOpts = 'v'));
  if(jj%in%factors&&jj!=ii)
    print(runGGPLOT(responded,jj,ii,position = 'fill',ylab='Fraction',xlab=jj));
}
