#' ---
#' title: "GPC Family Height and Weight Survey, Variable Selection"
#' author: "Laura Manuel and Alex Bokov, UTHSCSA"
#' date: "May 2nd, 2016"
#' ---
source('../ciRd.R');
source('obesitySurveyHelpers.R');
if(!all(c(require(party),require(rpart),require(psy)))){
  install.packages(c('party','rpart','psy'));
}
load('survSave.rdata');
resps <- c('possible_research','children_research','deid_data','research_feeling');
preds <- c('insurance','latino_origin','PR_Me_DependsAbout','PR_Me_If_Spec','PR_Me_Time','PR_Me_Doctor_Op','PR_Me_Compensation','PR_Me_Involve_Child','PR_Me_Other','PR_Child_DependsAbout','PR_Child_If_Spec','PR_Child_TiChild','PR_Child_Doctor_Op','PR_Child_Compensation','PR_Child_Involve_Child','PR_Child_Other');


 #'# Willingness to participate, among survey-2 respondents.
#+ fig.width=10, fig.height=10
for(ii in resps) 
  pcawrap(subset(samp,s2resp=='Yes'),respvar = ii,drop=c('family_id','proj_id','patient_num'),pca='f',contraction='Yes');

pcawrap(subset(samp,s2resp=='Yes'),drop=c('family_id','proj_id','patient_num'));
heatmap(cor(nprep(subset(samp,s2resp=='Yes'))),symm = T);

#'# Answered first or second survey
#+ fig.width=10, fig.height=10
pcawrap(samp[,c(1:13,76)],'s1s2resp',drop=c('family_id','proj_id','patient_num'),pca='f',contraction='Yes');
heatmap(cor(nprep(samp[,c(1:13,76)])),symm = T);

#'# Characteristics of survey respondents.
for(ii in resps) for (jj in c(setdiff(resps,ii),preds)) {
  print(runGGPLOT(samp,ii,jj)); 
  print(runGGPLOT(samp,ii,jj,position = 'fill'))}
