source('obesitySurveyHelpers.R');
#Some plots by race
runByRaceVariable(samp, "possible_research", "Interested in Being Contacted for Research")
runByRaceVariable(samp, "survey_contact_method", "Survey Contact Method")
runByRaceVariable(samp, "cancer_anytype_self", "Cancer - Self")
runByRaceVariable(samp, "elev_bs_diabetes", "Elevated Blood Sugar/ Diabetes - Child")
runByRaceVariable(samp, "cancer_anytype", "Cancer- Child")
runByRaceVariable(samp, "sex", "Gender")
runByRaceVariable(samp, "latino_origin", "Latino Origin")
runByRaceVariable(samp, "income", "Income")
runByRaceVariable(samp, "insurance", "Insurance by Race")
runGGPLOT(samp, "income", "insurance", xlab ="Income", ylab = "Insurance", omitNA_X = FALSE)
#lets play with some mosaic plots...
ggMosaicP(samp$site, samp$possible_research)
mosaic(structable(site ~ surv_2, data = samp), shade = TRUE, legend = TRUE)

#categorical trees-- just playing with these for now. 
#This was the most interesting one I found
library("party")
fit <- ctree(income ~ site + Race, data = samp)
#Not as much a fan of this one, maybe Alex can work out what to do with it. 
library("rpart")
fit <- rpart(income ~ site + Race, method = "class", data = samp)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Plots by willingness to participate - most of the factors don't seem to stand out
runByWilling2P(samp, "site")
runByWilling2P(samp, "income", "Income by Willingness to Participate")
runByWilling2P(samp, "survey_contact_method") #People have a hard time saying no on the phone? 
