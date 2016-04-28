#Obesity Script
library(ggplot2)
library(plyr)
library(reshape)
library(vcd)
#save(obd, samp, file = "survSave.rdata")
load("survSave.rdata")
# obd <- read.table("obesity_survey_v0.3.csv", header = TRUE, sep = "\t")
# 
# #convert non informative race__1 race__2 titles to White/caucasian  Black/African American etc.
# colnames(obd)[58:63] = c("White", "Black", "American_Indian", "Asian", "Other", "PrefNotAnswer")
# 
# #Clean up in-race names for ggplot -- They're currently too long and overlapping
# levels(obd[,58])<-c("0", "0", "White")
# levels(obd[,59])<-c("0", "Black")
# levels(obd[,60])<-c("0", "American_Indian")
# levels(obd[,62])<-c("0", "Other")
# levels(obd[,63])<-c("0", "NA")
# 
# #Clean up willing to participate answers for ggplot -- They're currently too long and overlapping
# levels(obd$possible_research)<-c("", "Maybe", "NA", "No", "Yes")
# 
# #Clean pt_sex
# levels(obd$pat_sex) = c("F", "F", "M", "M")
# 
# # Arrange the levels for income to keep like incomes together
# obd$income <- factor(obd$income, levels(obd$income)[c(8,2,4,3,7,5,6,1)])
# obd$Race <- apply(obd[,58:62], 1,concatRace)
# obd$Race <- as.factor(obd$Race)
# obd$surv_2 <- apply(obd[,17:72], 1, surveyResponded)
# samp = pickSample(obd, .25)


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
