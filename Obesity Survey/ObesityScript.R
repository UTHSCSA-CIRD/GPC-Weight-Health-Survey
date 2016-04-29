#Obesity Script
library(ggplot2)
library(plyr)
library(reshape)
library(vcd)

# some handy functions
source('../ciRd.R');


#load clean and save 
obd <- read.table("testoutput.csv", header = TRUE, sep = "\t")

# set variables all in one place if practical
textfields <- grep('^other_|ans6_response$|types2_child$',names(obd),v=T);
numfields <- vs(obd,'z',exclude=c('','None','0'));

# clean up state name
levels(obd$state) <- toupper(levels(obd$state));

#convert non informative race__1 race__2 titles to White/caucasian  Black/African American etc.
colnames(obd)[58:63] = c("White", "Black", "American_Indian", "Asian", "Other", "PrefNotAnswer")

#Clean up in-race names for ggplot -- They're currently too long and overlapping
levels(obd[,58])<-c("0", "0", "White")
levels(obd[,59])<-c("0", "0", "Black")
levels(obd[,60])<-c("0", "0", "American_Indian")
levels(obd[,61])<-c("0", "0", "Asian")
levels(obd[,62])<-c("0", "0","Other")
levels(obd[,63])<-c("0", "0", "NA")

#Clean up willing to participate answers for ggplot -- They're currently too long and overlapping
levels(obd$possible_research)<-c("", "Maybe", "NA", "No", "Yes")

#Clean pt_sex
levels(obd$pat_sex) = c("0", "M", "F", "F", "F","F", "M", "M", "M")

# Arrange the levels for income to keep like incomes together
obd$income <- factor(obd$income, levels(obd$income)[c(8,2,4,3,7,5,6,1)])
obd$Race <- apply(obd[,58:62], 1,concatRace)
obd$Race <- as.factor(obd$Race)
obd$surv_2 <- apply(obd[,17:72], 1, surveyResponded)

#possible research checkboxes for depends on.... for me and child
colnames(obd)[19:25] <- c("PR_Me_DependsAbout","PR_Me_If_Spec","PR_Me_Time","PR_Me_Doctor_Op", "PR_Me_Compensation", "PR_Me_Involve_Child","PR_Me_Other")
colnames(obd)[27:33] <- c("PR_Child_DependsAbout","PR_Child_If_Spec","PR_Child_Time","PR_Child_Doctor_Op", "PR_Child_Compensation", "PR_Child_Involve_Child","PR_Child_Other")
respStr = c("", "0", "Yes") #no survey response, did not check, checked. Using this to these more readable now that the colnames are slightly more readable
levels(obd[,19]) = respStr
levels(obd[,20]) = respStr
levels(obd[,21]) = respStr
levels(obd[,22]) = respStr
levels(obd[,23]) = respStr
levels(obd[,24]) = respStr
levels(obd[,25]) = respStr

levels(obd[,27]) = respStr
levels(obd[,28]) = respStr
levels(obd[,29]) = respStr
levels(obd[,30]) = respStr
levels(obd[,31]) = respStr
levels(obd[,32]) = respStr
levels(obd[,33]) = respStr

# backup of just the systematically modified fields
obd.backup <- obd[,c(textfields,numfields)];

#converting "notes" to characters so they don't get added to the data dictionary as factors when they aren't
obd[,textfields] <- sapply(obd[,textfields],as.character);

# converting things that ought to be numeric (or at least we don't mind if they
# are made numeric) to numeric values
obd[,numfields] <- sapply(obd[,numfields],function(xx) as.numeric(as.character(xx)));

#converting the logical to a factor
obd$surv_2 = as.factor(obd$surv_2)
obd$s2resp <- factor(obd$s2resp);

#bmi factor
obd$BMI = cut(obd$pat_bmi_pct, c(0,25,50,85,95,100)
              ,c("Q1","Q2","Normal","Overweight","Obese"));

samp = pickSample(obd, .25)
save(obd, samp, file = "survSave.rdata")

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
