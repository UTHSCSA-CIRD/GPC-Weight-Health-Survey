#Obesity Script
library(plyr)
library(reshape)
library(vcd)
library(digest)

# some handy functions
source('obesitySurveyHelpers.R');

rseed <- 6062016;
set.seed(rseed);

#load clean and save 
obd <- read.table("testoutput.csv", header = TRUE, sep = "\t")

# set variables all in one place if practical
textfields <- grep('^other_|ans6_response$|types2_child$',names(obd),v=T);
numfields <- vs(obd,'z',exclude=c('','None','0'));
racenames <- grep('race___',names(obd),val=T);
defaultNlevels <- 2;
researchaccept <- grep('research_accept_dec',names(obd),val=T);
# variables that are pseudo-IDs rather than actual data
toOmit <- c('wave','family_id','proj_id','patient_num');

# backup of just the systematically modified fields
obd.backup <- obd;
#converting "notes" to characters so they don't get added to the data dictionary as factors when they aren't
obd[,textfields] <- sapply(obd[,textfields],as.character);
# converting things that ought to be numeric (or at least we don't mind if they
# are made numeric) to numeric values
obd[,numfields] <- sapply(obd[,numfields],function(xx) as.numeric(as.character(xx)));

# the factor names need to be collected here, because some of the above steps 
# cause certain columns to stop being factors
factors <- vs(obd,'f');

# clean up state name
levels(obd$state) <- toupper(levels(obd$state));
#Clean pt_sex
obd$pat_sex <- mapstrings(obd$pat_sex,sexstringmap);
# clean up ALL other factor levels for readability
# though we're not done yet, some of them still have small numbers of garbage
# Apparently we need to use the simplify=F argument to prevent coercion of factors to strings
obd[,factors] <- sapply(obd[,factors],mapstrings,simplify = F);


#convert non informative race__1 race__2 titles to White/caucasian  Black/African American etc.
# All column names will be cleaned up in one shot further down, after we are 
# done using them.

#Clean up in-race names for ggplot -- They're currently too long and overlapping
obd[,racenames] <- sapply(obd[,racenames],binfactor,lev=defaultNlevels,oth='0',simplify=F);
# Combine race columns into a single one
obd$Race <- interaction(obd[,racenames],drop = T,sep = '');
levels(obd$Race) <- gsub('^White[0]{0,1}([A-Z])','\\1'
                 ,gsub('^0|0$',''
                       ,gsub('0+','0',levels(obd$Race))));

# Arrange the levels for income to keep like incomes together
# hardcoding indexes into levels is unstable, can change when data refreshed
obd$income <- factor(obd$income,levels=sort(levels(obd$income)));
# far from perfect, but at least resistant to changing of level order
# and can accommodate at least some variation in level names
levels(obd$income) <- gsub('^([1-9])','$\\1'
                           ,gsub('-0*','-$'
                                 ,gsub('^0([1-9])','\\1'
                                       ,gsub('^0+','0',levels(obd$income)))));

# find survey responses
obd$surv_2 <- apply(obd[,c(17:41,43,45:72)], 1, surveyResponded)

#possible research checkboxes for depends on.... for me and child
# All column names will be cleaned up in one shot further down, after we are 
# done using them.
#respStr = c("", "0", "Yes") #no survey response, did not check, checked. Using this to these more readable now that the colnames are slightly more readable
# As it turns out, "" is not 'no survey response', "0" is and "" may be an 
# artifact
# So here we can get rid of them...
obd[,researchaccept] <- sapply(obd[,researchaccept],binfactor,lev=defaultNlevels,oth='0',simplify = F);

#converting the logicals back to factors
obd$surv_2 = as.factor(obd$surv_2)
obd$s2resp <- factor(obd$s2resp,levels=c('0','1'),labels=c('No','Yes'));
# answered the first survey and/or the second survey
obd$s1s2resp <- factor(obd$s2resp=='Yes'|obd$invite_response_nature=='Yes',levels=c('FALSE','TRUE'),labels=c('No','Yes'));


# reordering the yes-no-maybe variables
obd[,factors] <- sapply(obd[,factors],reOrderYesNo,simplify=F);
# reordering the longer variables
obd[,factors] <- sapply(obd[,factors],longFactorLev,simplify=F);

#bmi factor
obd$BMI <- cut(obd$pat_bmi_pct, c(0,5,85,95,100)
              ,c("Underweight","Normal","Overweight","Obese"));
#make the missing BMI values follow the same convention as the rest of the factors
obd$BMI <- factor(obd$BMI,levels=c(NA,levels(obd$BMI)),labels=c('',levels(obd$BMI))
                  ,exclude=NULL);

# tracker_form_complete ought be a factor
obd$tracker_form_complete <- factor(obd$tracker_form_complete,levels=0:2,labels=c('0','Partial','Complete'));


for(ii in names(obd.backup)) 
  if(isTRUE(all.equal(as.character(obd.backup[[ii]]),as.character(obd[[ii]])))) 
    obd.backup[,ii]<-NULL;

names(obd) <- mapstrings(names(obd),colnamestringmap);
names(obd.backup) <- mapstrings(names(obd.backup),colnamestringmap);

##Data Enhancements 
samp$adultOrChild = as.factor(samp$pat_age > 18)

samp$height_req <- NULL
samp$surv_2 <- NULL
samp$height = (samp$height_feet*12) + samp$height_in
for(cnt in 1:nrow(samp)){
  if(is.na(samp[cnt,"height"])){
    samp[cnt,"height"] = samp[cnt,"height_in"]
  }
}
samp$height_in = samp$height
samp$height = NULL
samp$height_feet = NULL
samp$height_value_cm = NULL
for(cnt in 1:nrow(samp)){
  if(is.na(samp[cnt,"weight_value_lbs"]) && !is.na(samp[cnt, "weight_value_kg"])){
    samp[cnt,"weight_value_lbs"] = samp[cnt,"weight_value_kg"] * 2.20462
  }
}
samp$weight_req = NULL
samp$weight_value_kg = NULL


samp = pickSample(obd, .25)
save(obd,rseed,obd.backup,samp, file = "survProcessed.rdata")
# We delete the ID-type variables
samp <- samp[,setdiff(names(samp),c(toOmit,textfields))];
samp <- samp[,c(vs(samp,'f'),vs(samp))];
serverHash = digest("ChangeThisInYourCode!", algo = "sha512", ascii = TRUE)
filter_surv2 = subset(samp,s2resp=='Yes')
filter_surv2_kids = subset(samp,s2resp=='Yes' & pat_age < 18)
filter_kids = subset(samp,pat_age < 18)
serverData = list(samp, filter_surv2, filter_surv2_kids, filter_kids)
serverDataDic = c("No filter", "Survey 2 Respondants Only", "Survey 2 Respondants & Pat < 18", "Pat < 18")
serverTitle = "Obesity Survey Sample Data Review."
serverStatement = quote(h4("Enter valid shiny tags and information here. Mostly... you know... Like a link to a data dictionary or something.")))
#DEBUG#save(samp, file = "survSavet0.rdata")
#DEBUG#save(serverData, file = "survSavet1.rdata")
#DEBUG#save(serverData ,serverDataDic , file="survSavet2.rdata")
save(serverData ,serverDataDic , serverHash, serverTitle, serverStatement,file="survSave.rdata")

