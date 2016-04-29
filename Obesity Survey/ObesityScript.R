#Obesity Script
library(ggplot2)
library(plyr)
library(reshape)
library(vcd)

# some handy functions
source('../ciRd.R');
source('obesitySurveyHelpers.R');


#load clean and save 
obd <- read.table("testoutput.csv", header = TRUE, sep = "\t")

# set variables all in one place if practical
textfields <- grep('^other_|ans6_response$|types2_child$',names(obd),v=T);
numfields <- vs(obd,'z',exclude=c('','None','0'));
racenames <- grep('race___',names(obd),val=T);
researchaccept <- grep('research_accept_dec',names(obd),val=T);

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
obd[,racenames] <- sapply(obd[,racenames],binfactor,lev=2,oth='0',simplify=F);
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
obd$surv_2 <- apply(obd[,17:72], 1, surveyResponded)

#possible research checkboxes for depends on.... for me and child
# All column names will be cleaned up in one shot further down, after we are 
# done using them.
#respStr = c("", "0", "Yes") #no survey response, did not check, checked. Using this to these more readable now that the colnames are slightly more readable
# As it turns out, "" is not 'no survey response', "0" is and "" may be an 
# artifact
# So here we can get rid of them...
obd[,researchaccept] <- sapply(obd[,researchaccept],binfactor,lev=2,oth='0',simplify = F);

#converting the logicals back to factors
obd$surv_2 = as.factor(obd$surv_2)
obd$s2resp <- factor(obd$s2resp);

#bmi factor
obd$BMI = cut(obd$pat_bmi_pct, c(0,25,50,85,95,100)
              ,c("Q1","Q2","Normal","Overweight","Obese"));

for(ii in names(obd.backup)) 
  if(isTRUE(all.equal(obd.backup[[ii]],obd[[ii]]))) 
    obd.backup[,ii]<-NULL;

names(obd) <- mapstrings(names(obd),colnamestringmap);
names(obd.backup) <- mapstrings(names(obd.backup),colnamestringmap);

samp = pickSample(obd, .25)
save(obd, obd.backup,samp, file = "survSave.rdata")

