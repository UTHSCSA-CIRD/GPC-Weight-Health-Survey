#' ---
#' title: "Obesity Diet Exploration and Plot Demos"
#' author: "Laura Manuel and Alex Bokov"
#' date: "May 2nd, 2016"
#' ---
source('../ciRd.R');
source('obesitySurveyHelpers.R');
install.packages(c('party','rpart','psy'));
load('survSave.rdata');
#'Some plots by race
runByRaceVariable(samp, "possible_research","Interested in Being Contacted for Research")
runByRaceVariable(samp, "survey_contact_method", "Survey Contact Method")
runByRaceVariable(samp, "cancer_anytype_self", "Cancer - Self")
runByRaceVariable(samp, "elev_bs_diabetes", "Elevated Blood Sugar/ Diabetes - Child")
runByRaceVariable(samp, "cancer_anytype", "Cancer- Child")
runByRaceVariable(samp, "sex", "Gender")
runByRaceVariable(samp, "latino_origin", "Latino Origin")
runByRaceVariable(samp, "income", "Income")
runByRaceVariable(samp, "insurance", "Insurance by Race")
runGGPLOT(samp, "income", "insurance", xlab ="Income", ylab = "Insurance", omitNA_X = FALSE)
#'lets play with some mosaic plots...
ggMosaicPlot(samp$site, samp$possible_research)
mosaic(structable(site ~ surv_2, data = samp), shade = TRUE, legend = TRUE)

#'categorical trees-- just playing with these for now. 
#'This was the most interesting one I found
library("party")
fitp <- ctree(income ~ site + Race, data = samp)
fitp;
plot(fitp);
#'Not as much a fan of this one, maybe Alex can work out what to do with it. 
library("rpart")
fitr <- rpart(income ~ site + Race, method = "class", data = samp)
printcp(fitr);
plotcp(fitr);
summary(fitr);
plot(fitr, uniform=TRUE)
text(fitr, use.n=TRUE, all=TRUE, cex=.8)

#'Plots by willingness to participate - most of the factors don't seem to stand out
runByWilling2P(samp, "site")
runByWilling2P(samp, "income", "Income by Willingness to Participate")
runByWilling2P(samp, "survey_contact_method") #People have a hard time saying no on the phone? 

#'Plot spherical PCA (a.k.a. constellation) plot 
#'of people who responded to survey-2
library(psy);
pcawrap(subset(samp,s2resp=='Yes')
        ,nbsphere=1 # this argument tells sphpca to draw just one sphere
        ,back=T     # ...which is translucent
);
#'Each point is a column from the dataset. The closer they are together the 
#'stronger their positive correlation, and the closer they are to 180 degrees,
#'the stronger their negative correlation. Variables 90 degrees to each other are 
#'uncorrelated (independent). 
#'
#'Now let's try rotating the sphere. First, we rotate it 45 degrees in the 
#'vertical plane:
pcawrap(subset(samp,s2resp=='Yes')
        ,nbsphere=1 # this argument tells sphpca to draw just one sphere
        ,back=T     # ...which is translucent
        ,v=45
);
#'Now the horizontal.
pcawrap(subset(samp,s2resp=='Yes')
        ,nbsphere=1 # this argument tells sphpca to draw just one sphere
        ,back=T     # ...which is translucent
        ,h=45
);
#'Now the plane facing the observer
pcawrap(subset(samp,s2resp=='Yes')
        ,nbsphere=1 # this argument tells sphpca to draw just one sphere
        ,back=T     # ...which is translucent
        ,f=45
);
#'Now all three
pcawrap(subset(samp,s2resp=='Yes')
        ,nbsphere=1 # this argument tells sphpca to draw just one sphere
        ,back=T     # ...which is translucent
        ,v=45,h=45,f=45
);
#'Negative numbers of degrees are permitted.
#'
#'By default the `pcawrap()` function does a spherical PCA plot
#'But we can make it do a focused PCA plot.
pcawrap(subset(samp,s2resp=='Yes')
        ,respvar = 'possible_research' # what the response variable should be
        ,pca='f'                       # argument specifying a focused PCA plot
        ,contraction='Yes'             # supposedly makes it easier to 
                                       # distinguish large numbers of variables
);
#'The v/h/f arguments are not valid when using pca='fpca'
#'The closer a point is to the center, the more closely the column it 
#'represents is correlated with the response variable in the center. Green 
#'indicates positive correlations and yellow, inverse correlations.
#'the response variable.
#'
#'One of the things `pcawrap()` does is send the input `data.frame` through 
#'another function we developed for this type of project called `nprep()`.
#'The latter coerces to numeric, centers, scales, and imputes data. This makes 
#'it suitable not only for `sphpca()` and `fpca()` but also for good old 
#'`heatmap()`. Let's try clustering respondents and variables...
nsamp <- nprep(samp);
heatmap(as.matrix(nsamp));
#'Now let's try doing the heatmap of just the correlation matrix
heatmap(cor(nsamp),symm=T);
