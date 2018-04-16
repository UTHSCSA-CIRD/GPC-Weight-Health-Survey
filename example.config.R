.workenv <- list();
#' This first data file is created by `ObesityScript.R` from the raw CSV file that
#' you will have received. If you want to bypass running `ObesityScript.R` each
#' time, save the `survProcessed.rdata` file it creates someplace on your
#' computer, figure out what the correct path to that file is from R's point 
#' of view (tab-completion in RStudio is your friend in this regard) and then
#' set the variable below to that path (ending in the name of the file of course).
s01_preppaper.datafile <- '\\Users\\YourName\\Desktop\\GPC\\survProcessed.rdata';
#' As above, except this is a second-stage file created by `prepPaper01.R` using
#' the above data-file (i.e. it does not directly access the original CSV file).
s02_obesitymodels.datafile <- '\\Users\\YourName\\Desktop\\GPC\\obesityPaper01.rdata';

#' It is strongly recommended to save all data outside the directory
#' containing your scripts!!!!
#' 
