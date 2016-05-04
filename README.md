# GPC-Weight-Health-Survey
Instructions: to prepare the data, do the following:

1. make sure all the *_dd.csv, *_survey.csv, and *.db files are in the same directory as the python scripts and make sure all files from the same site have the same prefix
2. `python ddcheck.py` in that directory
3. `python svcombine.py` in that directory
4. copy testoutput.csv to the "Obesity Survey" directory
5. Open an R console and `source ObesityScript.R`
6. Now you can run the various other visualization R scripts (or knit them) as well as `shiny::runApp()`
