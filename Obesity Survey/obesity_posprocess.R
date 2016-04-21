# project settings

inputfile = 'obesity_survey_v0.3.csv';
na.strings = c('',' ');
dataname = 'obd'; ddname = 'ddob';
# columns to transform
makeChar = c('research_types2_child','q6_ans6_response','q7_ans6_response','other_race','other_insurance','other_language');
makeNum = c('patient_num','weight_value_lbs')

# data dictionary generator
ddgen <- function(df,...){
  out <- data.frame(t(sapply(df,function(xx) c(class(xx),length(levels(xx))))));
  out$col<-rownames(out); rownames(out)<-NULL;
  out<-out[,c(3,1,2)];
  names(out) <- c('column','type','levels');
  out;
}

# coerce data types
coercecols <- function(df,makeChar,makeNum){
  cn <- colnames(df);
  if(!missing(makeChar)){
    makeChar <- intersect(makeChar,cn);
    if(length(makeChar)>0){
      df[,makeChar]<-
        sapply(df[,makeChar],as.character);
    }
  }
  if(!missing(makeNum)){
    makeNum <- intersect(makeNum,cn);
    if(length(makeNum)>0){
      df[,makeNum]<-
        sapply(df[,makeNum],function(xx) as.numeric(as.character(xx)));
    }
  }
  df;
}


# read in data
.GlobalEnv[[dataname]] <- read.delim(inputfile,header=T,na.strings=na.strings);
.GlobalEnv[[dataname]] <- coercecols(.GlobalEnv[[dataname]],makeChar,makeNum);
.GlobalEnv[[ddname]] <- ddgen(.GlobalEnv[[dataname]]);
