
adadt <- function(indsn
                  , date_var='BMDTC'
                  , subjid="SUBJID"
                  , biodata="bioelec.rds"
                  , biowher=NULL
                  , keepvars=c("ADT","ADTM","ATM")
){
  
  
  if(any(keepvars %in% names(indsn))){
    stop("Warning: One or more of the Treatment Start/End Date variables are already in the input dataset!")
  }
  
  ## read in exposure data 
  bioD <- biodata
  
  # filter EX data if there is selection criteria
  if (length(biowher)>0) {
    bio1 <- bioD %>% filter_(biowher)
  }else{
    bio1 <- bioD
  }
  
  
  bio_f <- bio1 %>%
    mutate(ADT  = date_from_dtc(get(date_var)),
           ADTM = datetime_from_dtc(get(date_var)),
           ATM  = time_from_dtc(get(date_var))
           )
  
  
  # add the derived variables into the indsn dataset
  outdsn <- left_join(indsn, bio_f, by=intersect(colnames(indsn), colnames(bio_f)))
  
  final <- select(outdsn, append(names(indsn),keepvars))
  
  
  return(final)
}

