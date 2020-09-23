adarfstdt <- function(indsn,
                      keepvars = NULL) {
  
  require(dplyr)
  
  names(indsn)=toupper(names(indsn))
  
  if (is.null(keepvars)) stop('ERROR: keepvars must be speicifed.')
  if(any(keepvars %in% names(indsn))){
    stop("Warning: One or more of Analysis Reference Start Date/Datetime are already in the input dataset!")
  }
  # Get the EX domain and include only the first EX.EXSTDTC date records
  exposure <- read_podr("ex", libname="janssen_synthetic")
  ex_ <- exposure %>%
    filter(nchar(EXSTDTC) == 10 & EXDOSE > 0) %>%
    select(USUBJID,  EXSTDTC) %>%
    group_by(USUBJID) %>%
    summarise(EXSTDTC = min(EXSTDTC)) %>%
    ungroup()
  
  # Get the reference start date from DS and EX
  ds <- read_podr("ds", libname="janssen_synthetic")
  if (nrow(filter(ds, toupper(trimws(DSDECOD)) == 'RANDOMIZED')) > 0) {
    final_ <- ds %>%
      filter(toupper(trimws(DSDECOD)) == 'RANDOMIZED') %>%
      select(USUBJID, DSSTDTC) %>%
      distinct(USUBJID, .keep_all = TRUE) %>%
      full_join(ex_, by = 'USUBJID') %>%
      mutate(ARFSTDTC = if_else(!is.na(EXSTDTC) & EXSTDTC != '', EXSTDTC, DSSTDTC)) %>%
      mutate_if(is.character, function(x) if_else(is.na(x), '', x))
  } else {
    final_ <- ex_ %>% mutate(ARFSTDTC = EXSTDTC)
  }
  
  # Get the numeric form of reference start date
  final_1 <- addtm(final_, dtcvars = 'ARFSTDTC')
  names(final_1)=toupper(names(final_1))
  
  if ('ARFSDTM' %in% names(final_1)) {
    final_1 <- mutate(final_1, ARFSTDT = ARFSDT, ARFSTDTM = ARFSDTM)%>% select(c(USUBJID, toupper(keepvars)))
  } else {
    final_1 <- mutate(final_1, ARFSTDT = ARFSDT) %>% select(c(USUBJID, toupper(keepvars)))
  }
  
  
  if (anyDuplicated(final_1$USUBJID)) {
    stop('ERROR: Many to many merge may occur! -- adarfstdt()')
  } else {
    data <- left_join(indsn, final_1, by = 'USUBJID')
  }
  
  names(data)=tolower(names(data))
  
  return(data)
}

