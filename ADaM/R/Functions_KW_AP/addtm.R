
addtm <- function(.data,
                  dtcvars  = 'all',
                  adtcvars = NA_character_,
                  keepdtc  = T,
                  keeptm   = F) {
  
  fun.name <- 'addtm'
  cat(paste0('-- Function ', fun.name, '() begins..\n'))
  
  require(dplyr)
  
  names(.data)=tolower(names(.data))
  dtcvars  <- tolower(dtcvars)
  adtcvars <- tolower(adtcvars)
  
  if (is.null(dtcvars)) stop('ERROR: dtcvars cannot be empty.')
  if (length(adtcvars)>1) stop('ERROR: adtcvars can only include one variable.')
  
  data <- .data
  dtcvars_ori <- dtcvars
  
  for (suf in c('dtc', 'stdt', 'endt', 'stdtm', 'endtm')) {
    
    if (length(grep(paste0(suf, '$'), dtcvars_ori, ignore.case = T)) == 0 &
        length(grep(paste0(suf, '$'), adtcvars, ignore.case = T)) == 0 &
        tolower(dtcvars_ori[1]) != 'all')
      next
    
    dtcvars <- dtcvars_ori
    
    # Polymorphic argument
    if (tolower(dtcvars_ori[1]) == 'all') {
      dtcvars <- names(.data)[grep(paste0(suf, '$'), names(.data), ignore.case = T)]
    } else {
      dtcvars <- dtcvars[grep(paste0(suf, '$'), dtcvars, ignore.case = T)]
    }
    
    # If variables are passed to both adtcvars and dtcvars, remove it from dtcvars
    dtcvars <- setdiff(dtcvars, adtcvars)
    
    # Create prefix xxST- and xxEN- for xxSDT and xxEDT, respectively to support IG naming convention.
    prefix_dtc <- gsub(paste0('(.*(|st|en))', sub('.*(dtc|dt|dtm)$', '\\1', suf, ignore.case = T), '$'), '\\1',
                       dtcvars, ignore.case = TRUE)
    prefix_dtc <- if_else(grepl('(st|en)dtc$', dtcvars, ignore.case = T),
                          substr(prefix_dtc, 1, nchar(prefix_dtc) - 1), prefix_dtc)
    
    if (length(grep(paste0(suf, '$'), adtcvars)) > 0) {
      # Create prefix AST- and AEN- for ASTDT and AENDT, respectively
      prefix_adtc <- if_else(grepl(paste0('(st|en)', sub('.*(dtc|dt|dtm)$', '\\1', suf, ignore.case = T), '$'),
                                   adtcvars, ignore.case = TRUE, perl = T),
                             gsub(paste0('(.*)(st|en)', sub('.*(dtc|dt|dtm)$', '\\1', suf, ignore.case = T), '$'),
                                  'a\\2', adtcvars, ignore.case = TRUE, perl = T),
                             'a')
      
      varnames <- c(dtcvars, adtcvars)
      prefixes <- c(prefix_dtc, prefix_adtc)
    } else {
      varnames <- c(dtcvars)
      prefixes <- c(prefix_dtc)
    }
    
    for (i in seq_along(varnames)) {
      # Take advantage of the flexible parsing of lubridate::parse_date_time()
      dtm <- lubridate::parse_date_time(data[[varnames[i]]], c('ymd HMS', 'ymd HM', 'ymd'))
      data[paste0(prefixes[i], 'dtm')] <- dtm
      data[paste0(prefixes[i], 'dt')] <- as.Date(dtm)
      data[paste0(prefixes[i], 'tm')] <- hms::parse_hms(strftime(dtm, '%H:%M:%S', tz = 'UTC'))
      
      # Set date variables null if the date/time variables have an insufficient length
      # Flexible parsing automatically impute the missing date/time parts. This imputation may not be wanted.
      data <- data %>%
        mutate(!!paste0(prefixes[i], 'dtm') :=
                 if_else(nchar(data[[varnames[i]]]) >= 16, data[[paste0(prefixes[i], 'dtm')]], NULL),
               
               !!paste0(prefixes[i], 'tm')  :=
                 case_when(nchar(data[[varnames[i]]]) >= 16 ~ data[[paste0(prefixes[i], 'tm')]],
                           
                           nchar(data[[varnames[i]]]) %in% 13:15 &
                             grepl('T', data[[varnames[i]]]) ~ data[[paste0(prefixes[i], 'tm')]],
                           
                           TRUE ~ NA_real_
                 )
        )
      
      # Remove variables that have only null value in the data
      if (keeptm == F) {
        if (!(FALSE %in% is.na(data[paste0(prefixes[i], 'dtm')]))) data <- select(data, -!!paste0(prefixes[i], 'dtm'))
        if (!(FALSE %in% is.na(data[paste0(prefixes[i], 'tm' )]))) data <- select(data, -!!paste0(prefixes[i], 'tm' ))
      }
    }
    
    if (!keepdtc) data <- dplyr::select(data, -all_of(varnames))
  }
  
  names(data)=tolower(names(data))
  cat(paste0('-- Function ', fun.name, '() ends..\n'))
  
  return(data)
}

