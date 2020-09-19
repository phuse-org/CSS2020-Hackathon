#' Make a connection to PODR
#'
#' This function will set a PODR connection within the 'podr_connection' 
#' option. Ideally, you shouldn't need to worry about this after it's set. 
#' Using the read_podr function will utilize the connection automatically
#' by default.
#' 
#' You will be prompted within the RStudio interface for your password
#' when setting the connection, 
#' @param username Username provided for PODR given as a character string
#'
#' @return Nothing - sets the 'podr_connection' option
#'
#' @examples
#' connect_podr('my_username')
connect_podr <- function(username) {
  # Make postgreSQL connection into PODR
  con <- DBI::dbConnect(odbc::odbc(), 
                          Driver="postgresql", 
                          Server = "podr.phuse.global", 
                          Port = "5432", 
                          Database = "nihpo", 
                          UID = "phuse_p7r82p0vul", 
                          PWD = rstudioapi::askForPassword("Database password:"), 
                          timeout = 10)
  # Store connection in option
  options('podr_connection'=con)
}

#' Read a dataset from PODR
#' 
#' This function will properly query PODR for datasets used within the 
#' PHUSE 2020 Hackathon. There are three "libraries" available to read from:
#' - the CDISC Pilot ADaM data (PHUSE TDF Project 2019 cut)
#' - the CDISC Pilot SDTM data (PHUSE TDF Project 2019 cut)
#' - Janssen Synthetic SDTM data
#' 
#' Read data by specifying your desired dataset as a character string, then
#' specify the library you'd like to read from. The libnames are specified as
#' - cdisc_pilot_adam
#' - cdisc_pilot_sdtm
#' - janssen_synthetic
#'
#'
#' @param dataset Dataset name, specified as a character string
#' @param libname Library name, one of cdisc_pilot_adam, cdisc_pilot_sdtm, or janssen_synthetic
#' @param con The connection to PODR. Use connect_podr to establish a connection, or specify a variable
#' containing the proper PostgreSQL connection into PODR yourself
#'
#' @return The desired dataset as a data.frame
#'
#' @examples
#' 
#' read_podr('adae', libname='cdisc_pilot_adam')
#' read_podr('ae', libname='cdisc_pilot_sdtm')
#' read_podr('ae', libname='janssen_synthetic')
#' 
read_podr <- function(dataset, 
                      libname=c('cdisc_pilot_adam', 'cdisc_pilot_sdtm', 'janssen_synthetic'), 
                      con=getOption('podr_connection')) {
  
  require('magrittr')
  
  # Make sure the dataset name is a character string
  assertthat::assert_that(is.character(dataset))
  
  # Connection must be set first
  if (is.null(con)) {
    stop('Please use the `connect_podr` function before using read_podr.')
  }
                        
  # Establish the dataset prefix for files in PODR
  libname <- match.arg(libname)
  
  # Dictionary of libraries to read from PODR
  libs <- c('cdisc_pilot_adam'='virtual_css_2020_adam_', 
            'cdisc_pilot_sdtm'='virtual_css_2020_sdtm_', 
            'janssen_synthetic'='virtual_css_2020_synth_')
  
  # Build the query string using the dataset name
  query_string <- sprintf('select * from public.%s%s', libs[libname], dataset)
  
  # make the query
  out <- DBI::dbGetQuery(con,query_string) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with('DT')), lubridate::as_date)
  out
}






