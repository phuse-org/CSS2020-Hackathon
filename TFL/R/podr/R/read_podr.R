#' Read a dataset from PODR
#' 
#' This function will properly query PODR for datasets. There are three "libraries" available to read from:
#' - CDISC Pilot ADaM data (PHUSE TDF Project 2019 cut)
#' - CDISC Pilot SDTM data (PHUSE TDF Project 2019 cut)
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
#' @param  query_string provides full SQL statement
#'
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr mutate_at
#' @importFrom assertthat assert_that
#' @import magrittr 
#' 
#' @return The desired dataset as a data.frame
#'
#' @examples
#'\dontrun{
#' conn_podr()
#' read_podr('adae', libname='cdisc_pilot_adam')
#' read_podr('ae', libname='cdisc_pilot_sdtm')
#' read_podr('ae', libname='janssen_synthetic')
#'}
#'
#' @author Hanming Tu
#' @name read_podr

# Code History
#   09/22/2020 (htu) - initial coding based on 
#     https://github.com/phuse-org/CSS2020-Hackathon/blob/master/TFL/R/podr_connections.R
# SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'
#

read_podr <- function(dataset, 
                      libname=c('cdisc_pilot_adam', 'cdisc_pilot_sdtm', 'janssen_synthetic'), 
                      con=getOption('podr_connection'),
                      query_string = NULL
                      ) {
  
  #  require('magrittr')
  
  # Connection must be set first
  if (is.null(con)) {
    stop('Please use the `conn_podr` function before using read_podr.')
  }
  
  # dataset or query_string is required
  if (is.null(dataset) && is.null(query_string)) {
    stop('Please provide dataset name or query_string.')
  }
  # Make sure the dataset name is a character string
  if (! is.null(dataset)) {
    assertthat::assert_that(is.character(dataset))
  }
  
  # Establish the dataset prefix for files in PODR
  # libname <- match.arg(libname)
  
  # Dictionary of libraries to read from PODR
  libs <- c('cdisc_pilot_adam'='virtual_css_2020_adam_', 
            'cdisc_pilot_sdtm'='virtual_css_2020_sdtm_', 
            'janssen_synthetic'='virtual_css_2020_synth_')
  
  # Build the query string using the dataset name
  if (is.null(query_string)) {
    query_string <- sprintf('select * from public.%s_%s', libname, dataset)
  }
  
  # make the query
  out <- DBI::dbGetQuery(con,query_string) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with('DT')), lubridate::as_date)
  out
} 

