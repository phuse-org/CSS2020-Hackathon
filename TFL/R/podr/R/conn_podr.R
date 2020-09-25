#' Make a connection to PHUSE Open Data Repository (PODR)
#'
#' This function will set a PODR connection within the 'podr_connection' 
#' option. Ideally, you shouldn't need to worry about this after it's set. 
#' Using the read_podr function will utilize the connection automatically
#' by default.
#' 
#' You will be prompted within the RStudio interface for your user name and password
#' if you did not provides when setting the connection, 
#' @param username provides user name for PODR database connection given as a character string
#' @param userpwd provides user password for PODR user given as a character string
#' @param driver is database driver defaults to "postgresql"
#' @param server is server name in FQDN or IP address defaults to "podr.phuse.global"
#' @param port is port number defaults to "5432"
#' @param database is database name defaults to "nihpo"
#' @export
#' @importFrom DBI dbConnect
#' @importFrom rstudioapi askForPassword
#' @importFrom odbc odbc
#' @return Nothing - sets the 'podr_connection' option
#'
#' @examples
#'\dontrun{
#'   conn_podr()   # require user to provide name and password interactively
#'   conn_podr('my_username','my_pwd')
#'}
#'
#' @author Hanming Tu
#' @name conn_podr
#
# Code History
#   09/22/2020 (htu) - initial coding based on 
#     https://github.com/phuse-org/CSS2020-Hackathon/blob/master/TFL/R/podr_connections.R
#

conn_podr <- function(username = NULL, 
                      userpwd = NULL,
                      driver = "postgresql", 
                      server = "podr.phuse.global", 
                      port = "5432", 
                      database = "nihpo"           
                      ) {
  # check inputs
  if (is.null(username)) {
    userpwd <-  rstudioapi::askForPassword("Database User Name:"); 
  }
  if (is.null(userpwd)) {
    userpwd <-  rstudioapi::askForPassword("Database User Password:"); 
  }
  
  # Make postgreSQL connection into PODR
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver = driver, 
                        Server = server, 
                        Port = port, 
                        Database = database, 
                        UID = username, 
                        PWD = userpwd, 
                        timeout = 10)
  # Store connection in option
  options('podr_connection'=con)
  con
}
