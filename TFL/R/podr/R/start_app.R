#' Start R Shiny app 
#' @description start R Shiny apps included in this package. 
#' @param app_name app or script name
#' @param n app number
#' @param pkg  package name
#' @param pt Port number
#' @param lb define the browser- shiny.launch.browser
#' @param ht define the host or ip address
#' @param dm display modes are auto, normal or showcase
#' @param msg_lvl message level
#' @param loc location of the scirpt: local|github; default to 'local'
#' @export
#' @examples
#'\dontrun{
#'   library(genTS)
#'   start_appe()  # default to "02_display"
#'   start_app(1)  # start "01_html"
#'}
#' @author Hanming Tu
#' @name start_app
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  06/10/2019 (htu) - initial creation based on start_phuse
#  06/11/2019 (htu) - added loc parameter so that you can use local repository
#
#
# start_app <- function (n = 2)
#{
#  app_name <- paste0(sprintf("%02d", n), "_display")
#  start_app(app_name)
# }
#
# rm(list=ls())

start_app <- function (app_name = "01_podr", n = 1, pkg = "podr"
                        , pt = NULL
                        , lb = getOption("shiny.launch.browser",interactive())
                        , ht = getOption("shiny.host", "127.0.0.1")
                        #, dm = c("auto", "normal", "Normal")
                        , dm =  "normal"
                        , msg_lvl = NULL
                       , loc = 'local'
) {
  prg <- "start_app"; echo_msg(prg,0.0,'Started', 1)
  
  
  if (is.null(msg_lvl)) {
    Sys.setenv("g_lvl"=0, "d_lvl"=0)
  } else {
    Sys.setenv("g_lvl"=msg_lvl, "d_lvl"=msg_lvl)
  }
  appDir <- system.file("apps", package = pkg )
  apps    <- list.files(appDir);
  if (is.null(app_name)) {  app <- apps[n] } else { app <- app_name }
  
  echo_msg(prg,0.1, paste("App dir =", appDir), 1)
  
  # dir <- shiny:::resolve(appDir, app)
  dir <- resolve(appDir, app)
  echo_msg(prg,0.2, paste("Resolved dir =", dir), 1)
  
  
  if (is.null(dir)) {
    echo_msg(prg,1.1,paste('could not find a dir for ',app), 1)
    if (is.na(app)) {
      errFun <- message
      errMsg <- ""
    } else {
      errFun <- stop
      errMsg <- paste("App", app, "does not exist. ")
    }
    errFun(errMsg, "Valid apps are \""
           , paste(list.files(appDir), collapse = "\", \""), "\"")
  } else {
    echo_msg(prg,1.2,paste('Start app from ',dir), 1)
    shiny::runApp(dir
                  , port = pt, host = ht, launch.browser = lb, display.mode = dm)
  }
}

