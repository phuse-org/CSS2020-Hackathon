#' R Shiny app to display data sets in PODR 
#' @description Display data sets in PODR 
##' @param fn a file name or URL pointing to script metadata file
#' @return R shiny code for providing inputs and downloading TS file
#' @export
#' @examples
#'\dontrun{
#'   install.package("podr")
#'   library("podr")
#'   start_app()
#'}
#' @author Hanming Tu
##' @name app
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  09/22/2020 (htu) - initial creation
# usr <- 'phuse_su67e99huj'
# pwd <- 'xxxx'
# cp <- conn_podr(usr, pwd);
# rd <- read_podr('ae', libname = 'cdisc_pilot_sdtm', con =  cp)
# conn_podr(usr, pwd) %>% read_podr('ae', libname = 'cdisc_pilot_sdtm', con = .)
# qry <- "SELECT * FROM information_schema.tables WHERE table_schema = 'public'"
# tbs <- conn_podr(usr, pwd) %>% read_podr('ae', libname = 'cdisc_pilot_sdtm', con = ., query_string = qry)

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(SASxport)
library(Hmisc)
library(rhandsontable)
# library(phuse)
library(DT)
# library(V8)
library(stringr)
library(podr)
library(tibble)

is_empty <- podr::is_empty;

header <- dashboardHeader(
  title = "Display PODR Datasets"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tab1",
    menuItem("PODR", icon = icon("cog"),
        menuSubItem("PODR in GitHub", href = 'https://github.com/phuse-org/PODR', newtab = TRUE)
      , menuSubItem("About this Package", href = 'https://github.com/TuCai/podr', newtab = TRUE)
      , menuSubItem('Source Code',href='https://github.com/TuCai/podr/blob/master/inst/apps/01_podr/app.R', newtab = TRUE)
    )
    , style = "background-color: blue; "
  )
)
jsHideSB <- 'shinyjs.hideSidebar = function(params) {
      $("body").addClass("sidebar-collapse");
      $(window).trigger("resize"); }'
jsShowSB <- 'shinyjs.showSidebar = function(params) {
      $("body").removeClass("sidebar-collapse");
      $(window).trigger("resize"); }'

ui <- dashboardPage(
  # dashboardHeader(),
  # dashboardSidebar(),
  header,
  sidebar,
  dashboardBody(
    useShinyjs()
    , extendShinyjs(text = jsHideSB, functions = c("hideSidebar"))
    , extendShinyjs(text = jsShowSB, functions = c("showSidebar"))
    , bsButton("showpanel", "Show/Hide sidebar",icon = icon("toggle-off"),
               type = "toggle",style = "info", value = TRUE)
    , tags$head(
      tags$style(type="text/css",
                 "label{ display: table-cell; text-align: right; vertical-align: middle; }
         .form-group { display: table-col;}")
    )
    , fluidRow(tabsetPanel(id='tabs'
                           , tabPanel("Login", uiOutput("tabP1"))
                           , tabPanel("Show", uiOutput("tabP2"))
    ))
    #    , bsAlert(inputID = "alert_anchor")
    # , tabItems(
    , fluidRow(
      tabItem("tab1", hr()
              , menuItem("PODR in GitHub", icon=icon('code'), href = 'https://github.com/phuse-org/PODR', newtab = TRUE)
              , menuItem("About this Package", icon=icon('code'), href = 'https://github.com/TuCai/podr', newtab = TRUE)
              , menuItem('Source Code',icon=icon('code'), href='https://github.com/TuCai/podr/blob/master/inst/apps/01_podr/app.R', newtab = TRUE)
              , hr()
      )
    )
    , tags$footer("PHUSE DVOST Project"
                  , align = "center"
                  , style = "position:dynamic;
              bottom:0;
              width:100%;
              height:30px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: blue;
              z-index: 1000;"
    )
  )
)

server <- function(input, output, session) {
  
  # -------------------- 1 tabPanel: SetDB  --------------------------------
  get_conn <- reactive ({
      validate(
        need(input$username != "", "Please provide Database User Name.")
      )
      validate(
        need(input$userpwd != "", "Please provide Database User Password.")
      )
      req(input$username)
      req(input$userpwd)
      conn_podr(input$username, input$userpwd)
  })
  
  qry <- "SELECT table_name
            FROM information_schema.tables WHERE table_schema = 'public'";
  
  get_tb_names <- reactive ({
    str(qry)
    cc <- get_conn() %>% 
      read_podr("xx",  libname = "yy", con = ., query_string = qry ) %>%
      add_column(libname = gsub('_[[:alpha:]]+$', '\\1', .[,"table_name"], ignore.case = TRUE )) %>%
      add_column(dataset = str_extract(.[,"table_name"], '([[:alpha:]]+)$' ));
    cc[with(cc, order(libname, dataset)),]
  })
  
  output$DT1 <- renderDataTable({
    d <- get_tb_names();
    if (length(d) < 1 || is.null(d) || is.na(d)) { d <- data.frame() }
    datatable(d);  
  })
  
  output$tabP1 <- renderUI({
    tabPanel("SetDB"
             , div(id = "form"
                   , style="display:inline-block"
                   , textInput("username", "Database User Name *", value = "phuse_su67e99huj" )
                   , bsAlert("alert")
                   , passwordInput("userpwd", "Database User Password *" )
                   , submitButton("Show", icon("refresh"))      
             )
             , hr()
             , h1("Public Tables")
             , DT::dataTableOutput("DT1")
    )
  })
  
  # -------------------- 2 tabPanel: Show  --------------------------------
  get_cc <- reactive({ c <- get_tb_names();
    with(c,aggregate(libname,by=list(libname=libname, dataset=dataset), max));
    })
  # lib_list <- reactive({ cc <- get_cc(); unique(cc$libname); 
  #  cc[with(cc, order(libname)),]
  # })
   lib_list <- list("CDISC Pilot ADaM" = "virtual_css_2020_adam"
                   , "CDISC Pilot SDTM" = "virtual_css_2020_sdtm"
                   , "Janssen Synthetic" = "virtual_css_2020_synth"
   )
  
  ds_list <- reactive({
    cc <- get_cc();
    str(cc)
    c1 <- cc[which(cc$libname==input$libname),]; c2 <- as.list(sort(c1$dataset))
    c2
  })
  
  get_lib_name <- reactive ({ 
    libname <- 'cdisc_pilot_sdtm'
    if (! is.null(input$libname)) {libname <- input$libname}
    libname
  })
  
  get_dataset <- reactive ({
    conn_podr(input$username, input$userpwd) %>% 
      read_podr(input$dataset,con = ., libname = input$libname);
  })
  
  get_title <- reactive ({
    paste(toupper(input$dataset), toupper(input$libname), sep = " from ")
  })

  output$DT2 <- renderDataTable({
    d <- get_dataset();
    if (length(d) < 1 || is.null(d) || is.na(d)) { d <- data.frame() }
    datatable(d);  
  })
  
  output$tabP2 <- renderUI({
    tabPanel("Show"
             , div(id = "form"
                   , style="display:inline-block"
                   # , textInput("dataset", "Dataset Name", value = get_ds_name() )
                   , selectInput("libname", "Library Name: "
                                 , choices = lib_list, multiple = FALSE
                                 , selected = get_lib_name())
                   , selectInput("dataset", "Dataset Name: ", multiple = FALSE
                                 , choices = ds_list()
                                 , selected = input$dataset)
                   , submitButton("Show", icon("refresh"))      
             )
             , hr()
             , h1(get_title())
             , DT::dataTableOutput("DT2")
    )
  })
  
  # --------------------  Obverses ------------------------------------------

  observe({
    if(input$showpanel == TRUE) {
      js$showSidebar()
    }
    else {
      js$hideSidebar()
    }
  })
  
#  observeEvent(input$studyid, {
#    if (input$studyid == "")
#      hide("downloadData")
#    else
#      show("downloadData")
#  })
}

shinyApp(ui, server)

