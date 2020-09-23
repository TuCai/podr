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
# pwd <- 'bGopEaaIQ7uB'
# cp <- conn_podr(usr, pwd);
# rd <- read_podr('ae', libname = 'cdisc_pilot_sdtm', con =  cp)
#

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

is_empty <- phuse::is_empty;

header <- dashboardHeader(
  title = "Display PODR Datasets"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tab1",
    menuItem("PODR", icon = icon("cog"),
        menuSubItem("PODR in GitHub", href = 'https://github.com/phuse-org/PODR', newtab = TRUE)
      , menuSubItem("About this Package", href = 'install_phuse_pkg.png', newtab = TRUE)
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
                           , tabPanel("View", uiOutput("tabP3"))
    ))
    #    , bsAlert(inputID = "alert_anchor")
    # , tabItems(
    , fluidRow(
      tabItem("tab1", hr()
              , menuItem("PODR in GitHub", icon=icon('code'), href = 'https://github.com/phuse-org/PODR', newtab = TRUE)
              , menuItem("About this Package", icon=icon('code'), href = 'install_phuse_pkg.png', newtab = TRUE)
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
    con <- getOption('podr_connection')
    if (is.null(con)) {
      validate(
        need(input$username != "", "Please provide Database User Name.")
      )
      validate(
        need(input$userpwd != "", "Please provide Database User Password.")
      )
      req(input$username)
      req(input$userpwd)
      con <- conn_podr(input$username, input$userpwd)
    }
    con 
  })
  
  qry <- "SELECT * FROM information_schema.tables WHERE table_schema = 'public'";
  
  get_tb_names <- reactive ({
    get_conn() %>% 
      read_podr(input$dataset,con = ., libname = input$libname
                , query_string = qry);
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
                   , textInput("userpwd", "Database User Password *", value = "bGopEaaIQ7uB" )
                   , submitButton("Show", icon("refresh"))      
             )
             , hr()
             , h1(get_title())
             , DT::dataTableOutput("DT1")
    )
  })
  
  # -------------------- 2 tabPanel: Show  --------------------------------
  lib_list <- list("CDISC Pilot ADaM" = "cdisc_pilot_adam"
                   , "CDISC Pilot SDTM" = "cdisc_pilot_sdtm"
                   , "Janssen Synthetic" = "janssen_synthetic"
                   )
  
  get_dataset <- reactive ({
    conn_podr(input$username, input$userpwd) %>% 
      read_podr(input$dataset,con = ., libname = input$libname);
  })
  
  get_tb_names <- reactive ({
    get_conn() %>% 
      read_podr(input$dataset,con = ., libname = input$libname);
  })

  get_ds_name <- reactive ({ input$dataset})
  
  get_title <- reactive ({
    paste(toupper(input$dataset), toupper(input$libname), sep = " from ")
  })

  output$DT2 <- renderDataTable({
    # d <- get_dataset();
    d <- get_tb_names();
    if (length(d) < 1 || is.null(d) || is.na(d)) { d <- data.frame() }
    datatable(d);  
  })
  
  output$tabP2 <- renderUI({
    tabPanel("Show"
             , div(id = "form"
                   , style="display:inline-block"
                   , textInput("username", "Database User Name *", value = "phuse_su67e99huj" )
                   , bsAlert("alert")
                   , textInput("userpwd", "Database User Password *", value = "bGopEaaIQ7uB" )
                   , bsAlert("alert")
                   , textInput("dataset", "Dataset Name", value = get_ds_name() )
                   , selectInput("libname", "Library Name"
                                 , choices = lib_list
                                 , selected = "cdisc_pilot_sdtm")
                   , submitButton("Show", icon("refresh"))      
             )
             , hr()
             , h1(get_title())
             , DT::dataTableOutput("DT2")
    )
  })
  
  # -------------------- 3 tabPanel: View  ----------------------------------
  output$tabP3 <- renderUI({
    tabPanel("View"
             , DT::dataTableOutput("DT2")
             , hr()
    )
  })
  
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

