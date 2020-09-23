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
                           , tabPanel("Create", uiOutput("tabP1"))
                           , tabPanel("View", uiOutput("tabP2"))
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
  ts_content <- reactive({
    validate(
      need(input$username != "", "Please provide Database User Name.")
    )
    validate(
      need(input$userpwd != "", "Please provide Database User Password.")
    )
    req(input$username)
    req(input$userpwd)
    ts <-data.frame(USERNAME=input$username
                    , USERPWD=input$userpwd
                    , DATASET=input$dataset
                    , LIBNAMEF=input$libname
    );
    ts
  })
  
  # -------------------- 1 tabPanel: Show  --------------------------------
  
  lib_list <- list("CDISC Pilot ADaM" = "cdisc_pilot_adam"
                   , "CDISC Pilot SDTM" = "cdisc_pilot_sdtm"
                   , "Janssen Synthetic" = "janssen_synthetic"
                   )
  dataset1  <- reactive ({
    cp <- conn_podr(input$username, input$userpwd);
    rd <- read_podr(input$dataset, libname = input$libname, con =  cp)
    rd 
  })

  output$DT1 <- renderDataTable({
    str(dataset1);
    datatable(dataset1);  
  })
  
  output$tabP1 <- renderUI({
    tabPanel("Show"
             , div(id = "form"
                   , style="display:inline-block"
                   , textInput("username", "Database User Name *", value = "phuse_su67e99huj" )
                   , bsAlert("alert")
                   , textInput("userpwd", "Database User Password *", value = "bGopEaaIQ7uB" )
                   , bsAlert("alert")
                   , textInput("dataset", "Dataset Name",value = 'ae' )
                   , selectInput("libname", "Library Name"
                                 , choices = lib_list
                                 , selected = "cdisc_pilot_adam")
                   
             )
             , submitButton("Show", icon("refresh"))
             , hr()
             , DT::dataTableOutput("DT1")
    )
  })
  
  # -------------------- 2 tabPanel: View  ----------------------------------
  output$tabP2 <- renderUI({
    tabPanel("View"
             , DT::dataTableOutput("DT1")
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

