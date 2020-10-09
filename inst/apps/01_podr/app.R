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
# qry <- "SELECT * FROM information_schema.tables WHERE table_schema = 'public'"
# cc <- get_table_names(con = cp, query_string = qry, lib_sel = 'readme' )
# tb <- get_table_names(con=cp)
# rd <- read_podr('ae', libname = 'virtual_css_2020_sdtm', con =  cp)
# conn_podr(usr, pwd) %>% read_podr('ae', libname = 'virtual_css_2020_sdtm', con = .)
#
# tbs <- conn_podr(usr, pwd) %>% read_podr('ae', libname = 'cdisc_pilot_sdtm', con = ., query_string = qry)
# bGopEaaIQ7uB

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
library(RPostgres)
library(assertthat)

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

v <- list();  # create a global variable for storing static key/value paire

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
      #tags$head(
      #  tags$style(type="text/css","label{ display: table-cell; text-align: right;vertical-align: middle; } .form-group { display: table-row;}")
      #)
      #,
      tags$style(type="text/css",
                 "label{ display: table-cell; text-align: right; vertical-align: middle; } .form-group { display: table-row;}")
    )
    , fluidRow(tabsetPanel(id='tabs'
                           , tabPanel("Login", uiOutput("tabLogin"))
                           , tabPanel("ReadMe", uiOutput("tabReadme"))
                           , tabPanel("Table", uiOutput("tabTable"))
                           , tabPanel("Show", uiOutput("tabShow"))

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

  # -------------------- 1 tabPanel: Login  --------------------------------
  get_conn <- reactive ({
      validate(
        need(input$username != "", "Please provide Database User Name.")
      )
      validate(
        need(input$userpwd != "", "Please provide Database User Password.")
      )
      req(input$username)
      req(input$userpwd)
      conn_podr(username = input$username, userpwd = input$userpwd)
  })

  # v <- reactiveValues(tbs = NULL)

  qry <- "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'";

  get_tb_names <- reactive ({
    cc <- get_table_names(con = get_conn(), query_string = qry )
    options('podr_tables'=cc)
    cc
  })

  output$DT1 <- DT::renderDataTable({
    dd <- get_tb_names();
    if (length(dd) < 1 || is.null(dd) || is.na(dd)) { dd <- data.frame() }
    DT::datatable(dd);
  })

  output$tabLogin <- renderUI({
    tabPanel("Login"
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

  # -------------------- 2 tabPanel: Readme  --------------------------------
  list_libs <- reactive({
    cc <- get_table_names(query_string = qry, lib_sel = 'readme' )
    as.list(sort(cc$table_name))
  })

  output$DT2 <- renderDataTable({
    rr <- data.frame()
    if (is_empty(input$lib_name)) { return(datatable(rr)) }

    dd <- read_podr(dataset = 'xx',tabname=input$lib_name)
    if (length(dd) < 1 || is.null(dd) || is.na(dd)) { dd <- data.frame() }
    datatable(t(dd));
  })

  output$tabReadme <- renderUI({
    tabPanel("Readme"
             , div(id = "form"
                   , style="display:inline-block .form-group { display: table-row;}"
                   , selectInput("lib_name", "Library Name: "
                                 , choices = list_libs(), multiple = FALSE
                                 , selected = input$lib_name)
                   # , submitButton("Show", icon("refresh"))
             )
             , hr()
             , h1(toupper(input$lib_name))
             , DT::dataTableOutput("DT2")
    )
  })

  # -------------------- 3 tabPanel: Table  --------------------------------
  get_tab_list <- reactive({
    # cc <- get_table_names(query_string = qry);
    cc <- getOption("podr_tables")
    c1 <- cc[which(cc$libname==input$lib3),];
    as.list(sort(c1$dataset))
  })

  get_tab_title <- reactive ({
    paste(toupper(input$tab3), toupper(input$lib3), sep = " from ")
  })

  output$DT3 <- renderDataTable({
    lb <- input$lib3;
    ds <- input$tab3;
    rr <- data.frame()
    if (is_empty(lb)) { return(datatable(rr)) }
    if (is_empty(ds)) { return(datatable(rr)) }

    dd <- get_table_names(query_string = qry);
    # str(dd)
    tb <- dd[which(dd$libname==lb & dd$dataset==ds),'table_name']
    rr <- data.frame()

    if (is_empty(tb)) { return(datatable(rr)) }
    cl <- "ordinal_position,column_name,is_nullable,data_type";
    qr <- sprintf("SELECT %s FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '%s'", cl, tb);
    rr <- read_podr(query_string = qr)
    # options("podr_columns"=rr)
    datatable(rr)
  })

  output$tabTable <- renderUI({
    # cc <- get_table_names(query_string = qry)
    cc <- getOption("podr_tables")
    libs <- as.list(unique(sort(cc$libname)))
    tabPanel("Show"
             , div(id = "form"
                   , style="display:inline-block"
                   , selectInput("lib3", "Library Name: ", multiple = FALSE
                                 , choices = libs, selected = input$lib3)
                   , selectInput("tab3", "Dataset Name: ", multiple = FALSE
                                 , choices = get_tab_list()  , selected = input$tab3)
             )
             , hr()
             , h1(get_tab_title())
             , DT::dataTableOutput("DT3")
    )
  })

  # -------------------- 4 tabPanel: Show  --------------------------------
  # get a list of dataset names
  get_ds_list <- reactive({
    # cc <- get_table_names(query_string = qry);
    cc <- getOption("podr_tables")
    c1 <- cc[which(cc$libname==input$lib4),];
    as.list(sort(c1$dataset))
  })

  # get a specific table name for the selected data set
  get_tab_name <- reactive({
    lb <- input$lib4;
    ds <- input$tab4;
    if (is_empty(lb)) { return() }
    if (is_empty(ds)) { return() }

    # dd <- get_table_names(query_string = qry);
    dd <- getOption("podr_tables")
    # str(dd)
    tb <- dd[which(dd$libname==lb & dd$dataset==ds),]
    # str(tb)
    tb[1,'table_name']
  })

  # get a list of column names for the selected table
  get_cols <- reactive({
    tb <- get_tab_name()
    if (is_empty(tb)) { return() }
    rr <- get_table_defs(tb)
    as.list(sort(rr[,'column_name']))
  })

  # get a list of unique values for the selected column
  get_values <- reactive({
    tb <- get_tab_name()
    if (is_empty(tb)) { return(list())}

    cl <- input$col;
    if (is_empty(cl)) { return(list())}
    lmt <- input$lmt;
    rr <- read_podr(tabname=tb, limit = lmt);
    cc <- list()
    if (cl %in% names(rr)) {
      cc <- as.list(unique(sort(rr[,cl])))
    }
    c("__ALL__"='__all__', cc)
  })

  # get title for the displayed table
  get_title <- reactive ({
    paste(toupper(input$tab4), toupper(input$lib4), sep = " from ")
  })

  # get total records in a table
  get_cnt <- reactive({
    tab <- get_tab_name();
    lmt <- input$lmt;
    if (is_empty(tab)) { return(sprintf('(Record Selected: %s/%s)', lmt, NULL)) }
    qr <- sprintf('select count(*) cnt from public.%s', tab)
    cc <- read_podr(query_string = qr)
    sprintf('(Record Selected: %s/%s)', lmt, cc[1,'cnt'])
  })

  output$DT4 <- renderDataTable({
    rr <- data.frame();
    lb <- input$lib4;
    ds <- input$tab4;
    cl <- input$col;
    vl <- input$val;
    cls <- input$cls;
    lmt <- input$lmt
    # str(lb)
    # str(ds)
    if (is_empty(lb)) { return(datatable(rr)) }
    if (is_empty(ds)) { return(datatable(rr)) }
    if (is_empty(lmt)){ lmt <- 100 }
    if (is_empty(cls)){ cls <- '*' } else { cls <- paste0('"', gsub(', ','","', toString(cls)), '"');  }

    tb <- get_tab_name();
    if (is_empty(tb)) { return(datatable(rr))}
    qr <- sprintf("SELECT %s FROM public.%s ", cls, tb )

    if (!is_empty(vl) && vl != '__all__') {
      wr <- build_where_clause(tab_name = tb, col_name = cl, col_value = vl)
      qr <- sprintf('%s %s', qr, wr)
    }

    rr <- read_podr(ds,libname = lb, tabname=tb, query_string = qr, limit = lmt);
    options("podr_values"=rr)
    # str(rr)
    if (isTRUE(input$chk4) && (as.numeric(lmt) < 11) ) { datatable(t(rr)); } else { datatable(rr); }
  })

  output$tabShow <- renderUI({
    # cc <- get_table_names(query_string = qry)
    cc <- getOption("podr_tables")
    libs <- as.list(unique(sort(cc$libname)))
    tabPanel("Show"
             , div(id = "form"
                   # , style="display:inline-block"
                   , style="label{ display: table-cell; text-align: right;vertical-align: middle; } .form-group { display: table-row;}"
                   , selectInput("lib4", "Library Name: "
                                 , choices = libs, multiple = FALSE
                                 , selected = input$lib4)
                   , selectInput("tab4", "Dataset Name: ", multiple = FALSE
                                , choices = get_ds_list()
                                , selected = input$tab4)
                   , selectInput("cls", "Select Columns: ", multiple = TRUE
                                 , choices = get_cols()
                                 , selected = input$cls)
                   , selectInput("col", "Column Name: ", multiple = FALSE
                                 , choices = get_cols()
                                 , selected = input$col)
                   , selectInput("val", "= Value: ", multiple = FALSE
                                 , choices = get_values()
                                 , selected = input$val)
                   , selectInput("lmt", "Record Limit: ", multiple = FALSE
                                 , choices = c(1,5,10,50,100,200,1000,2000)
                                 , selected = input$lmt)
                   , checkboxInput("chk4", "Tranpose", value = input$chk4)
                   # , actionButton("go3", "Show")
             )
             , hr()
             , h1(get_title())
             , h4(get_cnt())
             , DT::dataTableOutput("DT4")
    )
  })

  observeEvent(input$tab4, {
    #    # str(input$tsval);
    #    # str(length(input$tsval));
    # if (length(input$tab4) == 1) {
    #      # runjs('var x = document.getElementById("tsvalnf"); alert("TSVALNF=" + x.value);x.value = "";')
    #      # runjs('var x = document.getElementById("tsvalnf"); x.value = "";')
    updateTextInput(session, "col", value = NA)
    #    }
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

