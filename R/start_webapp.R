#' Start Web Application
#' @description start web appllication framework.
#' @param n Example number
#' @param pkg  package name
#' @param pt Port number
#' @param lb define the browser- shiny.launch.browser
#' @param ht define the host or ip address
#' @param dm display modes are auto, normal or showcase
#' @param msg_lvl message display level
#' @export
#' @examples
#'   # library(bldsql)
#'   # comment out the interactive sessions
#'   # start_webapp()  # default to "01_webapp"
#' @author Hanming Tu
#' @name start_webapp
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  11/28/2017 (htu) - initial creation
#  02/13/2018 (htu) -
#    1) renamed port to pt, launch.browser to lb, host to ht and
#       display.mode to dm
#    2) change dm = c("auto", "normal", "showcase") to dm = "normal"
#  10/17/2018 (htu) -
#    1) added "04-slider" app
#    2) added msg_lvl input parameter
#
start_webapp <- function (n = 1, pkg = "podr"
  , pt = NULL
  , lb = getOption("shiny.launch.browser",interactive())
  , ht = getOption("shiny.host", "127.0.0.1")
  # , dm = c("auto", "normal", "Normal")
  , dm =  "normal"
  , msg_lvl = NULL
) {
  if (is.null(msg_lvl)) {
    Sys.setenv("g_lvl"=0, "d_lvl"=0)
  } else {
    Sys.setenv("g_lvl"=msg_lvl, "d_lvl"=msg_lvl)
  }
  prg <- "start_webapp";
  apps    <- c("01-webapp","02-dynpanel","03-shinyjs","04-slider");
  if (n > length(apps)) {
    msg <- paste("Available apps: ", paste(apps, sep=','));
    echo_msg(prg,0.1,msg, 1)
    return()
  }
  example <- apps[n];
  examplesDir <- system.file("examples", package = pkg )
  # dir <- shiny:::resolve(examplesDir, example)
  dir <- resolve(examplesDir, example)
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ""
    } else {
      errFun <- stop
      errMsg <- paste("Example", example, "does not exist. ")
    }
    errFun(errMsg, "Valid examples are \""
      , paste(list.files(examplesDir), collapse = "\", \""), "\"")
  } else {
    shiny::runApp(dir
      , port = pt, host = ht, launch.browser = lb, display.mode = dm)
  }
}


