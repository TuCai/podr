#' Get Table Definition from PODR
#'
#' This function will get column definition for a table in"PODR".
#'
#' @param  tab_name table name
#' @param con The connection to PODR. Use connect_podr to establish a connection, or specify a variable
#' containing the proper PostgreSQL connection into PODR yourself
#' @export
#'
#' @return column definition in a data.frame
#'
#' @examples
#'\dontrun{
#' cc <- conn_podr()
#' get_table_defs('css_2020_ae',con=cc)
#'}
#'
#' @author Hanming Tu
#' @name get_table_defs

# Code History
#   10/06/2020 (htu) - initial coding
#   10/07/2020 (htu) - saved the result to "podr_columns"
#
# SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'
#

get_table_defs <- function(tab_name, con=getOption('podr_connection')) {
  # Connection must be set first
  if (is.null(con)) {
    stop('Please use the `conn_podr` function before using get_tab_defs')
  }
  if (is.null(tab_name)) {
    stop('Please provide tab_name')
  }

  # dataset or query_string is required
  qry <- sprintf("SELECT * FROM information_schema.columns WHERE table_name = '%s'", tab_name);
  rr <- read_podr(query_string = qry, con = con, limit = 1000)
  options("podr_columns"=rr)
  rr
}

