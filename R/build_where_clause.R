#' Build Where Clause
#'
#' This function builds a where clause for a SQL statement
#'
#' @param  tab_name table name
#' @param  col_name column name
#' @param  col_value column value
#' @param con The connection to PODR. Use connect_podr to establish a connection, or specify a variable
#' containing the proper PostgreSQL connection into PODR yourself
#' @export
#'
#' @return column definition in a data.frame
#'
#' @examples
#'\dontrun{
#' cc <- conn_podr()
#' build_where_clause('css_2020_ae',con=cc)
#'}
#'
#' @author Hanming Tu
#' @name build_where_clause

# Code History
#   10/06/2020 (htu) - initial coding
#
# SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'
#

build_where_clause <- function(tab_name
                               , col_name
                               , col_value
                               , con=getOption('podr_connection')) {
  prg <- "podr::build_where_caluse"
  # 1. check inputs
  # Connection must be set first
  if (is.null(con)) {
    stop(sprintf('%s: Please use `conn_podr` function first', prg))
  }
  if (is_empty(tab_name)) {
    stop(sprintf('%s: %s', prg, 'Please provide tab_name'))
  }
  if (is_empty(col_name)) {
    stop(sprintf('%s: %s', prg, 'Please provide col_name'))
  }
  if (is_empty(col_value)) {
    stop(sprintf('%s: %s', prg, 'Please provide col_value'))
  }

  # 2. get column definition
  cc <- get_table_defs(tab_name=tab_name, con=con)
  nn <- cc[which(cc$column_name==col_name),'is_nullable']
  dt <- cc[which(cc$column_name==col_name),'data_type']

  if (is_empty(dt)) { return('') }

  # 3. define where clause
  # Number: bigint, double precision,
  # Char: text
  qry <- ''
  if (! is_empty(dt) && dt %in% c('text')) {
    qry <- sprintf("WHERE \"%s\" = '%s'", col_name, col_value);
  } else {
    qry <- sprintf("WHERE \"%s\" = %s", col_name, col_value);
  }
  qry
}

