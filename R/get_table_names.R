#' Get Table Names from PODR
#'
#' This function will get a list of table names from "PODR".
#'
#' @param con The connection to PODR. Use connect_podr to establish a connection, or specify a variable
#' containing the proper PostgreSQL connection into PODR yourself
#' @param  query_string provides full SQL statement
#' @param lib_sel Library name selection pattern
#' @param ds_sel Dataset name selection pattern
#' @importFrom tibble add_column
#' @importFrom stringr str_extract
#' @export
#'
#' @return The selected table names in a data.frame
#'
#' @examples
#'\dontrun{
#' cc <- conn_podr()
#' get_table_names(con=cc)
#' get_table_names(query_string = "select * from information_schema.tables")
#' get_table_names(query_string = "select * from information_schema.tables", lib_sel = "readme")
#'}
#'
#' @author Hanming Tu
#' @name get_table_names

# Code History
#   10/01/2020 (htu) - initial coding
#   10/07/2020 (htu) - saved result to "podr_tables"
#
# SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'
#

get_table_names <- function(con=getOption('podr_connection'),
                            query_string = NULL,
                            lib_sel = NULL,
                            ds_sel = NULL
                      ) {
  # Connection must be set first
  if (is.null(con)) {
    stop('Please use the `conn_podr` function before using get_table_names.')
  }

  # dataset or query_string is required
  if (is.null(query_string)) {
    query_string <- "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'";
  }
  # get_table_names: no visible binding for global variable ‘.’
  # Undefined global functions or variables: .
   cc <- read_podr(con = con, query_string = query_string, limit = 1000 ) %>%
      add_column(libname = gsub('_[[:alpha:]]+$', '\\1', .[,"table_name"], ignore.case = TRUE )) %>%
      add_column(dataset = str_extract(.[,"table_name"], '([[:alpha:]]+)$' ));

  # c1 <- read_podr(con = con, query_string = query_string, limit = 1000 )
  # c2 <- add_column(libname = gsub('_[[:alpha:]]+$', '\\1', c1[,"table_name"], ignore.case = TRUE ))
  # cc <- add_column(dataset = str_extract(c2[,"table_name"], '([[:alpha:]]+)$' ));

  if (! is.null(lib_sel)) { cc <- cc[grepl(lib_sel, cc$libname),];  }
  if (! is.null(ds_sel))  { cc <- cc[grepl(ds_sel, cc$dataset),];   }
  cc[with(cc, order(libname, dataset)),];
  options("podr_tables"=cc)
  cc
}

