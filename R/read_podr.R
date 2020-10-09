#' Read a dataset from PODR
#'
#' This function will properly query PODR for datasets. There are three "libraries" available to read from:
#' - CDISC Pilot ADaM data (PHUSE TDF Project 2019 cut)
#' - CDISC Pilot SDTM data (PHUSE TDF Project 2019 cut)
#' - Janssen Synthetic SDTM data
#'
#' Read data by specifying your desired dataset as a character string, then
#' specify the library you'd like to read from. The libnames are specified as
#' - cdisc_pilot_adam
#' - cdisc_pilot_sdtm
#' - janssen_synthetic
#'
#'
#' @param dataset Dataset name, specified as a character string
#' @param libname Library name, one of cdisc_pilot_adam, cdisc_pilot_sdtm, or janssen_synthetic
#' @param tabname Full table name, i.e., the combination of libname and dataset
#' @param con The connection to PODR. Use connect_podr to establish a connection, or specify a variable
#' containing the proper PostgreSQL connection into PODR yourself
#' @param  query_string provides full SQL statement
#' @param  limit to select specified number of records; defaults to 100
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr mutate_at
#' @importFrom assertthat assert_that
#' @import magrittr
#'
#' @return The desired dataset as a data.frame
#'
#' @examples
#'\dontrun{
#' conn_podr()
#' read_podr('adae', libname='cdisc_pilot_adam')
#' read_podr('ae', libname='cdisc_pilot_sdtm')
#' read_podr('ae', libname='janssen_synthetic')
#'}
#'
#' @author Hanming Tu
#' @name read_podr

# Code History
#   09/22/2020 (htu) - initial coding based on
#     https://github.com/phuse-org/CSS2020-Hackathon/blob/master/TFL/R/podr_connections.R
#   10/01/2020 (htu) - added tabname to select from full table name
#   10/06/2020 (htu) - added limit parameter and limit for query_string as well
# SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'
#

read_podr <- function(dataset=NULL,
                      libname=NULL,
                      tabname=NULL,
                      con=getOption('podr_connection'),
                      query_string = NULL,
                      limit = 100
                      ) {
  prg <- "podr::read_podr";

  #  require('magrittr')
  is_empty <- podr::is_empty;

  # Connection must be set first
  if (is.null(con)) {
    stop('Please use the `conn_podr` function before using read_podr.')
  }

  # dataset or query_string is required
  if (is_empty(dataset) && is_empty(tabname) && is_empty(query_string)) {
    stop('Please provide (dataset name and libname) or tabname or query_string.')
  }
  # Make sure the dataset name is a character string
  if (! is_empty(dataset)) {
    assertthat::assert_that(is.character(dataset))
  }

  # Build the query string using the dataset name
  if (is_empty(query_string)) {
    if (is_empty(tabname)) {
       if (is_empty(libname)) {
         query_string <- sprintf('select * from public.%s limit %d', dataset, limit)
       } else {
         query_string <- sprintf('select * from public.%s_%s limit %d', libname, dataset, limit)
       }
    } else {
      query_string <- sprintf('select * from public.%s limit %s', tabname, limit)
    }
  } else {
    query_string <- sprintf('%s limit %s', query_string, limit)
  }

  str(paste(prg, query_string, sep = ': '));

  # make the query
  out <- DBI::dbGetQuery(con,query_string) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with('DT')), lubridate::as_date)
  # str(out)
  out
}

