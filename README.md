# Explore PHUSE Open Data Repository (PODR) Datasets
This package contains functions and R Shiny app to allow you easily connect 
to PODR database and explore datasets related to CSS 2020 Hackathon.
To find out more about PODR, please go to [PODR Github](https://github.com/phuse-org/PODR).

# How to Install this package

Please follow this instruction:

    install.packages("devtools")
    library(devtools)
    install_github(”TuCai/podr")

# How to use it

Once you install it, you can start app by issuing the below command in RStudio:  

     library(podr)
     start_app()


# Code History
## Version 0.0.3 ()
* Functions
** Added *get_table_names* function to get a list of PODR table names
** Added *get_table_defs* function to get table definition
** Changed *conn_podr* to use open source ODBC driver from *RPostgres* package 
instead of RStudio Pro drive from *odbc* package
* User Infterface
** Added *ReadMe* tab to show ReadMe file for a data set in user interface (UI)
** Added *Table* tab to display table's column definition in UI
* Included all the dependent packages through *packrat* package

## Version 0.0.2 (09/30/2020)
* Added R Shiny app _01_podr_ for easily logging into PODR database and explore datasets
** Added *Login* tab to log into database
** Added *Show* tab to show records from a data set
* Functions
** Added *echo_msg* to display message
** Added *is_emnpty* to check variable NULL, NA, legnth
** Added *resolve* to get absolute path
** Added *start_app* to start this application
** Modified *conn_podr* 
** Modified *read_podr* functions to be used with the app
* Added _renv_ and _vignettes_

## Version 0.0.1 (09/22/2020)
* Added *conn_podr* function to connect to PODR database
* Added *read_podr* function to read PODR data sets
