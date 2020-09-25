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
## Version 0.0.2
* Added R Shiny app for easily logging into PODR database and explore datasets
* Modified *conn_podr* and *read_podr* functions to be used with the app
* Added *echo_msg*, *is_emnpty*, *resolve*, and *start_app* functions

## Version 0.0.1
* Added *conn_podr* function to connect to PODR database
* Added *read_podr* function to read PODR data sets
