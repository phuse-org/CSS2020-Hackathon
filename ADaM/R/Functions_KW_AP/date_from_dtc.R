# Functions needed for the adadt function

date_from_dtc <- function(dtc) {
  as.Date(lubridate::parse_date_time(dtc, "Ymd HMS", truncated = 3), origin = "1960-01-01")
}


# time_from_dtc <- function(dtc) {
#   time <- stringr::word(dtc, 2, sep = "T")
#   if (nchar(time) < 5 | is.na(time)) {
#     return(NA)
#   }
#   if (nchar(time) == 5) time <- paste0(time, ":00")
#   (hms::as_hms(time))
# }


time_from_dtc <- function(dtc) {
  hms::as_hms(case_when(
    nchar(dtc) < 16 ~ NA_character_,
    nchar(dtc) == 16 ~ paste0(stringr::word(dtc, 2, sep = "T"), ":00")
  ))
}


datetime_from_dtc <- function(dtc) {
  lubridate::parse_date_time(dtc, "Ymd HMS", truncated = 3)
}


