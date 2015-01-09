convert_dates <- function(dates){
  dates <- iconv(dates, to = "UTF-8")
  dates[nchar(dates) > 19] <- substring(dates[nchar(dates) > 19],1,19)
  dates <- strptime(dates, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  minute(dates) <- 0
  second(dates) <- 0
  return(as.character(dates))
}