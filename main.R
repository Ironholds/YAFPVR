main <- function(earliest = NULL, latest = NULL){
  
  core <- function(file){
    data <- read_file(file) %>%
      filter %>%
      tag %>%
      url_handler
    
    data$timestamp <- convert_dates(data$timestamp)
    data$country <- geolocate(data$ip_address, data$x_forwarded)
    data <- data[,c("ip_address","status_code",
                    "mime_type","x_forwarded","user_agent","x_analytics") := NULL]
    data <- data[,j = list(pageviews = .N*1000), by = names(data)]
    cat(".")
    return(data)
  }
  files <- get_files(20131201,20150101)
  data <- mclapply(files,core, mc.cores = 4, mc.preschedule=FALSE)
  
}