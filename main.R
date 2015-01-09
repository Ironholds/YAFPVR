
main <- function(earliest = NULL, latest = NULL){
  
  core <- function(file){
    data <- read_file(file) %>%
      filter %>%
      tag
    data$timestamp <- convert_dates(data$timestamp)
    data$country <- geolocate(data$ip_address, data$x_forwarded)
    
  }
  files <- get_files(earliest,latest)
  
}