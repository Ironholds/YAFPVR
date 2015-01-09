get_files <- function(earliest = NULL, latest = NULL){
  files <- list.files("/a/squid/archive/sampled/", full.names= TRUE, pattern = "gz$")
  if(!is.null(earliest)){
    file_dates <- as.numeric(substring(files,48,55))
    if(!is.null(latest)){
      files <- files[file_dates >= earliest & file_dates <= latest]
    } else {
      files <- files[file_dates >= earliest]
    }
  }
  return(files)
}

read_file <- function(file){
  output_file <- tempfile()
  system(paste("gunzip -c", file, ">", output_file))
  data <- as.data.table(read.delim(output_file, as.is = TRUE,
                                   header = FALSE, quote = "",
                                   col.names = c("squid","sequence_no",
                                                 "timestamp", "servicetime",
                                                 "ip_address", "status_code",
                                                 "reply_size", "request_method",
                                                 "url", "squid_status",
                                                 "mime_type", "referer",
                                                 "x_forwarded", "user_agent",
                                                 "lang", "x_analytics")))
  data <- data[!grepl(x = data$squid, pattern = "ssl", fixed = TRUE),]
  suppressWarnings({
    data <- data[,c("sequence_no","servicetime","request_method","reply_size", "lang","squid",
                    "squid_status") := NULL]
  })
  file.remove(output_file)
  return(data)
}