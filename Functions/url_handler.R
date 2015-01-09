url_handler <- function(data){
  data$url <- gsub(x = extract_host(data$url), pattern = "\\.(m|wap|mobile|zero)\\.", replacement = ".", perl = TRUE)
  data <- data[grepl(x = data$url, pattern = "\\.org$", perl = TRUE, useBytes = TRUE),]
  data$url <- gsub(x = data$url, pattern = "\\.org$", perl = TRUE, useBytes = TRUE, replacement = "")
  data$referer <- extract_host(url_decode(data$referer))
  referers <- character(nrow(data))
  referers[grepl(x = data$referer, pattern = "\\.wiki.*\\.org$", perl = TRUE)] <- "Internal"
  referers[data$referer == ""] <- "None"
  referers[grepl(x = data$referer, pattern = "google.", fixed = TRUE)] <- "Google"
  referers[grepl(x = data$referer, pattern = "yahoo.", fixed = TRUE)] <- "Yahoo"
  referers[grepl(x = data$referer, pattern = "facebook.", fixed = TRUE)] <- "Facebook"
  referers[grepl(x = data$referer, pattern = "twitter.", fixed = TRUE)] <- "Twitter"
  referers[grepl(x = data$referer, pattern = "t.co", fixed = TRUE)] <- "Twitter"
  referers[grepl(x = data$referer, pattern = "bing.", fixed = TRUE)] <- "Bing"
  referers[grepl(x = data$referer, pattern = "baidu.", fixed = TRUE)] <- "Baidu"
  referers[referers == ""] <- "Other"
  data$referer <- referers
  return(data)
}