tag <- function(data){
  
  is_zero_x <- grepl(x = data$x_analytics, pattern = "zero", fixed = TRUE)
  is_zero_url <- grepl(x = data$url, pattern = ".zero.", fixed = TRUE)
  data$is_zero <- is_zero_x | is_zero_url
  
  devices <- uaparser(data$user_agent, "device")
  is_wm_bot <- grepl(x = data$user_agent, pattern = "(wikiwix-bot|goo wikipedia|MediaWikiCrawler-Google)",
                     perl = TRUE, useBytes = TRUE)
  data$is_spider <- devices == "Spider" | is_wm_bot
  is_automata <- grepl(x = data$user_agent,
                       pattern =  "^(MediaWiki|Twisted|crawler4j|Wget/|Java/|curl/|libwww-perl|Python-urllib|WordPress/)",
                       perl = TRUE, useBytes = TRUE)
  data$is_automata <- is_automata
  type <- character(nrow(data))
  type[grepl(x = data$user_agent, pattern = "WikipediaApp", fixed = TRUE)] <- "mobile app"
  type[type == "" & grepl(x = data$url, pattern = "\\.(m|wap|zero|mobile)\\.", perl = TRUE)] <- "mobile web"
  type[type == ""] <- "desktop"
  data$access_method <- type
  return(data)
}