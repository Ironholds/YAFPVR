filter <- function(data){
  data$url <- url_decode(data$url)
  app_handler <- function(x){
    is_app <- grepl(x = x$user_agent, pattern = "WikipediaApp", fixed = TRUE, useBytes = TRUE)
    is_pv <- grepl(x = x$url, pattern = "sections=0", fixed = TRUE, useBytes = TRUE)
    x <- x[is_app & is_pv,]
    return(x)
  }
  
  data <- data[data$mime_type %in% c("text/html",
                                     "text/html; charset=iso-8859-1",
                                     "text/html; charset=ISO-8859-1",
                                     "text/html; charset=utf-8",
                                     "text/html; charset=UTF-8",
                                     "application/json"),]
  data <- data[grepl(x = data$status_code, pattern = "200", fixed = TRUE),]
  data <- data[grepl(x = data$url,
                     pattern = "((commons|meta|incubator|species)\\.((m|mobile|wap|zero)\\.)?wikimedia|(wik(ibooks|idata|inews|ipedia|iquote|isource|tionary|iversity|ivoyage)))\\.org",
                     perl = TRUE, useBytes = TRUE),]
  data <- data[grepl(x = data$url, pattern = "((/sr(-(ec|el))?|/w(iki)?|/zh(-(cn|hans|hant|hk|mo|my|sg|tw))?)/|\\?((cur|old)id|title)=)",
               perl = TRUE, useBytes = TRUE),]
  data <- data[!grepl(x = data$url, pattern = "(BannerRandom|CentralAutoLogin|MobileEditor|Undefined|UserLogin|ZeroRatedMobileAccess)",
                     perl = TRUE, useBytes = TRUE),]
  
  is_api <- grepl(x = data$url, pattern = "api.php", fixed = TRUE, useBytes = TRUE)
  data <- rbind(data[!is_api,], app_handler(data[is_api,]))
  return(data)
}
