geolocate <- function(ips, xffs){
  ips[!xffs == "-"] <- xffs[!xffs == "-"]
  ips <- xff_extract(ips)
  countries <- geolookup(ip_addresses = ips,
                         filename = "/usr/local/share/GeoIP/GeoIP2-Country.mmdb",
                         fields = "country_name")$country_name
  return(countries)
}