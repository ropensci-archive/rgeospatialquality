BASE_URL <- "http://api-geospatial.vertnet-portal.appspot.com/geospatial"

gq_parse <- function(req) {
    return(jsonlite::fromJSON(httr::content(req, as="text"), simplifyVector=TRUE))
}
