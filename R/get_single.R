#' Calculate flags for a single record
#'
#' \code{get_single} calls the \code{GET} method of the API in order to extract
#' the flags for an individual record
#'
#' @param decimalLatitude Latitude in decimal degrees format (float, e.g.
#'   42.1833)
#' @param decimalLongitude Longitude in decimal degrees format (float, e.g.
#'   -1.8332)
#' @param countryCode Two or three-letter ISO-3166 country codes (string, e.g.
#'   "ES")
#' @param scientificName Binomial identifying the species (string, e.g. "Puma
#'   concolor")
#' @return The supplied record wih the geospatial quality flags
#' @examples
#' get_single(decimalLatitude=42.1833, decimalLongitude=-1.8332, countryCode="ES", scientificName="Puma concolor")
#' @seealso
#'   \code{\link{http://bioinformatics.oxfordjournals.org/content/early/2016/02/16/bioinformatics.btw057}}
#'   for more info on the API
#' @export
get_single <- function(decimalLatitude=NA, decimalLongitude=NA, countryCode=NA, scientificName="") {
    params = list(
        decimalLatitude=decimalLatitude,
        decimalLongitude=decimalLongitude,
        countryCode=countryCode,
        scientificName=scientificName
    )
    a <- httr::GET(BASE_URL, query=params)
    if(a$status_code==200){
        return(httr::content(a, "parsed"))
    } else {
        return(NA)
    }
}
