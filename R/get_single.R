#' Calculate flags for a single record
#'
#' \code{get_single} calls the \code{GET} method of the API in order to extract
#' the flags for an individual record
#'
#' @param record Vector-type object containing information on the record. If
#'   present, it MUST contain the following four named elements.
#' @param decimalLatitude Only if 'record' is not present. Latitude in decimal
#'   degrees format (float, e.g. 42.1833)
#' @param decimalLongitude Only if 'record' is not present. Longitude in decimal
#'   degrees format (float, e.g. -1.8332)
#' @param countryCode Only if 'record' is not present. Two or three-letter
#'   ISO-3166 country codes (string, e.g. "ES")
#' @param scientificName Only if 'record' is not present. Binomial identifying
#'   the species (string, e.g. "Puma concolor")
#' @return A vector with the supplied record plus the geospatial quality flags
#'   in the $flags element
#' @examples
#' rec <- list(decimalLatitude=42.1833, decimalLongitude=-1.8332,
#'             countryCode="ES", scientificName="Puma concolor")
#' get_single(record=rec)
#'
#' # OR
#'
#' get_single(decimalLatitude=42.1833, decimalLongitude=-1.8332,
#'            countryCode="ES", scientificName="Puma concolor")
#' @export
get_single <- function(record=NA, decimalLatitude=NA, decimalLongitude=NA, countryCode="", scientificName="") {
    # Parse parameters
    if (!missing(record)) {
        params <- gq_parse_record(record)
    } else {
        params <- list(
            decimalLatitude=decimalLatitude,
            decimalLongitude=decimalLongitude,
            countryCode=countryCode,
            scientificName=scientificName
        )
        params <- gq_parse_params(params)
    }
    # Make GET request
    req <- httr::GET(BASE_URL, query=params)
    gq_check(req)

    # Parse and return response
    resp <- gq_parse(req)
    resp
}
