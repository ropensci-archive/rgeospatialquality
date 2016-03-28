#' Calculate flags for a single record
#'
#' \code{parse_record} calls the \code{GET} method of the API in order to extract
#' the flags for an individual record. It returns just the \code{flags} element
#'
#' @param record List-type object containing information on the record. If
#'   present, it MUST contain the following four attributes as named elements.
#' @param decimalLatitude Only if 'record' is not present. Latitude in decimal
#'   degrees format (float, e.g. 42.1833)
#' @param decimalLongitude Only if 'record' is not present. Longitude in decimal
#'   degrees format (float, e.g. -1.8332)
#' @param countryCode Only if 'record' is not present. Two or three-letter
#'   ISO-3166 country codes (string, e.g. "ES")
#' @param scientificName Only if 'record' is not present. Binomial identifying
#'   the species (string, e.g. "Puma concolor")
#' @param ... Any extra parameters for \code{httr} \code{\link{GET}}
#'
#' @return A named list with the geospatial quality flags
#'
#' @examples
#' rec <- list(decimalLatitude=42.1833, decimalLongitude=-1.8332,
#'             countryCode="ES", scientificName="Puma concolor")
#' parse_record(record=rec)
#'
#' # OR
#'
#' parse_record(decimalLatitude=42.1833, decimalLongitude=-1.8332,
#'            countryCode="ES", scientificName="Puma concolor")
#'
#' @seealso \code{\link{flags}}, \code{\link{add_flags}}
#'
#' @export
parse_record <- function(record=NA, decimalLatitude=NA, decimalLongitude=NA, countryCode="", scientificName="", ...) {
    # Parse parameters
    if (!is.na(record)) {
        if (!is.na(decimalLatitude) || !is.na(decimalLongitude) || countryCode!="" || scientificName != "") {
            stop("Both \"record\" and other named parameters are provided. You should select one of the two methods.\nPlease see ?parse_record for more information.")
        }
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
    req <- httr::GET(BASE_URL, query=params, ...)

    # Parse and return response
    resp <- gq_parse(req)
    resp <- resp$flags
    resp
}

gq_parse_record <- function(record) {
    # Check that 'record' is a vector-type object
    if(!is.vector(record) && !is.list(record)) stop("'record' object must be a vector")

    params <- list()

    # Check that all four elements are present
    if(!'decimalLatitude' %in% names(record)) params$decimalLatitude <- "" else params$decimalLatitude <- record$decimalLatitude
    if(!'decimalLongitude' %in% names(record)) params$decimalLongitude <- "" else params$decimalLongitude <- record$decimalLongitude
    if(!'countryCode' %in% names(record)) params$countryCode <- "" else params$countryCode <- record$countryCode

    if('scientificName' %in% names(record)) {
        params$scientificName <- record$scientificName
    } else if('name' %in% names(record)) {
        params$scientificName <- record$name
    } else {
        params$scientificName <- ""
    }

    params
}

gq_parse_params <- function(params) {
    if(is.na(params$decimalLatitude)) params$decimalLatitude=""
    if(is.na(params$decimalLongitude)) params$decimalLongitude=""
    if(is.na(params$countryCode)) params$countryCode=""
    if(is.na(params$scientificName)) params$scientificName=""

    params
}
