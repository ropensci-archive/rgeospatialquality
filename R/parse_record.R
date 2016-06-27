#' Calculate flags for a single record
#'
#' \code{parse_record} calls the \code{GET} method of the API in order to
#' extract the flags for an individual record. It returns just the \code{flags}
#' element.
#'
#' Data can be passed in two different ways to this function: either with the
#' four key elements (\code{decimalLatitude}, \code{decimalLongitude},
#' \code{countryCode} and \code{scientificName}) passed as named arguments or
#' via a single \code{record} parameter that consists of a list with these four
#' elements. However, if both are filled, the function will stop and show an
#' error message. The more filled fields, the more informative the output of the
#' API will be. However, due to the flexible nature of the underlying API, a
#' valid response will be get even if no input data is provided. Therefore,
#' calling \code{parse_record()} with no arguments will return a list of three
#' \code{FALSE} elements, which represents no information at all. The function
#' will however throw a warning for each missing field. See \code{\link{flags}}
#' for more info on the input and output of the function.
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
#' @examples \dontrun{
#' # Using the 'record' parameter
#' # Create 'record' object with values
#' rec <- list(decimalLatitude=42.1833, decimalLongitude=-1.8332,
#'             countryCode="ES", scientificName="Puma concolor")
#' # Call the API and get the results
#' parse_record(record=rec)
#'
#' # Using named arguments
#' parse_record(decimalLatitude=42.1833, decimalLongitude=-1.8332,
#'            countryCode="ES", scientificName="Puma concolor")
#' # In both cases, the result will be the same
#'
#' # If any parameter is missing, the function runs, but throws a warning
#' # message. This will throw a warning saying that 'countryCode' and
#' # 'scientificName' fields are missing. Also, the results will be
#' # limited to the feasible calculations.
#' parse_record(decimalLatitude=42.1833, decimalLongitude=-1.8332)
#'
#' # One can call the function without parameters. Although valid,
#' # this way of calling the function is useless.
#' parse_record()
#' }
#' @seealso \code{\link{flags}}, \code{\link{add_flags}}
#'
#' @export
parse_record <- function(record=NULL, decimalLatitude=NULL, decimalLongitude=NULL, countryCode=NULL, scientificName=NULL, ...) {
    # Parse parameters
    if (!is.null(record)) {
        if (!is.null(decimalLatitude) || !is.null(decimalLongitude) || !is.null(countryCode) || !is.null(scientificName)) {
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
    }

    # Remove NULLs
    params <- plyr::compact(params)
    if (!("decimalLatitude" %in% names(params))) warning("'decimalLatitude' element missing")
    if (!("decimalLongitude" %in% names(params))) warning("'decimalLongitude' element missing")
    if (!("countryCode" %in% names(params))) warning("'countryCode' element missing")
    if (!("scientificName" %in% names(params))) warning("'scientificName' element missing")

    # Make GET request
    req <- httr::GET(BASE_URL, query=params, ...)

    # Parse and return response
    resp <- gq_parse(req)

    # Check for errors
    if(is.null(req)) return()

    resp <- resp$flags
    resp
}

gq_parse_record <- function(record) {
    # Check that 'record' is a vector-type object
    if(!is.vector(record) && !is.list(record)) stop("'record' object must be a vector")

    params <- list()

    # Check that all four elements are present
    if(!'decimalLatitude' %in% names(record)) params$decimalLatitude <- NULL else params$decimalLatitude <- record$decimalLatitude
    if(!'decimalLongitude' %in% names(record)) params$decimalLongitude <- NULL else params$decimalLongitude <- record$decimalLongitude
    if(!'countryCode' %in% names(record)) params$countryCode <- NULL else params$countryCode <- record$countryCode

    if('scientificName' %in% names(record)) {
        params$scientificName <- record$scientificName
    } else if('name' %in% names(record)) {
        params$scientificName <- record$name
    } else {
        params$scientificName <- NULL
    }

    params
}
