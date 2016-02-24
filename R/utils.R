BASE_URL <- "http://api-geospatial.vertnet-portal.appspot.com/geospatial"

gq_parse_record <- function(record) {
    # Check that 'record' is a vector-type object
    if(!is.vector(record)) stop("'record' object must be a vector")

    # Check that all four elements are present
    if(!'decimalLatitude' %in% names(record)) stop("'decimalLatitude' element not found in 'record' vector")
    if(!'decimalLongitude' %in% names(record)) stop("'decimalLongitude' element not found in 'record' vector")
    if(!'countryCode' %in% names(record)) stop("'countryCode' element not found in 'record' vector")
    if(!'scientificName' %in% names(record)) stop("'scientificName' element not found in 'record' vector")

    params <- list(
        decimalLatitude=record$decimalLatitude,
        decimalLongitude=record$decimalLongitude,
        countryCode=record$countryCode,
        scientificName=record$scientificName
    )

    params
}

gq_parse_params <- function(params) {
    if(is.na(params$decimalLatitude)) params$decimalLatitude=""
    if(is.na(params$decimalLongitude)) params$decimalLongitude=""
    if(is.na(params$countryCode)) params$countryCode=""
    if(is.na(params$scientificName)) params$scientificName=""

    params
}

gq_check <- function(req) {
    # If successful, return nothing
    if (req$status_code<400) return(invisible())
    # Otherwise, throw error/warning
    # TODO
}

gq_parse <- function(req) {
    return(jsonlite::fromJSON(httr::content(req, as="text"), simplifyVector=TRUE))
}
