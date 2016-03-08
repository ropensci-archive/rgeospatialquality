#' Calculate flags for a set of records and add them to the provided data frame
#'
#' \code{add_flags} calls the \code{POST} method of the API in order to extract
#' the flags for a set of records
#'
#' @param indf Required. Properly formatted data frame containing a row per
#'   record
#'
#' @return The provided data frame with the quality flags added as new columns
#'
#' @examples
#' # Using the rgbif package, prepare data
#' d <- rgbif::occ_data(scientificName="Apis mellifera", limit=50, minimal=FALSE)
#' d <- d$data
#'
#' # Execute the call to the API and store the results
#' dd <- add_flags(d)
#'
#' @seealso \code{\link{flags}}
#'
#' @export
add_flags <- function(indf=NA) {

    # Parse input
    gq_parse_dataframe(indf)

    # Prepare POST request
    req_body <- jsonlite::toJSON(indf)

    # Make POST request
    req <- httr::POST(BASE_URL, body=req_body)

    # Prepare response
    resp <- gq_parse(req)
    if("flags" %in% names(resp)) indf$flags <- resp$flags

    indf
}

gq_parse_dataframe <- function(indf) {

    # Parse input object type
    gq_check_df(indf)

    # Parse input content completeness
    if (!("decimalLatitude" %in% names(indf))) warning("'decimalLatitude' element missing")
    if (!("decimalLongitude" %in% names(indf))) warning("'decimalLongitude' element missing")
    if (!("countryCode" %in% names(indf))) warning("'countryCode' element missing")
    if (!("scientificName" %in% names(indf))) warning("'scientificName' element missing")

}
