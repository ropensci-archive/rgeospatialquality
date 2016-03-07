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
    message("Launching API request... ", appendLF = FALSE)
    req <- httr::POST(BASE_URL, body=req_body)
    message("done.")

    # Prepare response
    message("Parsing output... ", appendLF = FALSE)
    resp <- gq_parse(req)
    if("flags" %in% names(resp)) indf$flags <- resp$flags
    message("done.")
    indf
}

gq_parse_dataframe <- function(indf) {

    # Parse input object type
    if(is.na(indf) || (is.data.frame(indf) && nrow(indf) == 0)) stop("Input data frame missing or empty")
    if(!(is.data.frame(indf))) stop("Provided argument is not a data.frame")

    # Parse input content completeness
    if (!("decimalLatitude" %in% names(indf))) warning("'decimalLatitude' element missing")
    if (!("decimalLongitude" %in% names(indf))) warning("'decimalLongitude' element missing")
    if (!("countryCode" %in% names(indf))) warning("'countryCode' element missing")
    if (!("scientificName" %in% names(indf))) warning("'scientificName' element missing")

}
