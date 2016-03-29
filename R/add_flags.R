#' Calculate flags for a set of records and add them to the provided data frame
#'
#' \code{add_flags} calls the \code{POST} method of the API in order to extract
#' the flags for a set of records. After finishing, the function returns the
#' provided data.frame with a new column, \code{flags}, which holds for each
#' record a list of the geospatial quality flags. It also shows a summary of the
#' results, indicating how many records showed different types of issues.
#' \strong{NOTE:} currently, the API imposes a hard-limit of 1000 records per
#' request, to avoid malfunctioning due to some third-party library limtations.
#'
#' @param indf Required. Properly formatted data frame containing a row per
#'   record
#' @param show_summary Optional. Show a summary of the quality flags after the
#'   process has finished. Defaults to TRUE
#' @param quiet Optional. Don't show any logging message at all. Defaults to
#'   FALSE
#' @param ... Any extra parameters for \code{httr} \code{\link{POST}}
#'
#' @return The provided data frame with the quality flags added as new columns
#'
#' @examples
#' # Using the rgbif package, prepare data
#' if (requireNamespace("rgbif", quietly=TRUE)) {
#'  library("rgbif")
#'  d <- occ_data(scientificName="Apis mellifera", limit=50, minimal=FALSE)
#'  d <- d$data
#'
#'  # Format data.frame
#'  d <- format_gq(d, source="rgbif")
#'
#'  # Execute the call to the API and store the results
#'  dd <- add_flags(d)
#' }
#'
#' @seealso \code{\link{format_gq}}, \code{\link{flags}},
#'   \code{\link{parse_record}}
#'
#' @export
add_flags <- function(indf, show_summary=TRUE, quiet=FALSE, ...) {

    # Parse input
    gq_parse_dataframe(indf, quiet)

    # Prepare POST request
    req_body <- jsonlite::toJSON(indf)

    # Make POST request
    if (!(quiet)) message("Launching API request... ", appendLF = FALSE)
    req <- httr::POST(BASE_URL, body=req_body, ...)
    if (!(quiet)) message("done.")

    # Prepare response
    if (!(quiet)) message("Parsing output... ", appendLF = FALSE)
    resp <- gq_parse(req)
    if("flags" %in% names(resp)) indf$flags <- resp$flags
    if (!(quiet)) message("done.")

    if (show_summary && !(quiet)) gq_show_summary(resp$flags)

    indf
}

gq_parse_dataframe <- function(indf, quiet) {

    # Parse input object type
    gq_check_df(indf)

    # Parse input content completeness
    if (!(quiet)) {
        if (!("decimalLatitude" %in% names(indf))) warning("'decimalLatitude' element missing")
        if (!("decimalLongitude" %in% names(indf))) warning("'decimalLongitude' element missing")
        if (!("countryCode" %in% names(indf))) warning("'countryCode' element missing")
        if (!("scientificName" %in% names(indf))) warning("'scientificName' element missing")
    }

    if(nrow(indf)>1000) stop("Too many records. The API has a hard-limit of 1000 records per request")

}

gq_show_summary <- function(flags) {
    message("\n=== SUMMARY ===")
    message(c(nrow(flags)," records parsed"))
    message(c(sum(flags$hasCoordinates)," records with coordinates"), appendLF = FALSE)
    message(c(" (",sum(flags$validCoordinates)," valid)"))
    message(c(sum(flags$hasCountry)," records with country"), appendLF = FALSE)
    message(c(" (",sum(flags$validCountry)," valid)"))
    message(c(sum(flags$hasScientificName)," records with scientific name"))

    message("=== GEOSPATIAL ===")
    if("nonZeroCoordinates" %in% names(flags) && sum(!(flags$nonZeroCoordinates), na.rm = TRUE) > 0){
        message(c(sum(!(flags$nonZeroCoordinates), na.rm=TRUE), " record/s with coordinates = 0,0"))
    }
    if("highPrecisionCoordinates" %in% names(flags) && sum(!(flags$highPrecisionCoordinates), na.rm = TRUE) > 0) {
        message(c(sum(!(flags$highPrecisionCoordinates), na.rm = TRUE), " record/s with low precision coordinates"))
    }
    if("coordinatesInsideCountry" %in% names(flags) && sum(!(flags$coordinatesInsideCountry), na.rm = TRUE) > 0) {
        message(c(sum(!(flags$coordinatesInsideCountry), na.rm = TRUE), " record/s with coordinate-country mismatch"), appendLF = FALSE)
        if (sum(flags$negatedLatitude, na.rm = TRUE)>0 || sum(flags$negatedLongitude, na.rm = TRUE)>0 || sum(flags$transposedCoordinates, na.rm = TRUE)>0) {
            message(", of which...")
            if (sum(flags$negatedLatitude, na.rm = TRUE)>0) message(c("\t", sum(flags$negatedLatitude, na.rm = TRUE), " record/s with negated latitude"))
            if (sum(flags$negatedLongitude, na.rm = TRUE)>0) message(c("\t", sum(flags$negatedLongitude, na.rm = TRUE), " record/s with negated longitude"))
            if (sum(flags$transposedCoordinates, na.rm = TRUE)>0) message(c("\t", sum(flags$transposedCoordinates, na.rm = TRUE), " record/s with transposed coordinates"))
        } else {
            message("")
        }
    }

    if ("coordinatesInsideRangeMap" %in% names(flags)) {
        message("=== SPATIO-TAXONOMIC ===")
        if(sum(!(flags$coordinatesInsideRangeMap), na.rm=TRUE) > 0) {
            message(c(sum(!(flags$coordinatesInsideRangeMap), na.rm = TRUE), " record/s out of their species' range map"))
        }
    }

}
