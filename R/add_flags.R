#' Calculate flags for a set of records and add them to the provided data frame
#'
#' \code{add_flags} calls the \code{POST} method of the API in order to extract
#' the flags for a set of records. \strong{NOTE:} currently, the API imposes a
#' hard-limit of 1000 records per request, to avoid malfunctioning due to some
#' third-party library limtations. This function will not work if a
#' \code{data.frame} with more than 1000 rows is provided.
#'
#' Internally, the function takes the provided \code{data.frame}, transforms it
#' to JSON and makes a \code{POST} request to the underlying API with the JSON
#' object in the body of the request. In order to work properly and give
#' comprehensive results, the \code{data.frame} should have the four key fields
#' this API works with. See \code{\link{flags}} for details. If a field is
#' missing, the function will show a warning. If the name of the fields in your
#' \code{data.frame} don't conform to the DarwinCore standard, \code{add_flags}
#' can try to map the names in the \code{data.frame} to the standard ones if the
#' parameter \code{guess_fields} is set to \code{TRUE}. In this case, if there
#' is no match, the function will stop and give instructions on how to resume.
#' If there is, the original name in the \code{data.frame} will not change.
#'
#' After finishing, the function returns the provided \code{data.frame} with a
#' new column, \code{flags}, which holds for each record a list of the
#' geospatial quality flags. If \code{show_summary} is \code{TRUE} (default
#' value), it also shows a summary of the results, indicating how many records
#' showed different types of issues.
#'
#' @param indf Required. Properly formatted data frame containing a row per
#'   record
#' @param guess_fields Optional. Try or not to guess key fields if names don't
#'   follow the DarwinCore standard (see details). Defaults to FALSE, meaning it
#'   won't try to guess field names and will throw warnings for each missing
#'   field. Set to TRUE to try to guess field names (this will make the function
#'   stop if no match can be found for any of the key fields)
#' @param show_summary Optional. Show a summary of the quality flags after the
#'   process has finished. Defaults to TRUE
#' @param quiet Optional. Don't show any logging message at all. Defaults to
#'   FALSE
#' @param ... Any extra parameters for \code{httr} \code{\link{POST}}
#'
#' @return The provided data frame with the quality flags added as new columns
#'
#' @examples \dontrun{
#' # Using the rgbif package
#' if (requireNamespace("rgbif", quietly=TRUE)) {
#'  library("rgbif")
#'
#'  # Prepare data
#'  d <- occ_data(scientificName="Apis mellifera", limit=50, minimal=FALSE)
#'  d <- d$data
#'
#'  # Format data.frame
#'  d <- format_gq(d, source="rgbif")
#'
#'  # Execute the call to the API, showing output and
#'  # logging information, and store the results
#'  dd <- add_flags(d)
#'
#'  # Alternatively, instead of formating with 'format_gq', make the function
#'  # guess the correct name of the fields.
#'  dd <- add_flags(d, guess_fields=TRUE)
#'
#'  # Execute the call without showing summary output, but
#'  # showing logging information
#'  dd <- add_flags(d, show_summary=FALSE)
#'
#'  # Execute the call without showing any logging at all
#'  # (except errors, obviously)
#'  dd <- add_flags(d, quiet=TRUE)
#'
#'  # Data quality output will be stored in a new field called flags
#'  names(dd$flags)
#'
#'  # You can check records with certain flags as usual
#'  # See records with coordinates-country mismatch
#'  dd[dd$flags$coordinatesInsideCountry == FALSE,]
#' }
#' }
#'
#' @seealso \code{\link{format_gq}}, \code{\link{flags}},
#'   \code{\link{parse_record}}
#'
#' @export
add_flags <- function(indf, guess_fields=FALSE, show_summary=TRUE, quiet=FALSE, ...) {
    UseMethod("add_flags", indf)
}

#' @export
#' @rdname add_flags
add_flags.default <- function(indf, guess_fields=FALSE, show_summary=TRUE, quiet=FALSE, ...) {
    stop("Please provide a data.frame object as input", call. = FALSE)
}

#' @export
#' @rdname add_flags
add_flags.data.frame <- function(indf, guess_fields=FALSE, show_summary=TRUE, quiet=FALSE, ...) {

    # Parse input
    indf2 <- gq_parse_dataframe(indf, guess_fields, quiet)

    # Prepare POST request
    req_body <- jsonlite::toJSON(indf2)

    # Make POST request
    if (!(quiet)) message("Launching API request... ", appendLF = FALSE)
    req <- httr::POST(BASE_URL, body=req_body, ...)
    if (!(quiet)) message("done.")

    # Prepare response
    if (!(quiet)) message("Parsing output... ", appendLF = FALSE)
    resp <- gq_parse(req)

    # Check for errors
    if(is.null(req)) return()

    # If not, continue
    if("flags" %in% names(resp)) indf$flags <- resp$flags
    if (!(quiet)) message("done.")

    if (show_summary && !(quiet)) gq_show_summary(resp$flags)

    indf
}

gq_replace_field <- function(indf, orig, dwc, quiet) {
    if (orig %in% names(indf)) {
        names(indf)[names(indf)==orig] <- dwc
        if (!(quiet)) {
            message(paste("Field", orig, "identified as", dwc))
        }
    }
    return(indf)
}

gq_parse_dataframe <- function(indf, guess_fields=FALSE, quiet=FALSE) {

    # Parse input object type
    gq_check_df(indf)

    # Stop if too manu records.
    if(nrow(indf)>1000) stop("Too many records. The API has a hard-limit of 1000 records per request")

    # Parse input content completeness
    if (guess_fields) {
        # Try to guess "decimalLatitude"
        indf <- gq_replace_field(indf, "decimallatitude", "decimalLatitude", quiet)
        indf <- gq_replace_field(indf, "DecimalLatitude", "decimalLatitude", quiet)
        indf <- gq_replace_field(indf, "decimal.latitude", "decimalLatitude", quiet)
        indf <- gq_replace_field(indf, "decimal_latitude", "decimalLatitude", quiet)
        indf <- gq_replace_field(indf, "Lat", "decimalLatitude", quiet)
        indf <- gq_replace_field(indf, "lat", "decimalLatitude", quiet)
        indf <- gq_replace_field(indf, "Latitude", "decimalLatitude", quiet)
        indf <- gq_replace_field(indf, "latitude", "decimalLatitude", quiet)
        # Try to guess "decimalLongitude"
        indf <- gq_replace_field(indf, "decimallongitude", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "DecimalLongitude", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "decimal.longitude", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "decimal_longitude", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "Lon", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "lon", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "Lng", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "lng", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "Longitude", "decimalLongitude", quiet)
        indf <- gq_replace_field(indf, "longitude", "decimalLongitude", quiet)
        # Try to guess "countryCode"
        indf <- gq_replace_field(indf, "countrycode", "countryCode", quiet)
        indf <- gq_replace_field(indf, "CountryCode", "countryCode", quiet)
        indf <- gq_replace_field(indf, "country.code", "countryCode", quiet)
        indf <- gq_replace_field(indf, "country_code", "countryCode", quiet)
        indf <- gq_replace_field(indf, "country", "countryCode", quiet)
        indf <- gq_replace_field(indf, "Country", "countryCode", quiet)
        indf <- gq_replace_field(indf, "ccode", "countryCode", quiet)
        indf <- gq_replace_field(indf, "Ccode", "countryCode", quiet)
        # Try to guess "scientificName"
        indf <- gq_replace_field(indf, "scientificname", "scientificName", quiet)
        indf <- gq_replace_field(indf, "ScientificSame", "scientificName", quiet)
        indf <- gq_replace_field(indf, "scientific.name", "scientificName", quiet)
        indf <- gq_replace_field(indf, "scientific_name", "scientificName", quiet)
        indf <- gq_replace_field(indf, "sciname", "scientificName", quiet)
        indf <- gq_replace_field(indf, "Sciname", "scientificName", quiet)

        # Stop if any is missing
        mess1 <- "Could not find a match for"
        mess2 <- ". Please use 'format_gq' function to indicate field name mapping or run the function without 'guess_fields' to ignore the absence of this field."
        if (!("decimalLatitude" %in% names(indf))) stop(paste(mess1, "decimalLatitude", mess2))
        if (!("decimalLongitude" %in% names(indf))) stop(paste(mess1, "decimalLongitude", mess2))
        if (!("countryCode" %in% names(indf))) stop(paste(mess1, "countryCode", mess2))
        if (!("scientificName" %in% names(indf))) stop(paste(mess1, "scientificName", mess2))

    } else {
        if (!(quiet)) {
            if (!("decimalLatitude" %in% names(indf))) warning("'decimalLatitude' element missing")
            if (!("decimalLongitude" %in% names(indf))) warning("'decimalLongitude' element missing")
            if (!("countryCode" %in% names(indf))) warning("'countryCode' element missing")
            if (!("scientificName" %in% names(indf))) warning("'scientificName' element missing")
        }
    }

    return(indf)
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
