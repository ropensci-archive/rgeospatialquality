#' Prepare data frame for flagging functions
#'
#' \code{format_gq} renames certain fields to make sure the API knows how to use
#' them. This step is highly recommended for the proper assessment of the
#' provided data.frame.
#'
#' When invoked, there are three ways of indicating the function how to
#' transform the data.frame: using the \code{source} parameter, providing a
#' \code{config} object with field mapping, or passing individual values to the
#' mapping function. This is the order in which the function will parse
#' arguments; \code{source} overrides \code{config}, which overrides other
#' mapping arguments.
#'
#' \code{source} refers to the package that was used to retrieve the data.
#' Currently, three values are supported for this argument: "\code{rgbif}",
#' "\code{rvertnet}" and "\code{rinat}", but many more are on their way.
#'
#' \code{config} asks for a configuration object holding the mapping of the
#' field names. This option is basically a shortcut for those users with
#' custom-formatted data.frames who will use the same mapping many times, to
#' avoid having to type them each time. In practice, this object is a named list
#' with the following four fields: \code{decimaLatitude},
#' \code{decimaLongitude}, \code{countryCode} and \code{scientificName}. Each
#' element must have a string indicating the name of the column in the
#' data.frame holding the values for that element. If the data.frame doesn't
#' have one or more of these fields, put \code{NA} in that element; otherwise,
#' the function will throw an error. See the examples section.
#'
#' If none of the two is provided, the function expects the user to provide the
#' mapping by passing the individual column names associated with the right term
#' of the DarwinCore Standard. See the examples section.
#'
#' @param indf Required. The data.frame on which to operate.
#' @param source Optional. Indicates the package that was used to retrieve the
#'   data. Currently accepted values are "rvertnet", "rgbif" or "rinat". Either
#'   \code{source}, \code{config} or individual parameters must be present (see
#'   details).
#' @param config Optional. Configuration object indicating mapping of field
#'   names from the data.frame to the DarwinCore standard. Useful when importing
#'   data multiple times from a source not available via the \code{source}
#'   argument. Either \code{source}, \code{config} or individual parameters must
#'   be present (see details).
#' @param quiet Optional. Don't show any logging message at all. Defaults to
#'   FALSE.
#' @param ... Optional. If none of the previous is present, the four key
#'   arguments (\code{decimalLatitude}, \code{decimalLongitude},
#'   \code{countryCode}, \code{scientificName}) can be put here. See examples.
#'
#' @return The provided data frame, with field names changed to fit the API
#'   functioning.
#'
#' @examples \dontrun{
#' # Using the rgbif package and the source argument
#' if (requireNamespace("rgbif", quietly=TRUE)) {
#'  d <- rgbif::occ_data(scientificName="Apis mellifera", limit=50, minimal=FALSE)
#'  d <- d$data
#'  d <- format_gq(d, source="rgbif")
#'
#'  # Using a configuration object (matches 'rinat' schema)
#'  conf <- list(decimalLatitude="latitude",
#'               decimalLongitude="longitude",
#'               countryCode=NULL,
#'               scientificName="scientific_name")
#'  d <- format_gq(d, config=conf)
#'
#'  # Passing individual parameters, all optional
#'  d <- format_gq(d,
#'                 decimalLatitude="lat",
#'                 decimalLongitude="lng",
#'                 countryCode="ccode",
#'                 scientificName="sciname")
#' }
#' }
#' @seealso \code{\link{add_flags}}
#'
#' @export
format_gq <- function(indf, source=NULL, config=NULL, quiet=FALSE, ...) {
    UseMethod("format_gq", indf)
}

#' @export
#' @rdname format_gq
format_gq.default <- function(indf, source=NULL, config=NULL, quiet=FALSE, ...) {
    stop("Please provide a data.frame object as input", call. = FALSE)
}

#' @export
#' @rdname format_gq
format_gq.data.frame <- function(indf, source=NULL, config=NULL, quiet=FALSE, ...) {

    # Parse input object type
    gq_check_df(indf)

    # Mapping via 'source'
    if (!(is.null(source))) {
        match.arg(source, sources_list)
        if (!(quiet)) message(c("Mapping according to ", source, " format"))
        new_fields = gq_get_source(source)
    # Mapping via 'config'
    } else if (!(is.null(config))) {
        if (!(quiet)) message("Mapping according to config object")
        new_fields = gq_parse_config(config)
    # Mapping via individual parameters
    } else {
        if (!(quiet)) message("Mapping via individual parameters")
        new_fields = gq_parse_args(list(...))
    }

    # Apply the transformation
    if (!(is.null(new_fields$decimalLatitude)) && new_fields$decimalLatitude != "decimalLatitude") {
        if (new_fields$decimalLatitude %in% names(indf)) {
            if ("decimalLatitude" %in% names(indf)) {
                names(indf)[names(indf)=="decimalLatitude"] <- "decimalLatitude::original"
                if (!(quiet)) message("Changed \"decimalLatitude\" to \"decimalLatitude::original\"")
            }
            names(indf)[names(indf)==new_fields$decimalLatitude] <- "decimalLatitude"
            if (!(quiet)) message(c("Changed \"",new_fields$decimalLatitude,"\" to \"decimalLatitude\""))
        }
    }
    if (!(is.null(new_fields$decimalLongitude)) && new_fields$decimalLongitude != "decimalLongitude") {
        if (new_fields$decimalLongitude %in% names(indf)) {
            if ("decimalLongitude" %in% names(indf)) {
                names(indf)[names(indf)=="decimalLongitude"] <- "decimalLongitude::original"
                if (!(quiet)) message("Changed \"decimalLongitude\" to \"decimalLongitude::original\"")
            }
            names(indf)[names(indf)==new_fields$decimalLongitude] <- "decimalLongitude"
            if (!(quiet)) message(c("Changed \"",new_fields$decimalLongitude,"\" to \"decimalLongitude\""))
        }
    }
    if (!(is.null(new_fields$countryCode)) && new_fields$countryCode != "countryCode") {
        if (new_fields$countryCode %in% names(indf)) {
            if ("countryCode" %in% names(indf)) {
                names(indf)[names(indf)=="countryCode"] <- "countryCode::original"
                if (!(quiet)) message("Changed \"countryCode\" to \"countryCode::original\"")
            }
            names(indf)[names(indf)==new_fields$countryCode] <- "countryCode"
            if (!(quiet)) message(c("Changed \"",new_fields$countryCode,"\" to \"countryCode\""))
        }
    }
    if (!(is.null(new_fields$scientificName)) && new_fields$scientificName != "scientificName") {
        if (new_fields$scientificName %in% names(indf)) {
            if ("scientificName" %in% names(indf)) {
                names(indf)[names(indf)=="scientificName"] <- "scientificName::original"
                if (!(quiet)) message("Changed \"scientificName\" to \"scientificName::original\"")
            }
            names(indf)[names(indf)==new_fields$scientificName] <- "scientificName"
            if (!(quiet)) message(c("Changed \"",new_fields$scientificName,"\" to \"scientificName\""))
        }
    }

    indf
}

sources_list <- c(
    "rgbif",
    "rvertnet",
    "rinat"
)

gq_get_source <- function(source) {
    gq_sources <- list(
        rgbif=list(
            decimalLatitude="decimalLatitude",
            decimalLongitude="decimalLongitude",
            countryCode="countryCode",
            scientificName="name"
        ),
        rvertnet=list(
            decimalLatitude="decimallatitude",
            decimalLongitude="decimallongitude",
            countryCode="countrycode",
            scientificName="scientificname"
        ),
        rinat=list(
            decimalLatitude="latitude",
            decimalLongitude="longitude",
            countryCode=NULL,
            scientificName="scientific_name"
        )
    )
    return(gq_sources[[source]])
}

gq_parse_config <- function(config){
    if (!("decimalLatitude" %in% names(config))) stop("\"decimalLatitude\" missing from configuration object")
    if (!("decimalLongitude" %in% names(config))) stop("\"decimalLongitude\" missing from configuration object")
    if (!("countryCode" %in% names(config))) stop("\"countryCode\" missing from configuration object")
    if (!("scientificName" %in% names(config))) stop("\"scientificName\" missing from configuration object")
    return(config)
}

gq_parse_args <- function(args) {
    gq_args = list()
    if ("decimalLatitude" %in% names(args)) {
        gq_args$decimalLatitude=args$decimalLatitude
    } else {
        gq_args$decimalLatitude=NULL
    }
    if ("decimalLongitude" %in% names(args)) {
        gq_args$decimalLongitude=args$decimalLongitude
    } else {
        gq_args$decimalLongitude=NULL
    }
    if ("countryCode" %in% names(args)) {
        gq_args$countryCode=args$countryCode
    } else {
        gq_args$countryCode=NULL
    }
    if ("scientificName" %in% names(args)) {
        gq_args$scientificName=args$scientificName
    } else {
        gq_args$scientificName=NULL
    }
    return(gq_args)
}
