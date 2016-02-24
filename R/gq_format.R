#' Rename fields and modify content type to fit the geospatial quality API
#' format
#'
#' \code{gq_format} performs two actions on the provided data frame. First, it
#' attempts to change the names of certain fields (coordinates, country code and
#' scientific name) for the ones accepted by the API. Second, it parses the
#' content of these fields and modifies the format of the values if needed (e.g.
#' turns coordinates into floating point numbers).
#'
#' @param indf Required. Data frame containing a row per record
#'
#' @return The provided data frame with names changed to fit the geospatial
#'   quality API format
#'
#' @examples
#' records <- gq_format(records)
#'
#' @seealso \code{\link{get_multi}}
#'
#' @export
gq_format <- function(indf) {
    if(is.na(indf)) stop("Missing input data frame")
}
