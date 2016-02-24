#' Calculate flags for a set of records
#'
#' \code{get_multi} calls the \code{POST} method of the API in order to extract
#' the flags for a set of records
#'
#' @param indf Required. Properly formatted data frame containing a row per
#'   record (see below)
#'
#' @return The provided data frame with the quality flags added as new columns
#'
#' @seealso \code{\link{gq_format}}, \code{\link{get_single}}
#'
#' @export
get_multi <- function(indf) {
    if(is.na(indf)) stop("Missing input data frame")
}
