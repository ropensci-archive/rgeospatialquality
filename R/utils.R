BASE_URL <- "http://api-geospatial.vertnet-portal.appspot.com/geospatial"

gq_check_df <- function(indf) {
    if(is.na(indf) || (is.data.frame(indf) && nrow(indf) == 0)) stop("Input data frame missing or empty")
    if(!(is.data.frame(indf))) stop("Provided argument is not a data.frame")
    return(invisible())
}

gq_parse <- function(req) {
    if (req$status_code<400) {
        return(jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), simplifyVector=TRUE))
    } else {
        err_msg <- c("Something went wrong with the call. Got status ", req$status_code, ": ", httr::content(req, as="text"))
        stop(err_msg)
    }
}
