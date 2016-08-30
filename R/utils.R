BASE_URL <- "http://api-geospatial.vertnet-portal.appspot.com/geospatial"

gq_check_df <- function(indf) {
    if(nrow(indf) == 0) stop("Input data frame missing or empty")
    return(invisible())
}

gq_parse <- function(req) {
    if (req$status_code<400) {
        resp <- httr::content(req, as="text", encoding="UTF-8")
        resp <- jsonlite::fromJSON(resp, simplifyVector=TRUE)

        # Catch when API is offline
        if ("status" %in% names(resp) && resp$status=="offline"){
            warning(resp$message)
            return(NULL)
        }
        return(resp)
    } else {
        err_msg <- c("Something went wrong with the call. Got status ", req$status_code, ": ", httr::content(req, as="text"))
        warning(err_msg)
        return(NULL)
    }
}
