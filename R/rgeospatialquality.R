#' rgeospatialquality: A wrapper for the Geospatial Quality REST API for primary
#' biodiversity data
#'
#' This package provides R-native access to the methods of the Geospatial
#' Quality API
#'
#' @section Information:
#'
#'   \code{\link{flags}} - Information on the input and output of the Geospatial
#'   Quality API
#'
#' @section Functions:
#'
#'   \describe{
#'
#'   \item{\code{\link{parse_record}}}{Performs the assessment of a single
#'   record}
#'
#'   \item{\code{\link{add_flags}}}{Performs the assessment of a data.frame with
#'   one or more records}
#'
#'   \item{\code{\link{format_gq}}}{Adapts the supplied data.frame to the needs
#'   of the Geospatial Quality API}
#'
#'   }
#'
#' @section Vignettes (published in RPubs):
#'
#'   \href{http://rpubs.com/jotegui/rgeospatialquality_paper}{Transcription of
#'   the scientific paper describing the Geospatial Quality API}
#'
#'   \href{http://rpubs.com/jotegui/rgeospatialquality_rgbif}{Using
#'   \code{rgeospatialquality} together with \code{rgbif}}
#'
#' @docType package
#' @name rgeospatialquality
#' @importFrom httr POST GET content
#' @importFrom jsonlite toJSON
#' @importFrom plyr compact
NULL
