#' popdens.
#'
#' @name popdens
#' @docType package
#' @author Mark Padgham, Robin Lovelace
#' @importFrom here here
#' @importFrom dodgr dodgr_vertices weight_streetnet
#' @importFrom dplyr group_by inner_join select summarise
#' @importFrom magrittr %>% extract2
#' @importFrom osmdata add_osm_feature getbb opq osmdata_sf trim_osmdata
#' @importFrom osmdata osm_poly2line
#' @importFrom raster crop extent raster projectRaster writeRaster
#' @importFrom sf st_as_sf st_centroid st_distance st_join st_point st_sfc
#' @importFrom stplanr bb2poly
#' @importFrom utils packageVersion setTxtProgressBar
#' @importFrom utils txtProgressBar unzip
NULL
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL
