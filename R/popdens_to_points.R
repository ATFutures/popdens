#' Assign raster cell values to points
#'
#' @param ras Raster dataset from which cell values are taken
#' @param nodes Points onto which cells will be distributed
#' @return A copy of \code{nodes} containing a new population variable (pop)
#' @export
#' @examples
#' \dontrun{
#' devtools::install_github("robinlovelace/osmdata")
#' library(osmdata)
#' library(raster)
#' library(tmap)
#' library(sf)
#' 
#' boundary_bb = getbb("Accra")
#' boundary = stplanr::bb2poly(boundary_bb) %>% 
#'   sf::st_as_sf()
#' # from inside the who-data repo
#' old = setwd("~/ATFutures/who-data/")
#' ras = raster::raster("accra/popdens/GHA15_040213.tif")
#' setwd(old)
#' ras # 200k cells
#' # ras = mask(ras, as_Spatial(st_geometry(boundary)))
#' ras = crop(ras, extent(boundary_bb)) # make smaller dataset - 148k cells
#' ways = readRDS("../who-data/accra/osm/accra-hw.Rds")
#' nodes = sf::st_cast(ways, "POINT")
#' nodes_new = pop2point(ras, nodes)
#' }
pop2point = function(ras, nodes) {
  pd_sf = ras %>% 
    raster::rasterToPolygons() %>% 
    sf::st_as_sf()
  pd_sf$id = 1:nrow(pd_sf)
    nodes_joined = sf::st_join(nodes, pd_sf) 
    # nodes_agg = aggregate(pd_sf, nodes, mean) # works but how to divide them again?
    nodes_aggregated = nodes_joined %>% 
      sf::st_set_geometry(NULL) %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(pop = mean(GHA15_040213, na.rm = T) / sum(!is.na(GHA15_040213)))
    # sum(pd_sf$GHA15_040213)
    # sum(nodes_joined$GHA15_040213, na.rm = T) # higher
    # sum(nodes_aggregated$pop, na.rm = T) # too few
    nodes_new = dplyr::inner_join(nodes_joined, nodes_aggregated)
    # sum(nodes_new$pop, na.rm = T) # 1/3 less - why?
    return(nodes_new)
}
