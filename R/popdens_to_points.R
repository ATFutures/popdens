#' Assign raster cell values to points
#'
#' @param ras Raster dataset from which cell values are taken
#' @param nodes Points onto which cells will be distributed
#' @param redistribute_missing How should empty cells be redistributed?
#' To the nearest node by default.
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
#' city <- "accra"
#' boundary_bb = getbb(city)
#' boundary = stplanr::bb2poly(boundary_bb) %>% 
#'   sf::st_as_sf()
#' data_dir <- file.path ("..", "who-data", city)
#' ras <- raster::raster(file.path (data_dir, "popdens",
#'          "NPL_ppp_v2c_2015.tif"))
#' # or  "GHA15_040213.tif" for Ghana
#' ras <- crop(ras, extent(boundary_bb)) # make smaller dataset - 148k cells
#' ways <- readRDS(file.path (data_dir, "osm", paste0 (city, "-hw.Rds")))
#' graph <- dodgr::weight_streetnet (ways)
#' nodes <- dodgr::dodgr_vertices (graph)
#' # filter out all unconnected components
#' nodes <- nodes [which (nodes$component == 1), ]
#' nodes$component <- NULL
#' # then convert to sf using internal xy_to_sfc function
#' nodes <- sf::st_sf (osm_id = nodes$id, geometry = xy_to_sfc (nodes))
#' # need to set CRS:
#' sf::st_crs (nodes) <- 4326
#' nodes_new = pop2point(ras, nodes)
#' }
pop2point = function(ras, nodes, redistribute_missing = NULL)
{
    pd_sf <- ras %>% 
        raster::rasterToPolygons() %>% 
        sf::st_as_sf()
    pd_sf$id <- 1:nrow(pd_sf)
    nodes_joined = sf::st_join(nodes, pd_sf) 
    # nodes_agg = aggregate(pd_sf, nodes, mean)
    # that works but how to divide them again?
    layer_name <- names (nodes_joined) [!names (nodes_joined) %in%
                                        c ("osm_id", "id", "geometry")]

    # with nodes now all vertices with osm_id values courtesy of dodgr, the
    # desired values are simply
    nodes_new <- nodes_joined
    nodes_new$pop <- nodes_new [[layer_name]] /
        sum (nodes_new [[layer_name]], na.rm = TRUE)

    # and all of this becomes redudnant, right?
    # nodes_aggregated <- nodes_joined %>% 
    #     sf::st_set_geometry(NULL) %>% 
    #     dplyr::group_by(osm_id) %>%
    #     dplyr::summarise(pop = mean(nodes_joined [[layer_name]], na.rm = T) /
    #                      sum(!is.na(nodes_joined [[layer_name]])))

    # sum(pd_sf [[layer_name]])
    # sum(nodes_joined [[layer_name]], na.rm = T) # higher
    # sum(nodes_aggregated$pop, na.rm = T) # too few
    # nodes_new <- dplyr::inner_join(nodes_joined, nodes_aggregated)
    # sum(nodes_new$pop, na.rm = T) # 1/3 less - why?
    # https://github.com/r-spatial/sf/issues/200

    # This should never be needed
    if(redistribute_missing == "nearest") {
        sel_missing <- !pd_sf$id %in% nodes_new$id
        pd_sf_missing <- pd_sf[sel_missing, ]
        pd_sf_missing_points <- sf::st_centroid(pd_sf_missing)
        # they are equal - must redistribut all values in there:
        # (sum(pd_sf_missing_points [[layer_name]]) + 
        #  sum(nodes_new$pop, na.rm = T)) /
        # sum(values(ras [[layer_name]]), na.rm = T)
        # solution involving buffers (st_within_distance would also work on
        # projected data)
        # pd_sf_missing_buffer = stplanr::geo_buffer(pd_sf_missing, 1000)

        # works but is really slow...
        for(i in seq_along(pd_sf_missing$id)) {
            d <- st_distance(nodes_new, pd_sf_missing_points$geometry[i])
            nodes_new$pop[which.min(d)] <- 
                nodes_new$pop[which.min(d)] +
                pd_sf_missing_points [[layer_name]] [i]
        }

        # raster attempt...
        # ras_masked = raster::mask(ras, nodes)
        # ras_agg = raster::aggregate(ras, fact = 2, fun = mean, na.rm = TRUE)
        # 
        # nodes_new_redist = sf::st_join(nodes_new, pd_sf_missing, st_is_within_distance, dist = 1000)
    }
        return(nodes_new)
}


#' xy_to_sfc
#'
#' Convert matrix of xy points to sfc object
#'
#' @param xy matrix, data.frame, or tibble of points
#' @return sf::sfc representation of same
xy_to_sfc <- function (xy)
{
    xy <- dplyr::select (xy, c (x, y)) %>%
        as.matrix ()
    sapply (seq (nrow (xy)), function (i)
            sf::st_point (xy [i, ]) %>%
                sf::st_sfc ()) %>%
    sf::st_sfc ()
}

