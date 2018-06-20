#' Assign raster cell values to points
#'
#' @param city City for which data are to be obtained
#' @param save_data Should data be saved to \code{who-data}?
#' @param quiet If \code{FALSE}, dump progress information to screen.
#' @return An \pkg{sf} \code{data.frame} containing OSM nodes, geometries (as
#' point objects), and aggregated population densities projected onto each node.
#' @export
#' @examples
#' \dontrun{
#' nodes <- pop2point (city = "kathmandu")
#' }
pop2point <- function (city, save_data = TRUE, quiet = FALSE)
{
    if (!"package:sf" %in% search ())
        stop ("Please load sf into workspace before proceeding")

    boundary_bb <- osmdata::getbb (city)

    city <- tolower (city)
    if (!city %in% c ("kathmandu", "accra"))
        stop ("This currently works only for kathmandu and accra")

    data_dir <- file.path (dirname (here::here ()), "who-data", city)
    if (city == "accra")
        ras <- raster::raster(file.path (data_dir, "popdens",
                                         "GHA15adj_040213.tif"))
    else
        ras <- raster::raster(file.path (data_dir, "popdens",
                                         "NPL_ppp_2020_adj_v2.tif"))

    ras <- raster::crop (ras, raster::extent (boundary_bb))
    ways <- readRDS (file.path (data_dir, "osm", paste0 (city, "-hw.Rds")))

    if (!quiet)
        message ("weighting streetnet ... ", appendLF = FALSE)
    graph <- dodgr::weight_streetnet (ways)
    nodes <- dodgr::dodgr_vertices (graph)
    osm_ids <- nodes$id # only used to check below that all worked
    #nodes <- nodes [which (nodes$component == 1), ] # connected components only
    #nodes$component <- NULL
    # then convert to sf using internal xy_to_sfc function
    if (!quiet)
        message ("done.\nconverting nodal coordinates to sf points ... ",
                 appendLF = FALSE)
    nodes <- sf::st_sf (osm_id = nodes$id,
                        geometry = xy_to_sfc (nodes),
                        crs = 4326)
    if (!quiet)
        message ("done.\nassigning population density points to nodes ... ",
                 appendLF = FALSE)
    nodes_new <- assign_points (ras, nodes)
    if (!quiet)
        message ("done.")

    # ensure that everything worked:
    if (!all (nodes$osm_id %in% osm_ids))
        stop ("Something went wrong: Not all nodes in population projection ",
              "exist in the street network")


    if (save_data)
        saveRDS (nodes_new, file = file.path (data_dir, "osm",
                                              "popdens_nodes.Rds"))
    invisible (nodes_new)
}

assign_points <- function (ras, nodes, redistribute_missing = "")
{
    pd_sf <- ras %>%
        raster::rasterToPolygons() %>%
        sf::st_as_sf()
    pd_sf$id <- 1:nrow(pd_sf)
    nodes_joined <- sf::st_join(nodes, pd_sf)
    # nodes_agg <- aggregate(pd_sf, nodes, mean)
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
    if (redistribute_missing == "nearest") {
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
        for (i in seq_along(pd_sf_missing$id)) {
            d <- sf::st_distance(nodes_new, pd_sf_missing_points$geometry[i])
            nodes_new$pop[which.min(d)] <-
                nodes_new$pop[which.min(d)] +
                pd_sf_missing_points [[layer_name]] [i]
        }

        # raster attempt...
        # ras_masked = raster::mask(ras, nodes)
        # ras_agg = raster::aggregate(ras, fact = 2, fun = mean, na.rm = TRUE)
        #
        # nodes_new_redist = sf::st_join(nodes_new,
        #                                pd_sf_missing,
        #                                st_is_within_distance,
        #                                dist = 1000)
    }
        return(nodes_new)
}


#' xy_to_sfc
#'
#' Convert matrix of xy points to sfc object
#'
#' @param xy matrix, data.frame, or tibble of points
#' @return sf::sfc representation of same
#' @export
xy_to_sfc <- function (xy)
{
    x <- y <- NULL
    xy <- dplyr::select (xy, c (x, y)) %>%
        as.matrix ()
    sapply (seq (nrow (xy)), function (i)
            sf::st_point (xy [i, ]) %>%
                sf::st_sfc ()) %>%
    sf::st_sfc ()
}
