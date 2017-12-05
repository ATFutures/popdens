#' crop_worldpop_tif
#'
#' \code{.zip} files downloaded from worldpop are national-scale. This function
#' crops them to the designated cities, and converts them to much smaller geotif
#' files.
#'
#' @param city Name of city (one of "accra", "kathmandu", or "bristol")
#' @param expand Relative factor by which to expand cropping limits beyond
#' rectangular bounding box of city
#' @return Nothing Files are written to the corresponding directories
#'
#' @note This should be run from somewhere within the \code{whodata} repository
#' structure; anywhere else will likely fail.
#'
#' @export
crop_worldpop_tif <- function (city = "accra", expand = 0.1)
{
    bb <- osmdata::getbb (city)
    bb_exp <- t (apply (bb, 1, function (i)
                        i + c (-expand, expand) * diff (i)))
    wd <- file.path (here::here (), city, "popdens")
    files <- list.files (wd)
    files <- file.path (wd, files [grep ("\\.zip", files)])

    if (length (files) == 0)
    {
        message ("No files to convert for ", city)
        return (NULL)
    }

    pb <- txtProgressBar (style = 3)
    count <- 1
    n <- length (files)
    for (f in files)
    {
        zfiles <- unzip (f, list = TRUE)$Name
        f1 <- zfiles [grep ("tif$", zfiles, ignore.case = TRUE)]
        unzip (f, files = f1)
        r <- raster::raster (f1) %>% raster::crop (bb_exp)
        f0 <- paste0 (tools::file_path_sans_ext (f1), ".zip")
        file.remove (file.path (wd, f0))
        file.remove (f1) # the full geotif dumped in current wd
        # faster convert file names to lower case, so
        f1 <- paste0 (tools::file_path_sans_ext (f1), ".tif")
        f1 <- file.path (wd, f1)
        raster::writeRaster (r, f1)
        setTxtProgressBar (pb, count / n)
        count <- count + 1
    }
    close (pb)
}

#' crop_global_tif
#'
#' There are no worldpop data for Europe, but the EC Joint Research Centre Data
#' Catalogue offers 250m resolution global population density \code{tif} files.
#' These are huge, and presumed to be downloaded into the root directory of this
#' project. This function crops them to designated cities (actuall only Bristol
#' at present) and deposits the results in the appropriate \code{[city]/popdens}
#' directory.
#'
#' @param city Name of city (one of "accra", "kathmandu", or "bristol")
#' @param expand Relative factor by which to expand cropping limits beyond
#' rectangular bounding box of city
#' @return Nothing Files are written to the corresponding directories
#'
#' @note This should be run from somewhere within the \code{whodata} repository
#' structure; anywhere else will likely fail. It also takes a r-e-a-l-l-y long
#' time because of the raster re-projection.
#'
#' @export
crop_global_tif <- function (city = "bristol", expand = 0.1)
{
    if (city != "bristol")
        stop ("currently hard-coded for bristol only")

    bb <- osmdata::getbb (city)
    bb_exp <- t (apply (bb, 1, function (i)
                        i + c (-expand, expand) * diff (i)))

    globes <- list.files ()
    globes <- globes [grep ("\\.zip", globes)]

    zfiles <- unzip (globes, list = TRUE)$Name
    f1 <- zfiles [grep ("tif$", zfiles, ignore.case = TRUE)]
    unzip (globes, files = f1)

    # This file is in World Molleweide (EPSG:54009) projection, for which the
    # rough bounds of bristol can be obtained from
    # https://epsg.io/map#srs=54009&x=-188375.09900285&y=6060835.47073653&z=7
    # These are used to first crop the map in this projection, then re-project
    # the cropped version, and then crop again to the nominatim EPSG84 bounds.
    bb_moll <- c (-230000.0, -160000.0, 6000000.0, 6060000.0)
    bb_moll <- c (-205000.0, -180000.0, 6015000.0, 6040000.0)
    message ("cropping global raster to Bristol ... ", appendLF = FALSE)
    r <- raster::raster (f1) %>% raster::crop (matrix (bb_moll, nrow = 2))
    crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    message ("done\nRe-projecting to WGS84 (this will take a long time!) ... ",
             appendLF = FALSE)
    r <- raster::projectRaster (r, crs = crs) %>% raster::crop (bb_exp)
    message ("done")

    fnew <- file.path (here::here (), city, "popdens")
    if (!file.exists (fnew))
        dir.create (fnew)
    fnew <- file.path (fnew, basename (f1))
    raster::writeRaster (r, fnew)

    invisible (file.remove (f1))
    invisible (file.remove (dirname (f1)))
}
