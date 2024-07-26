#' Get checklist of native plants around given coordinates
#'
#' Get a checklist of the native vascular plants around a given point in
#' the Iberian Peninsula
#'
#' @param lon Numeric. Longitude in geographic coordinates
#' @param lat Numeric. Latitude in geographic coordinates
#'
#' @return A dataframe
#'
#' @note As the original data (AFLIBER database) have 10-km resolution, the
#' resulting checklist may include taxa present within 10 km distance of the point or
#' polygon.
#' @export
#'
#' @examples
#' head(get_checklist(lon = -5, lat = 40))
#'
get_checklist <- function(lon = NULL, lat = NULL) {

  ## Check arguments ##
  stopifnot(is.numeric(lon))
  stopifnot(is.numeric(lat))
  stopifnot(lon > -9.5)
  stopifnot(lon < 4.34)
  stopifnot(lat > 36.0)
  stopifnot(lat < 43.8)

  ## Transform to sf
  zone <- data.frame(lon = lon, lat = lat)
  zone <- sf::st_as_sf(zone, coords = c("lon", "lat"), crs = 4326)

  ## Load spp distributions
  distr <- sf::st_as_sf(locs, coords = c("lng", "lat"), crs = 4326)


  ## Find grid points to be included
  # grid.sf is an internal dataset with the coordinates of all the unique points
  # Here we find all points within 7 km of the given point or polygon
  # sqrt(5000^2 + 5000^2) = 7071 m  (maximum distance from corner to 10x10 km grid cell centre)
  pts.near <- suppressMessages(
    unlist(nngeo::st_nn(zone, grid.sf, k = nrow(grid.sf), maxdist = 7000,
                        progress = FALSE))
  )


  if (length(pts.near) < 1) {
    message("These coordinates could not be matched to any grid cell. Please check them")
  }

  if (length(pts.near) > 0) {

    pts.sf <- grid.sf[pts.near, ]  # points to get taxa from

    requireNamespace("dplyr", quietly = TRUE)  # st_filter seems to require dplyr
    spp <- sf::st_filter(distr, pts.sf)   # filter Distributions dataset to those points only

    spp <- sf::st_drop_geometry(spp)
    # spp <- subset(spp)
    spp <- unique.data.frame(spp)

    return(spp)

  }

}


