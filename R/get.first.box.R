#' Get first occupied area in a time frame
#'
#'
#' @param simdat list of simulated tracks from \code{\link{make.sim.track.par}}
#' @param syear start year; a dummy variable for date determination
#' @param boxes spatial polygons strata for transition determination
#' @param seas.len length of season. typically 90 days but can be adjusted for monthly or other time intervals
#'
#' @return data frame with Year season previous box (strata) matrix transition and current box (strata). nuumbers of previous box  and current box refer to the polygon order in the the boxes parameter
#' @export
#' @seealso \code{\link{get.trans.prob}} \code{\link{box7}}
#' @examples
#' see vignette
get.first.box <- function(simdat, syear = 2000, boxes = box7, seas.len = 90, nyears = 2) {
  names(simdat) <- seq_along(simdat)

  sim_clean <- lapply(simdat, function(df) {
    df <- as.data.frame(df)
    if (ncol(df) >= 3) names(df)[1:3] <- c("lon", "lat", "Month")
    df
  })

  datbox <- dplyr::bind_rows(sim_clean, .id = "TagID") |>
    dplyr::mutate(TagID = as.numeric(TagID)) |>
    dplyr::group_by(TagID) |>
    dplyr::mutate(
      Step = dplyr::row_number(),
      Year = syear + floor((Step - 1) / 360)
    ) |>
    dplyr::slice(seq(1, dplyr::n(), by = seas.len)) |>
    dplyr::ungroup()

  spatial_subset <- datbox |>
    dplyr::select(lon, lat, dplyr::any_of(c("Month", "season")))

  box_results <- get.box.vals(as.data.frame(spatial_subset), boxes = boxes)
  datbox$cbox <- box_results$box

  flbox <- datbox |>
    dplyr::group_by(TagID) |>
    dplyr::mutate(
      pbox = dplyr::lag(cbox, default = dplyr::first(cbox)),
      btrans = cbox - pbox
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(season = Month) |>
    dplyr::select(Year, season, TagID, pbox, btrans, cbox) |>
    as.data.frame()

  flbox
}
