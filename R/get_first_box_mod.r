
#' Get First Box 
#'
#' @param simdat
#' @param syear
#' @param boxes
#' @param seas.len
#'
#' @returns
#'
#' @export
#' @examples
get_first_box_mod <- function(simdat, syear = 2000, boxes = box7, seas.len = 90) {
  
  names(simdat) <- seq_along(simdat)
  
  datbox <- dplyr::bind_rows(simdat, .id = "TagID") |> 
    dplyr::mutate(TagID = as.numeric(TagID)) |> 
    dplyr::group_by(TagID) |> 
    dplyr::mutate(
      Step = dplyr::row_number(),
      Year = syear + floor((Step - 1) / 360)
    ) |> 
    dplyr::slice(seq(1, dplyr::n(), by = seas.len)) |> 
    dplyr::ungroup()
  
  # Pass only the necessary spatial/temporal columns to prevent sp package matrix errors
  spatial_subset <- datbox |> 
    dplyr::select(lon, lat, dplyr::any_of(c("Month", "season")))
  
  box_results <- SatTagSim::get.box.vals(as.data.frame(spatial_subset), boxes = boxes)
  
  datbox$cbox <- box_results$box
  
  flbox <- datbox |> 
    dplyr::group_by(TagID) |> 
    dplyr::mutate(
      pbox = dplyr::lag(cbox, default = dplyr::first(cbox)),
      btrans = cbox - pbox
    ) |> 
    dplyr::ungroup() |> 
    # Add this line to officially convert the column name
    dplyr::rename(season = Month) |>
    # Strictly select the exact columns requested, which automatically drops 'Month'
    dplyr::select(Year, season, TagID, pbox, btrans, cbox)
  
  return(flbox)
}
