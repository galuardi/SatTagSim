#' Make spatio-temporal array of movement parameters
#' This function generates monthly advection and diffusion mean and standard deviation for each area in the spatial strata used. Missing months default to the previous months value. Missing areas should be filled in by the user.
#'
#' @param tracks spatial points data frame of tracks
#' @param inbox spatial polygons (shapefile) of areas
#' @param rasbox optional raster version of spatial polygons for areas. If included, rrows and rcols may be NULL
#' @param rrows number of rows for raster creation
#' @param rcols number of columns for raster creation
#' @param use_wts T/F for using weights
#' @param missvec vector of missing rows (areas) in final output. This can be obtained by running once with this function left as NULL
#' @param fillvec vector of replacement rows for missvec. This is a subjectie decision by the user and is dependent on the spatial strata used
#'
#' @return
#' @details
#' The default size raster created is 130 degrees latitude and 145 degrees longitude, with 5 degree cells. This should be specific to the spatial strata used (e.g. the 11 box model) and should ideally have cells that split the areas along ploygon lines.
#'
#' @examples
#' @export
make.par.array <- function(tracks = nsfish, inbox = box7, rasbox = NULL, rrows = 26*5, rcols = 29*5, use_wts = NULL, missvec = NULL, fillvec = NULL){
  modern_process_tracks(
    tracks   = tracks,
    inbox    = inbox,
    rasbox   = rasbox,
    rrows    = rrows,
    rcols    = rcols,
    use_wts  = use_wts,
    missvec  = missvec,
    fillvec  = fillvec
  )
}


