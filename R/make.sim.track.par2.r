#' Simulate tracks in parallel (Legacy wrapper)
#'
#' @inheritParams sim_tracks_par
#' @export
#' @author Benjamin Galuardi
#' @examples
#' see vignette
make.sim.track.par2 <- function(par_array = par_array, simorder = simorder, sp = spts, bath = bath, sstmat = sstmat, boxmat = boxmat, seaslen = 30, sstol = 2, mcoptions = setup.parallel(), ...) {
  sim_tracks_par(par_array = par_array, simorder = simorder, sp = sp, bath = bath, sstmat = sstmat, boxmat = boxmat, seaslen = seaslen, sstol = sstol, mcoptions = mcoptions, ...)
}
