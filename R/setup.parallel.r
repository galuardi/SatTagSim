#' Setup parallel processing
#' Setup parallel processing for simulations
#'
#' @return multicore options for prallel processing
#' @export
#'
#' @examples
#' mcoptions = setup.parallel()
#'
setup.parallel <- function(){
  n.cores <- detectCores()
  if(n.cores==1) registerDoSEQ() else registerDoParallel(cores=n.cores) # multicore functionality
  mcoptions <- list(preschedule=TRUE)
  cat(paste("\nUsing",n.cores,"cores for parallel processing."))
  mcoptions
}

