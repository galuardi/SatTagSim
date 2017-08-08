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
get.first.box <- function(simdat, syear=2000, boxes=box7, seas.len = 90, nyears = 2){

  n = nrow(simdat[[1]])

  names(simdat) = 1:length(simdat)
  simdatsub = ldply(simdat, function(x) x[seq(1, n, by = seas.len),])[,2:4] # 90 days in a season
  datbox1 = get.box.vals(simdatsub, boxes = boxes)

  datbox1$TagID = as.vector(apply(data.frame(names(simdat)), 1, rep, n/seas.len)) # 90 days in a season
  years = syear:((syear+n/360)-1)
  datbox1$Year = as.vector(apply(data.frame(years), 1, rep, n/seas.len*nyears)) # 180 is the total of 2 seasons for each year
  datbox1$cbox = datbox1$box

  # flbox = ddply(datbox1, c('Year','season', 'TagID'), function(x) c(x$box[1]))

  # names(flbox)[4] = c('cbox')
  #==================================#
  # function that calculates movement in terms of changes from the previous area	i.e. box 4-->6 = 2, box 6-->4 = -2
  #==================================#
  get.btrans=function(x){
    nseas = nrow(x)
    c(0,diff(x$cbox))
  }

  flbox = ddply(datbox1, 'TagID', function(x) data.frame(x, btrans=get.btrans(x)))

  flbox$pbox = flbox$cbox-flbox$btrans

  flbox = flbox[,c('Year','season','TagID','pbox','btrans','cbox')]

    flbox
}
