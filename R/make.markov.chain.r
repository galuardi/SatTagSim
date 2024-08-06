#' Generate Markov chains from a set of transition matrices

#' @param s.init starting area
#' @param sorder order of the seasons. 1-4 is winter, spring, summer and fall
#' @param boxtrans list of transition matrices. Ususally this will be seasonal; winter, spring, summer and fall
#' @return a vector of area categories corresponding to the boxes used in the simulation phase
#' @export
#' @details need to make sure not to get into an impossible scenario where the prob is zero of moving out of the current area... this happens for fish going into the Med from the West
#'
#' @examples
#' # simulate 100 chains (fish) for 100 cycles (years) with randomized starting seasons
#' xx = sapply(sample(1:4, 100, replace = T ), function(x) make.markov.chain(boxtrans, s.init = x, sorder = rep(1:4, 1000)))
#'
#' # plot two chains (fish) for 50 cycles (years)
#' image.plot(1:200, 1:2,(xx[1:200,1:2]), nlevel = 7, zlim = c(1,7), xlab = 'cycle', ylab = 'chain (fish)', axes = T)
#' barplot(table(xx[1:200,]))

make.markov.chain <-
  function(boxtrans = boxtrans, s.init = 3,  sorder = rep(1:4, 1000)) {
    states = 1:nrow(boxtrans[[1]])
    # sorder = rep(1:4, 1000) # order of the seasons. 1-4 is winter, spring, summer and fall
    schain = numeric(length = length(sorder))
    schain[1] = s.init
    for (i in 2:length(sorder)) {
      step = ifelse((i != 4000), 1,-3999)
      possible.states = states[which(rowSums(boxtrans[[sorder[i + step]]])!=0)]
      svec = boxtrans[[sorder[i]]][schain[i - abs(step)], possible.states]
      schain[i] = sample(possible.states, 1, prob = svec)
    }
    schain
  }


