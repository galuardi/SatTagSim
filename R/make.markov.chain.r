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

make.markov.chain <- function(boxtrans, s.init = 3, sorder = rep(1:4, 1000)) {
  n_trans <- length(sorder)
  if (n_trans < 1) return(numeric(0))

  states <- seq_len(nrow(boxtrans[[1]]))
  schain <- numeric(length = n_trans)
  schain[1] <- s.init

  for (i in 2:n_trans) {
    next_idx <- if (i < n_trans) i + 1 else 1
    possible.states <- states[rowSums(boxtrans[[sorder[next_idx]]]) != 0]
    if (length(possible.states) == 0) possible.states <- states

    svec <- boxtrans[[sorder[i]]][schain[i - 1], possible.states]
    prob_sum <- sum(svec)
    if (is.na(prob_sum) || prob_sum == 0) {
      schain[i] <- schain[i - 1]
    } else {
      schain[i] <- sample(possible.states, 1, prob = svec)
    }
  }
  schain
}


