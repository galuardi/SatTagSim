#' Transition probability matrix generator
#'
#' This function is designed to run on a data frame of many simulated tracks. It loops through each one to get transitions and sums the probabilities for the entire set of simulated tracks. This function uses the first area occupied at the beginning of the time period, and transitions to the first occupied area in the next time period.
#'
#' @param datbox data frame returned from \code{\link{get.first.box}}
#' @param nyears dummy variable that sets up a dataframe to be filled. Should be greater than the number of years simulated
#' @param adims array dimensions. For a 7-box area and 4 seasons, c(7, 7, 4)
#' @param perc logical. Return results as a percentage of the row total
#'
#' @return a list of transition probabilities for each time period. The row index represents the previous area and the column represnts the current area for the given time period.
#' @export
#'
#' @examples
#' see vignette

get.trans.prob <-  function (datbox, nyears = 100, adims = c(7, 7, 4), perc = T, ...)
{

  fillone <- function(trans)
  {
    n = nrow(trans)
    for(i in 1:n){
      trans[i,i] = ifelse(sum(trans[i,])==0, 1, trans[i,i])
    }
    trans
  }

  if(adims[3] == 4) {
    dname3 =  c("Winter", "Spring", "Summer", "Fall")
  }else {
    dname3 = as.character(adims[3])
  }

  allmat = array(
    0, dim = c(adims), dimnames = list(
      as.character(1:adims[1])
    , as.character(1:adims[2])
    , dname3
    )
  )


  tab = table(datbox$pbox, datbox$cbox, datbox$season)

  for(i in 1:dim(allmat)[3]){
    ridx = match(dimnames(tab[,,i])[[1]], dimnames(allmat[,,i])[[1]])
    cidx = match(dimnames(tab[,,i])[[2]], dimnames(allmat[,,i])[[2]])
    allmat[ridx, cidx, i] = tab[,,i]
  }

  mmat = allmat

  if(perc==F){
    tmat = alply(mmat, 3, function(x) x)
  }else{
    tmat = alply(mmat, 3, function(x) x / rowSums(x))
    for (i in 1:4){
      tmat[[i]][is.nan(tmat[[i]])] = 0
    }
    tmat = lapply(tmat, fillone)
  }
  tmat
}
