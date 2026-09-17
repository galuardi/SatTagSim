
#' Diagnostic ggplots of advection
#'
#' @param allpar data frame returned from \code{\link{get.allpar}}
#' @param save should the plot be saved
#' @param fname filename
#' @param ulims plot limits in u (x) direction
#' @param vlims plot limits in v (y) direction
#' @param ... additional arguements to ggsave
#'
#' @return a ggplot
#' @export
#' @rawNamespace export(plot.uv.density)
#'
#' @examples
#' see vignette
plot.uv.density <- function(allpar, save = FALSE, fname = NULL, ulims = c(-50, 50), vlims = c(-50, 50), ...) {
  usd   <- tapply(allpar$u, allpar$Month, sd, na.rm = TRUE)
  umean <- tapply(allpar$u, allpar$Month, mean, na.rm = TRUE)
  vsd   <- tapply(allpar$v, allpar$Month, sd, na.rm = TRUE)
  vmean <- tapply(allpar$v, allpar$Month, mean, na.rm = TRUE)

  # Generate simulated values per month (1 to 12)
  sim_list <- lapply(seq_len(12), function(m) {
    u_m <- if (is.finite(umean[as.character(m)]) && is.finite(usd[as.character(m)])) {
      truncnorm::rtruncnorm(1000, a = -50, b = 50, umean[as.character(m)], usd[as.character(m)])
    } else {
      rep(NA_real_, 1000)
    }
    v_m <- if (is.finite(vmean[as.character(m)]) && is.finite(vsd[as.character(m)])) {
      truncnorm::rtruncnorm(1000, a = -50, b = 50, vmean[as.character(m)], vsd[as.character(m)])
    } else {
      rep(NA_real_, 1000)
    }
    data.frame(u = u_m, v = v_m, month = m)
  })

  myd <- do.call(rbind, sim_list)
  myd <- myd[!is.na(myd$u) & !is.na(myd$v), ]
  myd$labs <- factor(myd$month, levels = 1:12, labels = month.name)

  p1 <- ggplot2::ggplot(myd, ggplot2::aes(x = u, y = v)) +
    ggplot2::stat_density2d(ggplot2::aes(fill = ggplot2::after_stat(level)), geom = "polygon") +
    ggplot2::coord_cartesian(xlim = ulims, ylim = vlims) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "salmon") +
    ggplot2::geom_hline(yintercept = 0, lty = 2, col = 2) +
    ggplot2::geom_vline(xintercept = 0, lty = 2, col = 2) +
    ggplot2::facet_wrap(~labs, nrow = 3)


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(u = xx, v = yy))
}

dat=circleFun(c(0,0), diameter=20, npoints=100)
p2 = p1+geom_path(data=dat, mapping=aes(u,v))
dat=circleFun(c(0,0), diameter=10, npoints=100)
p2 = p2+geom_path(data=dat, mapping=aes(u,v))
dat=circleFun(c(0,0), diameter=5, npoints=100)
p2 = p2+geom_path(data=dat, mapping=aes(u,v))
p2 = p2+theme_bw()


if(save==T){
	# pdf(10,10, file=fname, ...)
	ggsave(p2, file=fname,...)
	# dev.off()
}else{
p2
}
}




