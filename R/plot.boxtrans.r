#' plot.boxtrans
#'
#' @param boxtrans transition matrices returned from \code{\link{get.trans.prob}}
#' @param palette continuous color palette see \code{\link{RColorBrewer}}
#' @param text.col text color
#' @param text.size text size of cells
#' @param strip.text text size of panel strips
#' @param axis.text text size of axes
#'
#' @return a four panel, seasonal plot (ggplot)
#' @export
#' @rawNamespace export(plot.boxtrans)
#'
#' @examples
plot.boxtrans <- function(boxtrans, palette = "BrBG", text.col = 'white', text.size = 6, strip.text = 15, axis.text = 20){
  df_list <- lapply(seq_along(boxtrans), function(i) {
    df <- as.data.frame(as.table(boxtrans[[i]]))
    names(df) <- c("Var1", "Var2", "value")
    df$L1 <- as.character(i)
    df
  })
  bb <- do.call(rbind, df_list)
  bb$L1 <- factor(bb$L1, levels = as.character(seq_along(boxtrans)))
  if (length(boxtrans) == 4) {
    levels(bb$L1) <- c('Winter', 'Spring', 'Summer', 'Fall')
  }
  bb$Var1 <- as.factor(bb$Var1)
  bb$Var2 <- as.factor(bb$Var2)

  ggplot2::ggplot(bb, ggplot2::aes(x = Var2, y = Var1, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), col = text.col, fontface = 'bold', size = text.size) +
    ggplot2::scale_fill_distiller(palette = palette, direction = 1) +
    ggplot2::facet_wrap(~L1) +
    ggplot2::xlab('Ending Area') +
    ggplot2::ylab('Start Area') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text),
                   axis.title.x = ggplot2::element_text(size = axis.text),
                   axis.title.y = ggplot2::element_text(size = axis.text),
                   legend.title = ggplot2::element_blank())
}

# bb11 = melt(boxtrans11)
# bb11$seasons = c(sapply(1:4, function(x) rep(x, nrow(bb11)/4)))
# bb11$seasons = reorder(bb11$L1, bb11$seasons)
#
# ggplot(bb11, aes(x=Var2, y = Var1, fill = value))+
#   geom_tile()+
#   # geom_label(label = round(bb$value,2), colour = 'white', fontface = 'bold')+
#   geom_text(label = round(bb11$value,2), col = 'white', fontface = 'bold', size = 3)+
#   scale_fill_distiller(palette = "BrBG", direction = 1)+
#   facet_wrap(~seasons)+
#   xlab('End (Current)Area')+
#   ylab('Start (Previous) Area')+
#   theme(strip.text = element_text(size=15)
#         ,  axis.title.x = element_text(size = 20)
#         ,  axis.title.y = element_text(size = 20))

# plot.boxtrans(boxtrans)
