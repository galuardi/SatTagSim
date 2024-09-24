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
#'
#' @examples
plot.boxtrans <- function(boxtrans, palette = "BrBG", text.col = 'white', text.size = 6, strip.text = 15, axis.text = 20){
  bb = melt(boxtrans)
  bb$L1 = as.factor(bb$L1)
  bb$Var1 = as.factor(bb$Var1)
  bb$Var2 = as.factor(bb$Var2)
levels(bb$L1) = c('Winter','Spring','Summer','Fall')
lab.dim = dim(boxtrans[[1]])[1]
ggplot(bb, aes(x=Var2, y = Var1, fill = value))+
  geom_tile()+
  # geom_label(label = round(bb$value,2), colour = 'white', fontface = 'bold')+
  geom_text(label = round(bb$value,2), col = text.col, fontface = 'bold', size = text.size)+
  scale_fill_distiller(palette = palette, direction = 1)+
  facet_wrap(~L1)+
  xlab('Ending Area')+
  ylab('Start Area')+
  # scale_x_continuous(breaks = 1:lab.dim, labels = 1:lab.dim)+
  # scale_y_continuous(breaks = 1:lab.dim, labels = 1:lab.dim)+
  # theme(axis.text = element_text(size = 12),
  #       axis.title=element_text(size=14, face="bold"),
  #       strip.text = element_text(size = 12),
  #       panel.background = element_rect(fill = "white"))
    theme(strip.text = element_text(size = strip.text)
          ,  axis.title.x = element_text(size = axis.text)
          ,  axis.title.y = element_text(size = axis.text))
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
