
month.colors2 = as.data.frame(month.colors[order(as.numeric(month.colors[,1])),])

msp = as.numeric(spts[[5]][10,])

plot(msp[1], msp[2], xlim = c(-100, -40), ylim = c(15, 45))
sapply(1:12, function(x) lines(tsim[tsim$Month==x,1:2], col = month.colors[x==month.colors[,1],2], pch = 19, typ = 'o'))
# points(tsim[,1:2], col = month.colors[tsim$Month,2], pch = 19)
plot(map, add=T)
points(tsim[1,1], tsim[1,2], bg = 3, pch = 21, cex = 2, col = 1)
points(tsim[nrow(tsim),1], tsim[nrow(tsim),2], bg = 2, pch = 24, cex = 2, col = 1)
