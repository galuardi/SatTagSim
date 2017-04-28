# fix gomex residency


aa = (ddply(datbox1, 'TagID', function(x) mean(x$box, na.rm = T)))

gidx = which(aa$marea==1)

plot(map, xlim = c(-100, -50), ylim= c(10, 50))
sapply(1:600, function(x) lines(simdat[[x]][,1:2], col = hsv(1/x,.9,.9, alpha=.5)))


