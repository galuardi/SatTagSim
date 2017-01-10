library(plyr)
library(markovchain)

load("C:/Users/ben/Google Drive/PHD/log_linear_W_E.Rdata")

bigmat = matrix(0,nrow=28, ncol = 28)

bigmat[(1:7),(1:7)] = eatl[[1]] # winter
bigmat[(1:7)+7,(1:7)+7] = eatl[[2]] # spring
bigmat[(1:7)+14,(1:7)+14] = eatl[[3]] # summer
bigmat[(1:7)+21,(1:7)+21] = eatl[[4]] # fall

colnames(bigmat) = paste0('e_',as.vector(sapply(names(eatl)[1:4], function(x) rep(x,7))))
rownames(bigmat) = paste0('s_',as.vector(sapply(names(eatl)[c(4,1,2,3)], function(x) rep(x,7))))

# try the markov chain library
library(markovchain)

mc1 = as(eatl[[1]], 'markovchain')

plot(mc1)

steadyStates(mc1)

conditionalDistribution(mc1, 's6')

initialState<-c(0,0,0,.2,.5,.1,.2)
steps<-10
finalState<-initialState*mc1^steps #using power operator
finalState

# add some raw data
load("C:/Users/benjamin.galuardi/Google Drive/PHD/tagd/SIM_RESULTS/simdat_large_gt180d_vary_start.Rdata")


# try estimation stuff

# flip the ID and areas

db = datbox[,c(3,6)]

db2 = NULL

for(i in unique(db$TagID)){
 db2 = rbind(db2, t(subset(db, TagID==i)[,2]))
}
db2 = as.data.frame(db2)
names(db2) = paste0('time', 1:8)
db2$TagID = 1:1000

mfit = markovchainFit(data = db2[, 1:8], byrow = T, nboot = 100, sanitize = T, possibleStates = as.character(1:7))
mfitl = markovchainListFit(data = db2[, 1:8], byrow = T)

markovchainListFit(data = db2[, 1:8], name = "db_mclist")

sq = datbox$cbox[datbox$TagID==1]

sequenceMatr <- createSequenceMatrix(sq, sanitize = FALSE)
mcFitMLE <- markovchainFit(data = sq)
mcFitBSP <- markovchainFit(data = sq, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")







