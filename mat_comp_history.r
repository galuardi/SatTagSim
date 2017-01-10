library(SatTagSim)

username =  as.character(Sys.info()[7])
setwd(paste0('C:/Users/', username,'/Google Drive/PHD/COLLABORATIONS/SCRS_SIMS/'))
datadir = 'D:/SKYDRIVE_BACKUP/PHD_SIMS/SCRS_SIMS'

mycol = colorRampPalette(c("lightcyan", "royalblue", "blue", "lemonchiffon", "orange", "red"), space = "Lab")

# EATL/GBYP
env1 = new.env()
load(paste0(datadir, '/abft_SIMS_GBYP_ALL_1000_v2.Rdata'), envir = env1)
# WATL > 185cm
env2 = new.env()
load(paste0(datadir, '/abft_SIMS_WABFT_gt185_wNOAA_1000_v2.Rdata'), envir = env2)
# WATL < 185cm
env3 = new.env()
load(paste0(datadir, '/abft_SIMS_WABFT_lt185_1000_v2.Rdata'), envir = env3)

eatl = env1$boxtrans
watl.big = env2$boxtrans
watl.small = env3$boxtrans

eatl.datbox = env1$datbox
watl.big.datbox = env2$datbox
watl.small.datbox = env3$datbox

eatl.ct = get.trans.prob(eatl.datbox, perc = F)
watl.big.ct = get.trans.prob(watl.big.datbox, perc = F)
watl.small.ct = get.trans.prob(watl.small.datbox, perc = F)

watld = melt(watl.big.ct)
names(watld) = c('start','end','freq','season')
watld$stock = 'west'
eatld = melt(eatl.ct)
names(eatld) = c('start','end','freq','season')
eatld$stock = 'east'

sims = rbind(watld,eatld)

sims$freq = round(sims$freq)
glm.model = glm(freq~start*end*season*stock, data = sims, family=poisson)
anova(glm.model, test="Chisq")
glm.model = glm(freq~season*stock, data = sims, family=poisson)
anova(glm.model, test="Chisq")

#---------------------------------------------------------------------------------#

# load("C:/Users/ben/Google Drive/PHD/log_linear_W_E.Rdata")
#
# load('E:/Ben/PHD/PHD_SIMS/SCRS_SIMS/abft_SIMS_WABFT_gt185_wNOAA_1000.Rdata')

# need this in here..
fillone <- function(trans)
{
  n = nrow(trans)
  for(i in 1:n){
    trans[i,i] = ifelse(sum(trans[i,])==0, 1, trans[i,i])
  }
  trans
}

# look at where the fish started
data(box7)
sptsdf = (melt(env2$spts, c('x', 'y')))
svals = get.box.vals(sptsdf, box7)
table(svals$Month, svals$box)

# look at first point of the sims
sim_starts = ldply(env2$simdat, function(x) x[1,])
ssvals = get.box.vals(sim_starts, box7)
table(ssvals$Month, ssvals$box)

#------------------------------------------------#
#plot them..
coordinates(svals) = ~lon+lat
coordinates(ssvals) = ~lon+lat
# plot(svals, col = 4, add = F, cex = .2, pch = 3)
# plot(ssvals, col = 2, add = T, cex = .2, pch = 19)
x11()
plot(ssvals, col = tim.colors(12)[ssvals$Month], add = F, cex = .2, pch = 19)
plot(box7, add=T, border = 'salmon', lwd = 2)
world(add=T, fill=T, col = 1, border = 'white')
degAxis(1)
degAxis(2)
box()
# legend('bottom', pch = c(3, 19), col = c(4, 2), legend = c('start points','sim starting points'), cex = 1.6)
legend('bottom', pch = c(19), col = tim.colors(12)[1:12], legend = month.abb, cex = 1, horiz = T, bg = 'white')
title('Start Points for W. ABFT > 185cm')
savePlot('WABFT_GT185_STARTS', type = 'png')
#------------------------------------------------#

b1 = watl.big
b2 = eatl

ball.w = b1[[1]]%*%b1[[2]]%*%b1[[3]]%*%b1[[4]]
ball.e = b2[[1]]%*%b2[[2]]%*%b2[[3]]%*%b2[[4]]

dimnames(ball.w)[[1]] = dimnames(ball.w)[[2]] = as.character(1:7)
dimnames(ball.e)[[1]] = dimnames(ball.e)[[2]] = as.character(1:7)

bm = rbind(data.frame(melt(b1), stock = 'w'), data.frame(melt(b2), stock = 'e'))

names(bm) = c('b','e','f','s', 'stock')

# b3 = table(datbox$pbox, datbox$cbox, datbox$season)

#----------------------------------------------------------#
# GLM for chisq test

glm.model = glm(f~s*e*seas*stock, data=bm, family=poisson)
glm.model
anova(glm.model, test="Chisq")

glm.model = glm(f~b*e*s, data=bm, family=poisson)
anova(glm.model, test="Chisq")

glm.model = glm(f~e*b, data=bm, family=poisson)
anova(glm.model, test="Chisq")

#----------------------------------------------------------#
# more simplified
b1 = watl.big.ct
b2 = eatl.ct

mydata = rbind(b1[[1]][1, ], b1[[2]][1, ]) # counts

mytaba = xtabs(f~s+stock, data = bm) # no seasons
chisq.test(mytaba)
loglm(~stock+s, mytaba)

#----------------------------------------------------------#
#Log linear model?
# across rows within a run
loglm(~ Var1, xtabs(value ~ Var1+Var2, melt(watl.big)))

#----------------------------------------------------------#
# another function: compare any two rows..
#----------------------------------------------------------#

# function uses percentages
get.loglin <- function(mat1, mat2){
  fm = NULL
  for(i in 1:nrow(mat1)){
    fm[[i]] =  loglin(rbind(mat1[i,], mat2[i,]),c(1),print = F)
  }
  unlist(lapply(fm, function(x) pchisq(x$lrt, x$df)))
}

### Between seasons of same run
(bw.bs = get.loglin(watl.big[[1]], watl.big[[2]]))

### between runs, same season
# same row, same season, large and small WABFT
(bg.sm = get.loglin(watl.big[[1]], watl.small[[1]]))

sapply(1:4, function(x) get.loglin(watl.big[[x]], watl.small[[x]]))

### between stocks
(e.w = get.loglin(ball.e, ball.w))

### stocks and season
e.w.s = sapply(1:4, function(x) get.loglin(watl.big[[x]], eatl[[x]]))

#----------------------------------------------------------#
# try some markovchain library stuff

# load('E:/Ben/PHD/PHD_SIMS/SCRS_SIMS/abft_SIMS_WABFT_gt185_wNOAA_1000.Rdata')

library(markovchain)

seq = as.character(subset(watl.big.datbox, TagID == 1)$cbox)
mcFit <- markovchainFit(data = seq, byrow = FALSE)
divergenceTest(seq, mcFit$estimate@transitionMatrix)

d = NULL
for(i in 1:1000){
 seq2 =  as.character(subset(watl.big.datbox, TagID == i)$cbox)
 d[[i]] = divergenceTest(seq2, mcFit$estimate@transitionMatrix)$p.value
}

d = NULL
for(i in 1:1000){
  seq2 =  na.omit(as.character(subset(watl.big.datbox, TagID == i)$cbox))
  d[[i]] = divergenceTest(seq2, ball)$p.value
}

#------------------------------------------------------------#
# Multinomial variance?
# assuming that each row of the transition matrix is multinomial:

# X = b1[[1]][1,] # use the counts
get.mvar = function(x){
  p = drop(x) # use the percentages
  v = p*(1-p)
  # c = outer(p, 1-p)
  # vcv = diag(c)
  v
}

get.mvar.samp = function(vmu, vsd) {
  o = rtruncnorm(1, mean = vmu, sd = vsd, a = 0, 1)
  o[is.nan(o)] = 0
  o
}

# example for one
X = watl.big[[2]][1,]
get.mvar(X)

# example for all 4 transition matrices
vmat = lapply(watl.big, function(x) apply(x, 1, get.mvar))

# sample from posterior
post = sapply(1:7, function(x) get.mvar.samp(watl.big[[1]][x,], vmat[[1]][x,]))

# plot the variance for each cell, each season
lapply(vmat, function(x) image(1:7, 1:7, x, col = mycol(49)))


#------------------------------------------------------------------------#
# Total days in area
#------------------------------------------------------------------------#
simdatdf = env1$simdatdf
boxvals = get.box.vals(simdatdf, box7)
simdatdf$box = boxvals$box
simdatdf$TagID = as.vector(sapply(1:12000, function(x) rep(x, 720)))

# number of unique fish in area per season
ufish = daply(simdatdf, c('seas','box'), function(x) length(unique(x$TagID)))

# number of unique positions in area per season
upts = daply(simdatdf, c('seas','box'), function(x) nrow(x))

# look at NA box values
plot(sr[[i]]/max(values(sr[[i]]), na.rm=T), zlim = c(0.01,1), interp = T, col = mycol(256), axes=F)
world(add = T, col = 'grey80', fill = F)

points(subset(simdatdf, is.na(box))[,1:2], pch = 3, cex = .25)

#------------------------------------------------------------------------#
# Save workspace
#------------------------------------------------------------------------#
save.image('sim_comparison_models.Rdata')

