#' Simulate tracks in parallel
#'
#' Function to simulate a single track using monthly parameters, bathymetry and World Ocean Atlas SST in parallel
#' @param n number of tracks to simulate
#' @param tpar data frame of simulation parameters
#' @param morder month order for simulation (e.g. start in June vs start in January)
#' @param sp list of starting points
#' @param sstmat list of lon, lat and 3D matrix of monthly sea surface temperature. see details
#' @param seaslen length of each month to simulate. Defaults to 30 (days per month)
#' @param sstol tolerance for sst matching. see details
#' @param mcoptions This is a hidden variable that must be present to run in parallel
#' @details sstmat in this package is a running average of index suitable temperature for each month. Each month has values 0-3. A value of 3, for example, represents areas suitable in the current month, previous month and next month. The sstol flag can make matching criteria more or less restrictive. The default value means that at least two months of suitability, centered on the current month, must be valid for a simulation point.
#' Tracks may be generated by increasing morder (e.g. rep(morder, 2) for a 24 month track)
#' Function uses mcoptions from \code{\link{setup.parallel}} to run in parallel.
#' @return List of simulated tracks. Tracks are data frames with columns: lon lat month
#' @export
#' @author Benjamin Galuardi
#' @examples
#' see vignette
make.sim.track.par <- function(tpar = tpar, morder = morder, sp = spts, bath = bath, sstmat = sstmat, seaslen = 30, sstol = 2, mcoptions = setup.parallel(), ...)

  foreach(i = sp, .options.multicore = mcoptions) %dopar% #

  {
    # require(analyzepsat)
    require(SatTagSim)
    ## Subfunctions
    .get.samp <- function (vec, npoints, ci = 0.95)
    {
      vec = as.numeric(vec)
      Sigma <- matrix(vec[1:4], 2, 2) * ci
      mu <- c(vec[5:6])
      if (sum(Sigma) > 0) {
        ndata <- mvrnorm(npoints, mu, Sigma)
        return(ndata)
      }
    }
    .get.bath <- function (lon, lat, BATH)
    {
      X = as.vector(BATH$lon)
      Y = as.vector(BATH$lat)
      xidx = which.min((lon - X)^2)
      yidx = which.min((lat - Y)^2)
      BATH$data[yidx, xidx]
    }
    getsp <- function(sp, var=c(1,0,0,1), bath=bath){
      vec=c(var, sp)
      x = .get.samp(vec, 1)
      while(.get.bath(x[1], x[2], BATH=bath)>0){
        x = .get.samp(vec, 1)
        vec=c(var+c(.5, 0, 0, .5), sp)
      }
      x
    }
    # FUnCTION TO GET A SST MASK VALUE FROM A 3D ARRAY OF WORLD OCEAN ATLAS SST
    get.sst.mask.val <- function (lon, lat, mask, month)
    {
      X = as.vector(mask$lon)
      Y = as.vector(mask$lat)
      xidx = which.min((lon - X)^2)
      yidx = which.min((lat - Y)^2)
      mask$data[xidx, yidx, month] # flagged off for additive months
      # mask$data[xidx, yidx]
    }

    temp = NULL #cbind(c(sp, morder[1]))  # object will be the simulated track
    # names(temp) = c('lon,','lat','Month')
    if(!is.null(bath)){
      msp = getsp(i, bath = bath)
    }else{
      msp = as.numeric(i)
    }
    uvmult = 30/seaslen

    # for(n in 1:nrow(sp)){

    # msp = sp

    for(seas in morder){   # winter, spring, summer, fall
      midx = tpar$Month==seas
      u = c(tpar[midx, 2], tpar[1, 5])*uvmult
      v = c(tpar[midx, 3], tpar[1, 6])*uvmult
      D = c(tpar[midx, 4], tpar[1, 7])*uvmult
      Dorig = D
      uorig = u
      vorig = v

      # setup an sst dataframe for minimizing distance to non-NA area
      # 		xyz = cbind(expand.grid(sstmat$lon, sstmat$lat), as.vector(t(sstmat$data[,,seas])))
      #     names(xyz) = c('x','y','z')
      #     gidx = which(!is.na(xyz[[seas]]$z))

      #	seaslen = seaslen  # the first seas simulated will start at the midpoint of that season, not the beginning.

      for(j in 1:seaslen) {
        t1 = SatTagSim::simm.kf(2, u, v, D, msp)[2,]
        # else{
        # ii = 1
        # while(is.na(get.sst.mask.val(t1[1], t1[2], sstmat, seas))){
        # 					cat(seas)
        # t1 = simm.kf(2, u, v, D, msp)[2,]
        # ii = ii+1
        # if(ii>5) {
        # temp = temp[1:(nrow(temp)-j-1),]
        # j = j-1
        # t1 = NA
        # if(is.na(get.sst.mask.val(t1[1], t1[2], sstmat, seas))){
        # pd = pointDistance(t1, xyz[gidx, 1:2], lonlat = T)
        # t1 = as.numeric(xyz[gidx[which.min(pd)],1:2])
        # 		         while(is.na(get.sst.mask.val(t1[1], t1[2], sstmat, seas))){
        #   							ttmp = cbind(rep(t1[1], 100), rep(t1[2], 100))
        # 						    tsamp = t(apply(ttmp, 1, function(x)
        # 						        simm.kf(n = 2, sp = as.numeric(x), u = u, v = v, D = D)[2,]))
        #   							tidx = apply(tsamp, 1, function(y) get.sst.mask.val(y[1], y[2], sstmat, seas))
        # 						    t1 = as.numeric(tsamp[sample(which(!is.na(tidx)), 1),])
        if(!is.null(sstmat)){
          if((get.sst.mask.val(t1[1], t1[2], sstmat, seas)) < sstol){
            # if(is.na(get.sst.mask.val(t1[1], t1[2], sstmat, seas))){
            t1 = SatTagSim::simm.kf(2, u = c(-1*u[1], u[2]), v = c(-1*v[1], v[2]), D = c(D[1], 1000), msp)[2,]
            #   							ttmp = cbind(rep(t1[1], 100), rep(t1[2], 100))
            # 						    tsamp = t(apply(ttmp, 1, function(x)
            # 						          simm.kf(n = 2, sp = as.numeric(x), u = u, v = v, D = D)[2,]))
            #   							tval = apply(tsamp, 1, function(y) get.sst.mask.val(y[1], y[2], sstmat))
            #   							tidx = which(tval>=3)
            # 						    t1 = as.numeric(tsamp[sample(tidx, 1, prob = tval[tidx]),])
            # break
          }
        }

        if(!is.null(bath)){
          while(.get.bath(t1[1], t1[2], bath)>0){
            t1 = simm.kf(2, u, v, D, msp)[2,]
          }
        }
        # if(ii>5) break
        # }
        #             points(t1[1], t1[2], pch = 3, cex=.2, col=2)
        #             ii = ii+100
        # 					 t1 = get.nn.mask(t1, sst[[seas]], tol = 5)
        # }
        temp = rbind(temp,  cbind(t(t1), seas))
        msp = ifelse(is.na(t1), msp, t1)
      }
    }
    # }
    tsim = as.data.frame(temp)
    names(tsim) = c('lon,','lat','Month')
    tsim
  }




