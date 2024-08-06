\name{SatTagSim-package}
\alias{SatTagSim-package}
\alias{SatTagSim}
\docType{package}
\title{
SatTagSim: A package to simulate movements from telemetry based advection-diffusion parameters
}
\description{
This package calcualtes advection-diffusion parameters from psat data for use in individual based simulations. This is designed to generate tranistion probabilities between stock areas, ecoregions, or other regions of interest. The idea was conceived by Sibert et al. 2006. This version begins with final tracks estimated from eletronic tags using state space Kalman flters. Advection parameters are caclulated from the most probable path while the diffusion parameter is  estimated directly during the track estimation.
}
\details{
\tabular{ll}{
Package: \tab SatTagSim\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-03-25\cr
License: \tab WGPL-3\cr
}

This package requires input in the form returned from \code{\link{analyzepsat::make.btrack}}.

Parameters are cacluated using the \code{\link{guess}} and \code{\link{get.ud.area}} functions. These are cacluated on a monthly basis, by fish. This returns two lists (potentially long) of parameter values. These are merged together using \code{\link{merge.par}}. Monthly summaries may be output which are then used for simulations using \code{\link{make.etrack.list}}. These simulations follow the correlated random walk described in \code{\link{kftrack}}. Land avoidance is achieved by using a customized bathymetric layer. Visualization of simulated tracks is best done in a summarized fashion; a fucntion for plotting simulateed utilization distributions (similar to kernel density) is included (see \code{\link{plot.seas.ud}})

This package is desigend for use primarily with Atlantic bluefin tuna but may be useful for a variety of species. It is intended to be used for determining transition probabilities in support of operational modeling of Atlantic bluefin tuna. It is a primary portion of my PhD work as well.


}
\author{
Benjamin Galuardi

Maintainer: Who to complain to <drdrumfish@gmail.com>

}
\references{
Sibert, J. R., Musyl, M. K. and Brill, R. W. 2003. Horizontal movements of bigeye tuna (Thunnus obesus) near Hawaii determined by Kalman filter analysis of archival tagging data. Fish. Oceanogr. 12(3): 141?151.

Sibert, J. R., Lutcavage, M. E., Nielsen, A., Brill, R. W. and Wilson, S. G. 2006. Interannual variation in large-scale movement of Atlantic bluefin tuna (Thunnus thynnus) determined from pop-up satellite archival tags. Can. J. Fish. Aquat. Sci. 63(10): 2154?2166.

}

\keyword{advection}
\keyword{diffusion}


