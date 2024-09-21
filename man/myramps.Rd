\name{myramps}
\alias{myramps}
\docType{data}
\title{
myramps
}
\description{
Colors ramps for SatTagSim. Includes:
month colors fro plotting fish locations by month
bathymetric depth colors (two versions)
land.colors \link{fields} package
jet.colors from the \link{fields} package
month names

Color ramps prooduced using \link{colorRampPalette}
}
\usage{data("myramps")}
\format{
  color ramps
  \describe{
    \item{\code{bath.colors}}{a color ramp}
    \item{\code{bath.colors2}}{a color ramp}
    \item{\code{land.colors}}{a color ramp}
    \item{\code{jet.colors}}{a color ramp}
    \item{\code{month.colors}}{a 2D numeric matrix used for indexing colors}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
month.colors used in:

Galuardi, B., Royer, F., Golet, W., Logan, J., Nielson, J., and Lutcavage, M. 2010. Complex Migration Routes of Atlantic Bluefin Tuna Question Current Population Structure Paradigm. Can J. Fish. Aquat. Sci 67(6): 966–976.

}
\references{
\link{fields}
\link{analyzepsat} (if installed; this is the origin of this list)
}
\examples{

}
\keyword{datasets}
