\name{WOASST}
\alias{WOASST}
\docType{data}
\title{
World Ocean Atlas SST 2013
}
\description{
World Ocean Atlas surface temperature data, 1/4 degree, monthly for the North Atlantic
}
\usage{data("WOASST")}
\format{
  A list
  \describe{
    \item{\code{lon}}{a numeric vector}
    \item{\code{lat}}{a numeric vector}
    \item{\code{sst}}{a 3D array by month}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\url{https://www.nodc.noaa.gov/OC5/woa13/woa13data.html}
}
\references{
Boyer, T.P., J. I. Antonov, O. K. Baranova, C. Coleman, H. E. Garcia, A. Grodsky, D. R. Johnson, R. A. Locarnini, A. V. Mishonov, T.D. O'Brien, C.R. Paver, J.R. Reagan, D. Seidov, I. V. Smolyar, and M. M. Zweng, 2013: World Ocean Database 2013, NOAA Atlas NESDIS 72, S. Levitus, Ed., A. Mishonov, Technical Ed.; Silver Spring, MD, 209 pp., \url{http://doi.org/10.7289/V5NZ85MT}

}
\examples{
data(woasst)
library(fields)
image.plot(woasst$lon, woasst$lat, woasst$sst[,,1])
}
\keyword{datasets}
