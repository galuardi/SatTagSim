\name{bath}
\alias{bath}
\docType{data}
\title{
Bathymetric data
}
\description{
Bathymetric data truncated to 60N and 20S, 100W and 40E
This covers the known range of Atlantic bluefin tuna.
The straits of Gibraltar, Florida and St. Lawrence have been widened for simulation purposes. All spurious bodies of water have been converted to land (i.e. Great Lakes)
}
\usage{data(bath)}
\format{
  The format is:
List of 3
 $ lon : num [1:851] -98.5 -98.3 -98.2 -98 -97.8 ...
 $ lat : num [1:465] -9.03 -8.87 -8.7 -8.53 -8.37 ...
 $ data: num [1:465, 1:851] 1 1 1 1 1 1 1 1 1 1 ...
}

\source{
ETOPO1 bathymetric dataset
http://coastwatch.pfeg.noaa.gov/erddap/griddap/etopo360.graph
}
\references{
http://coastwatch.pfeg.noaa.gov/erddap/griddap/etopo360.graph
http://www.ngdc.noaa.gov/mgg/global/global.html
}
\examples{
library(fields)
data(bath)
bath.colors2 <- colorRampPalette(c("grey10", "gray40", "gray50", "gray70", "lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1","lemonchiffon1"))
image.plot(bath[[1]], bath[[2]], t(bath[[3]]), zlim = c(-10000,0), col = bath.colors2(256))
}
\keyword{datasets}
