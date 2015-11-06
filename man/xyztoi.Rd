\name{itoxyz}
\alias{itoxyz}

\title{Vector Index to Matrix Index}
\description{
  Given I index get ix,iy, iz for three dimensional grids.
}
\usage{
itoxyz(i, nx, ny, nz)
}

\arguments{
  \item{i}{index to long vector}
  \item{nx}{number of blocks in x axis}
  \item{ny}{number of blocks in y axis}
  \item{nz}{number of blocks in z axis (layers) }
}

\value{
  \item{ix}{Index of X-array}
  \item{iy}{Index of Y-array}
  \item{iz}{Index of Z-array (layer)}
  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{xyztoi}

\examples{

 itoxyz(24, 6, 6, 1)

kpos = itoxyz(2443:2500 , 20, 20, 13)


}

\keyword{misc}
