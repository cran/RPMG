\name{RESCALE}
\alias{RESCALE}

\title{Rescale a vector to fit in a certain range }
\description{
 Rescale a vector to fit in a certain range
}
\usage{
RESCALE(x, nx1=0, nx2=1, minx=0, maxx=1)
}

\arguments{
  \item{x}{vector}
  \item{nx1}{new minimum}
  \item{nx2}{new maximum}
  \item{minx}{old min}
  \item{maxx}{old max}
}
\details{
 Rescaling a vector, mostly used for graphics.
 If x does not vary, i.e. it is constant or minx and max are identical, the mean value of nx1 and nx2
 is returned.
}
\value{
 Scale version of x vector is returned.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{
x = rnorm(10)
RESCALE(x, 3, 9, min(x), max(x) )
}

\keyword{misc}
