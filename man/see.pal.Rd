\name{see.pal}
\alias{see.pal}
\title{plot a rectangular palette}
\description{
  the function adds to an existing plot
  in the lower left corner
}
\usage{
see.pal(col)
}
\arguments{
  \item{col}{vector of colors}
}

\value{
Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{see.pals}
\examples{

plot(c(0,1), c(0,1), type='n')
see.pal(rainbow(100))

}
\keyword{aplot}
