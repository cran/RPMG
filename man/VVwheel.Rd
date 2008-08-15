\name{VVwheel}
\alias{VVwheel}
\title{Make a color rectangle (wheel) }
\description{
  Make a color rectangle (wheel)
}
\usage{
VVwheel(BIGMESH = NULL, v = 1)
}
\arguments{
  \item{BIGMESH}{color mesh}
  \item{v}{v, from hsv color scheme}
}

\value{
  \item{M}{
    meshgrid:
    \describe{
   \item{x}{x - location}
   \item{y}{y - location}
   }
    }
\item{ARE}{Radii}
\item{pANG}{angle}
\item{dx}{delta x}
\item{dy}{delta y}
\item{RY}{range x}
\item{RX}{range y}

  

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{hsv, VVwheel, wheelrgb}
\examples{

\dontrun{
BIGMESH = VVwheel( v=1)
}

}
\keyword{misc}
\keyword{hplot}
