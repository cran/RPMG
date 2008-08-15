\name{colwheel}
\alias{colwheel}
\title{Choose rgb from a color rectangle}
\description{
  Shows and image of colors and allows one to
  choose a color and see what it looks like
  in swath with different backgrounds.
}
\usage{
colwheel(v = 1, BACK = "black")
}
\arguments{
  \item{v}{v, from hsv color scheme}
  \item{BACK}{starting background color}
}

\value{
vector of RGB colors in hex format.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{hsv, VVwheel, wheelrgb, SHOWPAL.A}
\examples{

\dontrun{
colwheel(v = 1, BACK = "black")

colwheel(v = 1, BACK = "white")
}

}
\keyword{misc}
