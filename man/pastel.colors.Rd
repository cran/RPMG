\name{pastel.colors}
\alias{pastel.colors}
\title{pastel colors}
\description{
  vector of pastel colors
}
\usage{
pastel.colors(num, seed=0)
}
\arguments{
  \item{num}{number of colors}
  \item{seed}{random number seed}
    
}
\details{
  The seed is a value given so that the same pastel colors
  can be extracted with each subsequent call to the code.
  
}
\value{
 vector of RGB hex colors
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{rainbow}
\examples{
pastel.colors(12)

pastel.colors(12, seed=1 )




}
\keyword{misc}
