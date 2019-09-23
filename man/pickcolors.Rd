\name{pickcolors}
\alias{pickcolors}
\title{Pick a SYSTEM color}
\description{
  Pick a SYSTEM color
}
\usage{
pickcolors(COLLIST = colors(), BACK = "white")
}
\arguments{
  \item{COLLIST}{system colors}
  \item{BACK}{background for colors}
}

\value{
 List of colors
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{syscolors}

\examples{
\dontrun{
######## see named colors, excluding grey
SYSCOL =  colors()
greys = grep('grey', SYSCOL)
grays = grep('gray', SYSCOL)

kolz = SYSCOL[-c(greys, grays) ]
pickcolors(COLLIST = kolz, BACK = "white")

###  or just one type
SYSCOL =  colors()
blues = SYSCOL[grep('blue', SYSCOL) ]
pickcolors(COLLIST = blues, BACK = "white")
}
}



\keyword{misc}
