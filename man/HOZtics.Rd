\name{HOZtics}
\alias{HOZtics}
\title{Add tics to Horizontal Scale
}
\description{Add tics and levels to color scale for am image plot.
}
\usage{
HOZtics(HOZ, side = 1)
}
\arguments{
  \item{HOZ}{Output coordinates of HOZscale
}
  \item{side}{1=above, 2=below
}
}
\details{The levels are determined via the pretty function.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{ColorScale
}
\examples{

pal1 = terrain.colors(100)
Z = c(1,40)
plot(c(0,1), c(0,1) )
hs = HOZscale(Z, col=pal1)
HOZtics(hs, side=1)

}
\keyword{misc}
