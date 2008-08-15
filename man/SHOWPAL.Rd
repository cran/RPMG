\name{SHOWPAL}
\alias{SHOWPAL}
\alias{showtopopal}


\title{Show a palette of colors as a bar}
\description{

  Show a palette of colors as a bar
}
\usage{
SHOWPAL(XMCOL, NAME = FALSE, ncol = 5)
}
\arguments{
  \item{XMCOL}{vector of colors}
  \item{NAME}{name of palette}
  \item{ncol}{number of colors}
}

\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{see.pals, help.pal , plotpal , leescolors}
\examples{

SHOWPAL(colors())
}
\keyword{hplot}
