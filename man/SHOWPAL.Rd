\name{SHOWPAL}
\alias{SHOWPAL}
\alias{showtopopal}


\title{Show a palette of colors as a bar}
\description{

  Show a palette of colors as a bar
}
\usage{
SHOWPAL( COLLIST , NAME = FALSE, NUM=FALSE, ncol = 5, BACK="transparent")
}
\arguments{
  \item{COLLIST}{vector of colors}
  \item{NAME}{name of palette}
  \item{NUM}{logical, TRUE=show index number}
  
  \item{ncol}{number of colors}
  \item{BACK}{Background color, default=NULL}
  
}

\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{see.pals, help.pal , plotpal , helpcolors}
\examples{

##########   make a large screen for a lot of colors
### dev.new(width=12, height=12)

SHOWPAL(colors(), ncol=15, NAME=FALSE)

gcol = setXMCOL()

SHOWPAL(gcol, ncol=10, NAME=TRUE)


####  show index:
SHOWPAL(gcol, ncol=10, NAME=TRUE, NUM=TRUE)

  pl = c("grey", "lightblue1", "pink", "darkseagreen2", "gold1",
      "chartreuse1", "aquamarine", "plum1", "goldenrod", "maroon1",
      "deepskyblue", "palegreen2", "salmon")


SHOWPAL(pl,  NAME=TRUE, NUM=TRUE)


SYSCOL =  pastel.colors(100)
SHOWPAL(SYSCOL, ncol=10)


SYSCOL = sepia.colors(100)
SHOWPAL(SYSCOL, ncol=10)


SYSCOL =  hcl(h=seq(from=0, to=260, length=100) )
SHOWPAL(SYSCOL, ncol=10)




}
\keyword{hplot}
