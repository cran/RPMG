\name{HOZscale}
\alias{HOZscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{add horizontal color scale}
\description{
 Add horizontal color scale to existing plot.
}
\usage{
HOZscale(z, col, units="", SIDE=1, s1=.6, s2=0.95,
	    format=1, digits=3, cex=1, cex.units=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{image matrix}
  \item{col}{color palette}
  \item{units}{character string, units}
  \item{SIDE}{Side of the plot}
  \item{s1}{percent of margin for bottom}
  \item{s2}{percent of margin for top}
  \item{format}{Format: 1 for normal number, 2 for exponential notation}
   \item{digits}{Significant digits}
  \item{cex}{Character expansion for the numeric values.}
  \item{cex.units}{Character expansion for the units.}
 
}

\value{
 Graphical Side effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{
data(volcano)
image(volcano, col=terrain.colors(100))

HOZscale(volcano,terrain.colors(100) , units = "", SIDE = 1, s1 = 0.4, s2 = 0.95)


plot(1:10, 1:10, type='n')
j = c(runif(1, -10, 10) , runif(1, 20, 10000) )

### example showing scale above and below
HOZscale(j, terrain.colors(100),
          units="hi", SIDE=3, s1=.4, s2=0.6, format=2, digits=2, cex.units = 1.2, cex=1.2)

j = c(runif(1, -10, 10)/1000 , runif(1, 1, 10) )

HOZscale(j, terrain.colors(100),
          units="hi", SIDE=1, s1=.6, s2=0.8, format=2, digits=2, cex.units = 0.8)


}

\keyword{hplot}
