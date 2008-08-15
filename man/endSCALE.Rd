\name{endSCALE}
\alias{endSCALE}
\title{Plot nice scale at end of trace.
}
\description{Calculate nice scale to use at the end of a plot. Use as an
  alternative to magicaxis.
  
}
\usage{
endSCALE(arange, digits = 3)
}
\arguments{
  \item{arange}{2-vector of bounds
}
  \item{digits}{number of digits to use
}
}
\details{The function returns information for plotting
  a nice bounds axis similar to MATLAB plotting style.

}
\value{character vector: min, max, exponent
}

\note{
  If the bounds span multiple orders of magnitude,
  may want to make adjustments (like setting a negative exponent
  bound to zero)
  }

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{plotwlet
}
\examples{
M = 1e-19
m = M

for(i in 1:10) {
  z = c( rnorm(1)*m ,  rnorm(1)*M )
  print(z)
  print( endSCALE(z)   )

##########  use in plotting:

x = seq(from=0, by=0.01, length=200)
a = 10000*rnorm(length(x))
old.par <- par(no.readonly = TRUE)
############  make room on the right margin
MAI = par("mai")
 MAI[4] = MAI[2]
par(mfrow=c(2,1))
 par(mai=MAI)
par(xaxs='i', yaxs='i')

plot(x,a, type='l')
  axtrace = range(a)
    Elabs =  endSCALE(axtrace)
    exp = parse(text = Elabs[3])
 axis(4, at=axtrace , labels=Elabs[1:2] , pos=max(x), tick=TRUE , line=0.5, cex.axis=0.8,las=2)
       mtext(exp, side = 3, at = max(x), line=0.5, adj=-1  , cex=0.8)
    mtext("m/s", side = 4, at =mean(axtrace) , line=0.5   , cex=0.8 ,las=1 )

a = rnorm(length(x))/100000


plot(x,a, type='l')
  axtrace = range(a)
    Elabs =  endSCALE(axtrace)
    exp = parse(text = Elabs[3])
 axis(4, at=axtrace , labels=Elabs[1:2] , pos=max(x), tick=TRUE , line=0.5, cex.axis=0.8,las=2)
       mtext(exp, side = 3, at = max(x), line=0.5, adj=-1  , cex=0.8)
    mtext("m/s", side = 4, at =mean(axtrace) , line=0.5   , cex=0.8 ,las=1 )

par(old.par)


} 




}
\keyword{misc}
