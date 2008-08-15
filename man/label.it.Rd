
\name{label.it}
\alias{label.it}

\title{Labels on Plots}
\description{
  Put Labels (A,B, C...) on corners of figures
}
\usage{
label.it(a = "", corn = 1, ...)
}

\arguments{
  \item{a}{letters}
  \item{corn}{corner}
  \item{...}{graphical parameters passed from par}
  
}

\value{
 Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

par(mfrow=c(2,2))
for(i in 1:4)
{
plot(rnorm(5), rnorm(5))
label.it(letters[i],1)
}

}


\keyword{aplot}

