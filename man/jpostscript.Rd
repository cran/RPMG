\name{jpostscript}
\alias{jpostscript}
\alias{jpng}

\title{Postscript Output}
\description{
  Get file name and recreate plot
  on a postsctipt device.  This program
  makes an attempt to keep the same size
  plot as viewed in the screen.
}
\usage{
jpostscript(file=NULL, P=NULL, w=NULL, h=NULL)
jpng(file=NULL, P = NULL)
}

\arguments{
  \item{file}{Postscript file name, eps will be added as a suffix}
  
  \item{P}{vector to fix the size, c(width, height) }
  \item{w}{width, inches }
  \item{h}{height, inches }



  
}
\details{
  If P=c(10,12) is missing or NULL, program will
  attempt to use current plotting region via par
  to duplicated the size
  of the postscript device. Must close this device with
  dev.off() to finish.  If either w or h are provided they will override
  the values in vector P.
  }
  
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{par, postscript, device }

\examples{

jjj = local.file('hi', 'eps')
x= rnorm(10)
y= rnorm(10)

plot(x,y)

print('resize the current plot')

jpostscript(jjj)
plot(x,y)
dev.off()

jpostscript("HiThere", P=c(7,7) )
plot(x,y)
dev.off()


jpostscript("HiThere", P=c(7,7), w=10 )
plot(x,y)
dev.off()




}

\keyword{misc}
