\name{jpng}
\alias{jpng}
\alias{jpdf}

\title{png or pdf  output}
\description{
  Get file name and recreate plot
  on a png or pdf device.  This program
  makes an attempt to keep the same size
  plot as viewed in the screen.
}
\usage{
jpng(file='tmp',  width = 8, height = 8,P = NULL,  bg = "white")
jpdf(file='tmp',  width = 8, height = 8,P = NULL)

}

\arguments{
  \item{file}{png or pdf: will be added as a suffix, if needed}
  \item{width}{width, inches }
  \item{height}{height, inches }
  \item{P}{vector to fix the size, c(width, height) }
  \item{bg}{background color (default="transparent") }
  
}
\details{
  If P=c(10,12) is missing or NULL, program will
  attempt to use current plotting region via par
  to duplicated the size
  of the postscript device. Must close this device with
  dev.off() to finish.  If either w or h are provided they will override
  the values in vector P.

  If the standard suffix (png or pdf) are provided
  the file will be set. If these are omitted, they will be added
  to the given name according to the local.file function.
  
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
