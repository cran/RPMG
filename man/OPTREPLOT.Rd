\name{OPTREPLOT}
\alias{OPTREPLOT}
\title{Replot Function for SELBUT
}
\description{Replot Function for SELBUT
}
\usage{
OPTREPLOT(opts , ncol=5, sel=1,  HOZ=TRUE, TOP=TRUE,
cols="white", scol="black", bcol="white" , tcol="black",
slwd=1, blwd=3, main="", xlim=c(0,1), ylim=c(0,1),
cex=1,  mpct = 0.1,  newplot=TRUE)
}
\arguments{
  \item{opts}{character list of options
}
  \item{ncol}{number of columns
  }
\item{sel}{vector of selected options}
  
  \item{HOZ}{logical, TRUE=plot horizontally
}
  \item{TOP}{logical, TRUE=plot top-down
}
  \item{cols}{colors
  }
  \item{scol}{select box color}
  \item{bcol}{default box color}
  \item{tcol}{box text color}
  
  \item{slwd}{select box line width}
  \item{blwd}{default box line width}
  \item{main}{character title
}
  \item{xlim}{x-limits in plotting region (user coordinates)
}
  \item{ylim}{y-limits in plotting region (user coordinates)
  }
  \item{cex}{character expansion for text in boxes}
  \item{mpct}{percentage margin to leave between option boxes}

  
  \item{newplot}{logical, TRUE=new plot
}
}
\details{Used internally in SELBUT as a replotting function
}
\value{list
  \item{ M}{x,y matrix of grid}
  \item{dx}{delta x}
  \item{dy}{delta y}
  \item{rx}{range of x}
  \item{ry}{range of y}
  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{SELBUT, swig
}
\examples{


STDLAB=c("DONE", "QUIT", "zoom.out", "zoom.in", "SELBUT", "FILT",
"UNFILT", "PSEL", "SGRAM", "WLET", "SPEC", "XTR" )
OPTREPLOT(STDLAB)


XMCOL =  setXMCOL()
YN = OPTREPLOT(XMCOL, cols =XMCOL, tcol=grey(.8)  ,
scol=  "transparent", bcol= "transparent",  mpct=0.05   )


YN = OPTREPLOT(XMCOL, cols =XMCOL, tcol=grey(.8)  ,
scol=  "transparent", bcol= "black",  mpct=0.05   )




}
\keyword{misc}
