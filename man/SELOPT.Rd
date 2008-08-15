\name{SELOPT}
\alias{SELOPT}
\title{Select Options
}
\description{Select buttons interactively.
}
\usage{
SELOPT(OPTS, onoff = -1, ncol=5, ocols = "white",
       cex=1, default="opt" )
}
\arguments{
  \item{OPTS}{character list of buttons
}
  \item{onoff}{which buttons are active, onoff=-1 turns all buttons off,
    onoff=0 turns all buttons on, any other vector is an index vector to
    selected options
    
  }
  \item{ncol}{number of columns, default = 5}
  
  \item{ocols}{colors for plotting option boxes
  }
   \item{cex}{character expansion for text in boxes}
 \item{default}{default vector of options
  }

  
}
\details{Used in swig.
  OPtions can be added, subtracted, deleted, or completely
  filled out based on interactive choice.
}
\value{character list of selected options }

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{OPTREPLOT, chooser
}
\examples{

\dontrun{
STDLAB=c("DONE", "QUIT", "zoom.out", "zoom.in", "SELOPT",
"FILT","UNFILT",
 "PSEL", "SGRAM", "WLET", "SPEC", "XTR" )
onoff = rep(0, length(STDLAB))
onoff[1:5] = 1
SELOPT(STDLAB, onoff=onoff)

###   second option for selecting colors
dev.new(width=12, height=12)

scol = SELOPT(colors(),  onoff=-1, ncol=15, ocols =colors(), cex=.6 )

###  old program
SHOWPAL(scol, NAME=TRUE)

###  show the options chosen from top to bottom
OPTREPLOT(scol,  cols=scol,  scol="green", bcol="blue", slwd=15 )


}

}
\keyword{misc}
