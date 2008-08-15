\name{rowBUTTONS}
\alias{rowBUTTONS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometry for the Really Poor Man's GUI  }
\description{
  Create a set of buttons and associated geometry for RPMG
}
\usage{
rowBUTTONS(labs, col = 6, pch = 4, cex=1, boxsize = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labs}{Vector of labels for the buttons running across the top
    and bottom of the plot }
  \item{col}{Optional vector of colors for the buttons }
  \item{pch}{Optional vector of symbols to be plotted in the center of
    the buttons }
  \item{cex}{optional character expansion for text}
  \item{boxsize}{optional box size for the buttons, default=-1
  where the size is adjusted for string size}
  
}
\details{
  rowBUTTONS is called after the R graphic has been created so
  the geometry of the buttons can be set.  Subsequent calls to whichbutt
  use the geometry to determine which button has been selected.
  Some of the parameters chosen here are controlled by par-like parameters.
}
\value{
  The function returns a list of buttons and the associated geometry.
  
  \item{N}{Number of Buttons}
  \item{labs}{Names of the Buttons}
  \item{x1}{vector of left x-coordinates for the buttons}
  \item{x2}{vector of right x-coordinates for the buttons}
  \item{y1}{vector of  top y-coordinates for the buttons}
  \item{y2}{vector of bottom  y-coordinates for the buttons}
  
}
\author{Jake Anderson and Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{ 
  rowBUTTONS uses the current plotting parameters from par()
  to set the geometry.  If the window is resized, rowBUTTONS should be
  reset to extract correct button position.  In interactive mode
  this is done each time the plot is refreshed.
}
\seealso{whichbutt, par}
\examples{

#######  create a plot
plot(c(0,1), c(0,1))
#######  set the character vector of button labels
mybutts = c("DONE", "REFRESH", "rainbow", "topo", "terrain", "CONT",
"XSEC","PS" )
#########  set colors and plotting chars for buttons
colabs = rep(1, length=length(mybutts))
pchlabs = rep(0,length(mybutts))
#######  create and set geometry for buttons:
buttons = rowBUTTONS(mybutts, col=colabs, pch=pchlabs)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iplot}
