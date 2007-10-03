\name{whichbutt}
\alias{whichbutt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determines which button was selected in RPGM}
\description{
  Function to determine which button of the RPMG was selected during a
  graphics session.
}
\usage{
whichbutt(v, buttons)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{list of x,y coordinates obtained from the locator() function }
  \item{buttons}{ list of buttons set by the function rowBUTTONS }
}
\details{
  whichbutt uses the geometry determined by rowButtons
  and a list of locator() points to return the buttons clicked on
  or, if none, 0.

}
\value{
  Returns a vector of indexes to buttons selected by the user.
  Buttons are numebred 1-N so if a click is not on a button, zero is returned.
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{
  This function can be used to get interaction with
  predined buttons and non-button clicks using locator().
}
\seealso{rowBUTTONS, locator }
\examples{

###################  initial plot
plot(c(0,1), c(0,1))
######  set buttons
mybutts = c("DONE", "REFRESH", "rainbow", "topo", "terrain", "CONT",
"XSEC","PS" )
colabs = rep(1, length=length(mybutts))
pchlabs = rep(0,length(mybutts))
######  set button geometry  
buttons = rowBUTTONS(mybutts, col=colabs, pch=pchlabs)
########  user clicks on plot.  When locator finishes, whichbutt
########  determines which buttons were selected and returns the vector
L = locator()

K = whichbutt(L, buttons)
print(K)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iplot}
