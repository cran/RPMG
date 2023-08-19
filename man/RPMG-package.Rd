\name{RPMG-package}
\alias{RPMG-package}
\alias{RPMG}
\docType{package}
\title{
Really Poor Man's GUI: sets up buttons for a graphical user interface in
R
}
\description{
  Really Poor Man's Graphical User Interface, used to create interactive
  R analysis sessions with simple R commands.  
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\keyword{ package }
\seealso{rowBUTTONS, whichbutt}
\examples{
###  get sample image data set.
data(volcano)
#####   set sample interval unit
attr(volcano, 'dx') =10
attr(volcano, 'dy') =10
###  create the list of labels
###  Actions for these buttons are described in the calling program XSECDEM
mybutts = c("DONE", "REFRESH", "rainbow", "topo", "terrain", "CONT", "XSEC","PS" )
XSECDEM(volcano, mybutts)
############################################# 
#############################################  CODE STUB
\dontrun{###   Example code chunk:
###  general set up of RPGM usage:
######  make a plot
#######  set buttons
buttons = rowBUTTONS(c("BUT1","BUT2") , col=c(1,1), pch=c(1,1))
#######  after plotting, locate in plot....
zloc = locator()
Nclick = length(zloc$x)
#############  the last click on the screen before stopping (middle
#############        mouse click) is used to set the action
K =  whichbutt(zloc , buttons)
while(TRUE)
      {
	if(K[Nclick] == match("BUT1", labs, nomatch = NOLAB))
          {
            ###  do what ever button 1 is supposed to do
          }
	if(K[Nclick] == match("BUT2", labs, nomatch = NOLAB))
          {
            ###  do what ever button 2 is supposed to do
          }
	}  ## end while loop
}



}
