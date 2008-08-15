\name{chooser}
\alias{chooser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Iteractive Selection Winder}
\description{
 Choose an option from a selection
}
\usage{
chooser(opts=c(1, 2, 5, 10, 15, 20) , ncol=5, nsel=NA, newdev=TRUE, STAY=FALSE,
           cols="red", main="", newplot=TRUE, xlim=c(0,1), ylim=c(0,1),
just="CEN", ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{opts}{list of options}
  \item{ncol}{number of columns}
  \item{nsel}{number of selections}
 \item{newdev}{ logical, TRUE=start new device, default=TRUE}
  \item{STAY}{ logical, TRUE=keep same device when done, default=FALSE}
  \item{cols}{colors for buttons, default = pastel.col(N)}
  \item{main}{title for screen (maybe instructions for picking) }
 \item{newplot}{logical, TRUE means start a new plot}
 \item{xlim}{xlim on the plot}
 \item{ylim}{ylim on the plot}
 \item{just}{character, justification in box, one of CEN, LEFT, RIGHT }
 
\item{\dots}{additional parameters from par, used for font, cex, etc...
}
 
  
}
\details{
  Used for interactive selections of numeric or other options.
  If the input vector is all numeric, a numeric value is returned.  If,
  on the other hand, the input is mixed or character, a character
  vector is returned. If the selection number nsel
  is left blank, it is set at 1.  If it is
  specified, selection can be truncated
  by clicking the right mouse.
}
\value{
 vector of selections.
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{locator}
\examples{

 k = letters[1:26]
    
pk = chooser(opts=k , nsel=3 )

print(pk)


 k = c( 1:26, letters[1:26])
    
pk = chooser(opts=k , nsel=3 )

print(pk)


k = 1:12
    
pk = chooser(opts=k , nsel=3 )

print(pk)
##################

plot(runif(10, 1, 100), runif(10, 1, 100), type='n')

APAL = c('tan2','red2','lightpink3','chocolate4','blue3','thistle4',
  'lightcyan4',
      'orangered1','purple4','darkred','dodgerblue1','gold3','chartreuse',
  'sienna4')

##   nchar( APAL )
wm = which.max(nchar( APAL ))
swidth = strwidth(APAL[wm])


upar = par("usr")

mhgt = sum( strheight(APAL )+0.5*strheight(APAL ))

mwid = max( strwidth(APAL) )

mwid = mwid + 0.05*mwid

chooser(opts=APAL  , ncol=1, nsel=NA, newdev=FALSE, STAY=TRUE,
           newplot=FALSE, xlim=c(upar[1], upar[1]+mwid) , ylim=c( (upar[4]-mhgt),upar[4]) , main=""  )






}

\keyword{iplot}
