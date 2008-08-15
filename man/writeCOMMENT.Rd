\name{writeCOMMENT}
\alias{writeCOMMENT}
\alias{writeCOMMENT2}

\title{write Code Comments}
\description{
  Create a print out of comments for insertion in computer code.
  Used for separating important blocks of code
  with helpful, easy to find comments.
}
\usage{
writeCOMMENT(temp, space = " ", letspace = "", MSUB = "0", prefix = "", suffix = "")
}
\arguments{
  \item{temp}{text string}
  \item{space}{space between words}
  \item{letspace}{space between letters}
  \item{MSUB}{text, substitute character, if this is "ALL", then each
    letter is substituted. default=NULL}
  \item{prefix}{prefix before the letters}
  \item{suffix}{suffix after the letters }
  
}
\details{
  This is a function used for creating comments
  in computer code.
  Letters are a fixed height of 7 lines
}
\value{
  \item{List}{26 letters}
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Code dumps to the screen, then you must paste in code.
  If sent in an email, spaces are not preserved.
  The letters are stored in the routine, these can be changed,
  but the constant (7 lines)  common height should be preserved.
  Each letter should be one block.
}

\examples{


 writeCOMMENT("GO TARHEELS", space="      ", letspace = "", MSUB="ALL", prefix="/*" , suffix="*/" )
   writeCOMMENT("START", space="      ", letspace = "", MSUB="ALL", prefix="#######" )
 writeCOMMENT("J M lees", space="      ", letspace = "", MSUB="0" )

 writeCOMMENT("J. M. Lees", space="      ", letspace = "", MSUB="0" )
 writeCOMMENT("J. M. Lees", space="      ", letspace = "", MSUB="." )

writeCOMMENT("J. M. Lees", space="      ", letspace = "" )
writeCOMMENT("J. M. Lees", space="---------", letspace = "" )

writeCOMMENT("J. M. Lees", space="      ", letspace = "", MSUB="ALL" )
writeCOMMENT("J_M_Lees", space="      ", letspace = "", MSUB="ALL" )




  writeCOMMENT("abcdefghi")
writeCOMMENT("jklmnop")

writeCOMMENT("qrstuvwxyz")
writeCOMMENT("1234567890")
writeCOMMENT("WHY?!.-+=_")
writeCOMMENT("2+2=4")
writeCOMMENT("e*exp(pi*i)=-1")





}
\keyword{misc}
