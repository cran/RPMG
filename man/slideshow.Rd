\name{slideshow}
\alias{slideshow}
\title{SlideShow
}
\description{MAke a slide show similar to Powerpoint
  presentations
}
\usage{
slideshow(P = c("hi", "there", "sugar pie"), dy = 0.2, EX = 0.1, ht = 3, font = 2, anim = FALSE)
}
\arguments{
  \item{P}{vector of character strings to display
}
  \item{dy}{vertical spacing, percentage 
}
  \item{EX}{horizontal offset, percentage
}
  \item{ht}{Character expansion, see par
}
  \item{font}{Font choice, see par
}
  \item{anim}{logical, Animation, TRUE=means animate the input line-by-line
}
}
\details{
  The function is meant to be used in presentations when
  R is running a script and text needs to be displayed to
  explain the talk.  The animation is controlled by
  clicking on the screen using locator(1) function.
}
\value{Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

Ptext1 = c("New Package: Rquake", "Earthquake Location",
  "Inverse Theory",
  "Graphics",
  "Statistical Analysis" )

slideshow(Ptext1, ht=3, anim=FALSE )

}
\keyword{misc}
