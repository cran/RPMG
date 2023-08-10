\name{sepia.colors}
\Rdversion{1.1}
\alias{sepia.colors}
\alias{myhcl.colors}

\title{Sepia Color Palette
}
\description{Sepia Color Palette
}
\usage{
sepia.colors(n, k = 1)
myhcl.colors(n, k = 260)
}
\arguments{
  \item{n}{Number of colors
}
  \item{k}{Sepia starting color, hcl ending number
}
}
\details{There are two version of sepia in the code, each has a slightly
  different
  sepia end member.
}
\value{vector of Octal color codes
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{tomo.colors, pastel.colors, syscolors, helpcolors
}
\examples{

scol = sepia.colors(100)
SHOWPAL(scol)
see.pal(scol)

}
\keyword{misc}
