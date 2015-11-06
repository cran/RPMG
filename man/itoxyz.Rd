\name{xyztoi}
\alias{xyztoi}

\title{Matrix Index to Vector index}
\description{
  Given ix, iy, iz index get I.
}
\usage{
xyztoi(ix, iy,iz,nx, ny, nz)
}

\arguments{
  \item{ix}{index to col vector}
  \item{iy}{index to row vector}
  \item{iz}{index to (depth) layer vector}
  \item{nx}{number of blocks in x axis}
  \item{ny}{number of blocks in y axis}
  \item{nz}{number of blocks in z axis (layers) }
}

\value{
  \item{i}{Index of matrix}
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{itoxyz}

\examples{

k = itoxyz(24, 6, 6, 1)
xyztoi(k$ix, k$iy, k$iz, 6, 6, 1)

nx = 20
ny = 20
nz = 40

k = itoxyz(2440, nx, ny, nz)
xyztoi(k$ix, k$iy, k$iz, nx, ny, nz )



}

\keyword{misc}
