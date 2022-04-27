\name{CompensationToSpillover}
\alias{CompensationToSpillover}
\title{From Compensation Matrix to Spillover Matrix}
\description{
This function inverts/solves the compensation matrix in order to retrieve the
spillover matrix.
FCS files contains spillover matrices.
ReadFCS and ReadFCSNavios uses the inverse of the spillover matrix to yield a
compensation matrix.
The compensation matrix is used in order to compute compensated data from
FCS (Version 3), which contains uncompensated data and a spillover matrix.
WriteFCS can write FCS files with an optional spillover matrix.
This function comes in handy, when working with ReadFCS/ReadFCSNavios and
WriteFCS.
Use this function to convert from compensation matrix to spillover matrix.
}
\usage{
CompensationToSpillover(CompensationMatrix)
}
\arguments{
\item{CompensationMatrix}{[1:d, 1:d] Numerical matrix with compensation matrix
for given variables.}
}
\value{
List of
\item{SpilloverMatrix}{[1:d, 1:d] Numerical matrix with spillover matrix for given variables.}
}
\author{
Quirin Stier
}
%\examples{
%}
\keyword{FCS}