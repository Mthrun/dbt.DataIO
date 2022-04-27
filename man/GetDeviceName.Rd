\name{GetDeviceName}
\alias{GetDeviceName}
\title{Get Name of Flowcytometer Device}
\description{
Extracts the name of the flowcytometer device from flowcytometer data.
This data must be given as object of flowFrame-class {flowCore}.
This data can stem from a FCS or a LMD file.
}
\usage{
GetDeviceName(flowFrame)
}
\arguments{
\item{flowFrame}{Object of flowFrame-class {flowCore}.}
}
\value{
List of
\item{DeviceName}{String name of flowcytometer device.}
}
\author{
Quirin Stier
}
%\examples{
%}
\keyword{FCS}