\name{WriteCSV}
\alias{WriteCSV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WriteCSV
}
\description{
Writes comma seperated file
}
\usage{
WriteCSV(FileName, Data, Header, OutDirectory = getwd(),Numeric=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
name of the  file to be written
}
  \item{Data}{
 the data to be written
}
  \item{Header}{
  Optional, string matrix with column names}
  \item{OutDirectory}{
the directory where to write into; if not given: current dir.
}
  \item{Numeric}{if false, character dara can be written out
}
}
\details{
if Header=NULL and data has colnames, they automatically define the header.

Decimal seperator is the point "."

Encoding is UTF-8
}

\value{
the csv
}

\author{
Michael Thrun
}

\seealso{
\code{\link{WriteLRN}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{WriteCSV}% use one of  RShowDoc("KEYWORDS")
