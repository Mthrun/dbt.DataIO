\name{ReadNAMES}
\alias{ReadNAMES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ load *.names files}
\description{
ACHTUNG: Neu programiert, Doku in Header der Funktion lesen!
  easily load dbt files in the NAMES format
  see /dbt/ZFileFormatDocuments/names.html
}
\usage{
ReadNAMES(FileName, InDirectory = getwd())
}
\arguments{
\item{FileName}{filename of *.names file}
\item{InDirectory}{InDirectory where *.names file is, default: current dir}
}
\details{
  \code{ReadNAMES} may have a *.names extension, if not, it will be added automatically,
  and only files with a .names extension can be loaded. 
}
\value{
  a list of length 5 containing the following:
  \item{Key}{[1:n], the unique key of the names, unique index from first column}
  \item{Names}{(1:n)  the names themselves, Names contained in Column 2 as a character vector}
  \item{FurtherText}{ (1:n,1:rr) additional info, optional, All in colums 3 and beyond}
  \item{Comment}{Optional, additional info,  Char Array of all lines of Commens,  without the leading "#"}
  \item{Header}{Either (1:rr+1) the header, character vector of all columns except the key}
    

}

\author{Michael Thrun}

\seealso{ \code{addext}, \code{WriteNAMES}, }

\keyword{ file, Names, NAMES }