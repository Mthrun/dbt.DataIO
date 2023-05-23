\name{WriteNAMES}
\alias{WriteNAMES}
\title{Write NAMES to file}
\description{
Save names and possibly further texts to a *.names file.
}
\usage{
WriteNAMES(FileName, Names, Key, FurtherTexts, Header,OutDirectory,
Comments)
}
\arguments{
\item{FileName}{'String' , Name of the NAMES-file}
\item{Names}{vector (1:n), Names to write in this file}
optional:
\item{Key}{vector(1:n) , key numbers for NAMES-data, default vector from 1 till
length of Names}
\item{FurtherTexts}{matrix(1:n, 1:c), string matrix with row FurtherTexts to be
put in third column}
\item{Header}{vector (1:c+1), or vector (1:c+2) optional header for the Names and FurtherTexts:
Array of chars, this line wil be insterted at top of file with leading #}
\item{OutDirectory}{'string', in case of out directory should not be working
directory, default: \code{getwd()}}
\item{Comments}{vector, Char Array or matrix, these lines will be insteted at
top of file with leading #}
}
\details{
Note: No "#" or "\%" in Names or Description are allowed. 
Typical usages are
\preformatted{WriteNAMES('File',c('Name1','Name2','Name3','Name4','Name5'),
c(1:5))}
\code{WriteNAMES} is a function to store names and connect them with keys. Key can be named but does need to necessarily in the header.
}
\value{
creates a *.names file in the working or out directory
}
\examples{
Names=c("A","B","C")
FurtherText=cbind(c("Gen13","wdsg3","SAF"),c("sg","§sdg","sdg.sd"))
Key=c(200,450,23)
WriteNAMES("Testfile1",Names,Key,FurtherText)

Header=c("a a","b","c ")
WriteNAMES("Testfile2",Names,Key,FurtherText,Header=Header)

Comment="Test"
WriteNAMES("Testfile3",Names,Key,FurtherText,Header=Header,Comment=Comment)
}
  
\author{Michael Thrun}
\seealso{\link{ReadNAMES}}
\keyword{file, Names, NAMES}