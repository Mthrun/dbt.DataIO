\name{ReadQS}
\alias{ReadQS}
\title{ReadQS}
\description{
Reads compressed data from serialized format in parallelized manner for fast and
storage efficient data input/output
}
\usage{
ReadQS(FileName, Strict = FALSE, NThreads = 1, InDirectory=getwd())
}
\arguments{
\item{FileName}{Character with single file name}
\item{Strict}{Boolean (Default: Strict = FALSE).
TRUE:  Throw error
FALSE: Report warning}
\item{NThreads}{Integer defining the number of threads used for parallel 
computations. (Default: NThreads=1).}
\item{InDirectory}{Character with directory path (Default: InDirectory=getwd())}
}
\details{
\code{ReadQS} is the primary resource to read a *.qs file
For the corresponding write function see \code{WriteQS}
}
\value{
\item{QSFile}{A list with l numeric matrices containing n rows and m columns}
}
\author{Quirin Stier}
\examples{
\donttest{
library(qs)
library(FCPS)

FileName = "FCPS.qs"
Data = list(Hepta$Data, Atom$Data, Chainlink$Data, Lsun$Data, GolfBall$Data,
            TwoDiamonds$Data, WingNut$Data, Target$Data, Lsun3D$Data)
Names = c("Hepta", "Atom", "Chainlink", "Lsun", "GolfBall",
          "TwoDiamonds", "WingNut", "Target", "Lsun3D")

WriteQS(FileName = FileName, Data = Data, Names = Names)

V = ReadQS(FileName = FileName)
}
}
\seealso{ \code{WriteQS}, \code{addext}}
\keyword{file, qs, QS}
