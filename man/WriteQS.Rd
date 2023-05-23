\name{WriteQS}
\alias{WriteQS}
\title{WriteQS}
\description{
Compresses data to serialized format in parallelized manner for fast and storage
efficient data input/output
}
\usage{
WriteQS(FileName, Data, Names = NULL, Preset = "high", Algorithm = "zstd",
Compress_level = 4L, Shuffle_control = 15L, Check_hash = TRUE, NThreads = 1,
OutDirectory = getwd())
}
\arguments{
\item{FileName}{ string, name of the  file to be written}
\item{Data}{Two input options: [1:n, 1:m] Numeric matrix OR [1:l] List of l numeric matrices of
dimensions [1:n, 1:m]}
\item{Names}{Character vector with}
\item{Preset}{\code{\link{qsave}}}
\item{Algorithm}{\code{\link{qsave}}}
\item{Compress_level}{\code{\link{qsave}}}
\item{Shuffle_control}{\code{\link{qsave}}}
\item{Check_hash}{\code{\link{qsave}}}
\item{NThreads}{\code{\link{qsave}}}
\item{OutDirectory}{Character with directory path (Default:
OutDirectory=getwd())}
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
\seealso{\code{ReadQS}, \code{addext}}
\keyword{file, qs, QS}