\name{ReadFCSNavios}
\alias{ReadFCSNavios}
\title{Read FCS Navios}
\description{
Reads a FCS Navios file (.lmd). A LMD file contains two flowFrames. The
data and spillover matrix is chosen from the second flowFrame. The description
is taken from the first flowFrame.
Furthermore, this customized readFCS-function extracts the compensated data, the
required compensation matrix, which is the inverse of the spillover matrix, the
variable names (from @Description[["$PnS"]]) and the variable identifiers (from
@Description[["$PnN"]]).
}
\usage{
ReadFCSNavios(FileName, FilePath = "", VarsToCompensate = NULL,
              tryToCompensate = TRUE, Anonymize = FALSE,Silent=FALSE)
}
\arguments{
\item{FileName}{String of filename.}
\item{FilePath}{Optional: String of directory.}
\item{VarsToCompensate}{Optional: Vector of TRUE and FALSE for each variable in
the data. If TRUE, the variable with matching index will be compensated.If not
given, the function will try to find the right variables itself. Boolean Vector
with length of number of variables.}
\item{tryToCompensate}{Optional: tries to compensate the PlainData if a compensation
matrix can be found}
\item{Anonymize}{Optional: Boolean, if true an anonymization attempt is made to
remove common keywords containing information about patients.
THIS IN NO WAY MAKES DATA SAVE TO GIVE OUT!!}
\item{Silent}{Output compensated variables.}
}
\value{
\item{FCSData}{Object of class flowFrame. Relates to flowCore::read.FCS.}
\item{PlainData}{[1:n,1:d] Data in a plain matrix format. This is the data as it was found
in the lmd/fcs file which might already be compensated.}
\item{CompensationMatrix}{[1:d2,1:d2] Compensation Matrix if existing.}
\item{CompensatedData}{[1:n,1:d] Matrix with compensated Data.}
\item{VarNames}{[1:d] variable names (e.g. "FS_PEAK_LIN", "FS", "SS_PEAK_LIN",
"SS", "TCRgd-FITC", "CD45-KrO")}
\item{VarIdentifiers}{[1:d] detector names used by the machine to adress the
variable) (e.g. "FS-H", "FS-A", "SS-H", "SS-A","FL1-A", "FL1-H", "FL2-A",
"FL2-H",..., "TIME")}
\item{DeviceName}{Flowcytometer device name.}
}
\author{
Quirin Stier
}
%\examples{
%}
\keyword{FCS}