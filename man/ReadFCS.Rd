\name{ReadFCS}
\alias{ReadFCS}
\title{Read FCS}
\description{
Reads a FCS file (.fcs). A FCS file can contain more than one flowFrame. The
user can choose which flowFrame to read.
Furthermore, this customized readFCS-function extracts the compensated data, the
required compensation matrix, which is the inverse of the spillover matrix, the
variable names (from @Description[["$PnS"]]), the variable identifiers (from
@Description[["$PnN"]]) and if existing, the classes from a possible clustering.
}
\usage{
ReadFCS(FileName, FilePath = "", Dataset = NULL, VarsToCompensate = NULL,
              tryToCompensate = T, Anonymize = F)
}
\arguments{
\item{FileName}{name of the file.}
\item{FilePath}{Optional: name of the file's directory}
\item{Dataset}{Optional: in FCS files, multiple datasets can be saved. If
another than the last shall be loaded, this gives its index.}
\item{VarsToCompensate}{Optional: Vector of TRUE and FALSE for each variable in
the data. If TRUE, the variable with matching index will be compensated.If not
given, the function will try to find the right variables itself. Boolean Vector
with length of number of variables.}
\item{tryToCompensate}{Optional: tries to compensate the PlainData if a compensation
matrix can be found}
\item{Anonymize}{Optional: Boolean, if true an anonymization attempt is made to
remove common keywords containing information about patients.
THIS IN NO WAY MAKES DATA SAVE TO GIVE OUT!!}
}
\value{
List of
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
\item{Cls}{[1:n] Classes from Clustering.}
}
\author{
Quirin Stier
}
%\examples{
%}
\keyword{FCS}