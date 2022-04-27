\name{WriteFCS}
\alias{WriteFCS}
\title{Write FCS File}
\description{
This function writes uncompensated raw flowcytometry data from lmd or fcs files
to a simplified FCS format (FCSVersion = "3").
This format does not carry specific information about the data collection 
process which concerns the parameter '@description' of the flowFrame which is
the data format of the FCS file.
This means that given information from a fcs or lmd file is partially discarded.
More precisely, this format carries only the data and the variable names.
Especially, there is no compensation matrix left.

In order to obtain an appropriate workflow with this method, retrieve the
raw data from a fcs or lmd file with FCSVersion 3 and an existing dataset.
There can be multiple datasets within one fcs or lmd file.
ReadFCS (FCS) or ReadFCSNavios (LMD) ensures the correct retrieving of the raw
data.
The data can be processed further if desired.
For example, a cluster analysis can be executed and the resulting labels can be
saved using parameter 'Cls'.

The resulting fcs file can be read and visualized in Kaluza software.
Potential cluster labels (concerning parameter 'Cls') can be colored within the
software environment.
Within the fcs format (flowFrame), only the @exprs and @parameter are kept with
full information.
The '@description' is removed but maybe false values are added depending on the
read function!
Do NOT rely on any information from the '@description' parameter!
This method is purely designed to read and write medical data in order to
analyze and for data scientists who are experts in the field of flowcytometry.
}
\usage{
WriteFCS(FileName, Data, DeviceName, SpilloverMatrix = NULL, VarIdentifiers = NULL,
               VarNames = NULL, Cls = NULL, OutDirectory = NULL)
}
\arguments{
\item{FileName}{String for name of the file.}
\item{Data}{[1:n, 1:d] Numerical katrix  with n observations and d features
carrying flowcytometry data values.}
%\item{Compensation}{Numerical Matrix [1:d2, 1:d2] carrying the compensation
%matrix which was computed as inverse of the Spillover matrix from a FCS file.}
\item{DeviceName}{String name of the flowcytometer device. Crucial attribute for
displaying the data in Kaluza correctly.}
\item{SpilloverMatrix}{[1:d2, 1:d2] Numerical matrix with spillover matrix 
for a selection of variables (see ReadFCS(Navios)).}
\item{VarIdentifiers}{Optional: [1:d] String vector for detector names.}
\item{VarNames}{Optional: [1:d] String vector for input variables.}
\item{Cls}{Optional: [1:n] Integer vector with classes for each datapoint.}
\item{OutDirectory}{Optional: String for name of the file's directory.}
}
\author{
Quirin Stier
}
%\examples{
%}
\keyword{FCS}