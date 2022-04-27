Append2LRN = function(FileName, Data, Key, OutDirectory=getwd()){
# DESCRIPTION
#
# INPUT
# FileName                         name of the  file to be written
# Data(1:n,1:d)                    matrix of data , cases in rows , variables in columns, may contain NaN, NA or any other missing number symbol
#                                  for is.infinite(x) and  is.nan(x) all these x are written as NaN
#
# OPTIONAL
# OutDirectory                    the directory where to write into; if not given: current dir.
#
#1.Editor: QMS 2021
  if(is.list(Data)){
    stop('Data is a list, but it ought to be a matrix')
  #WriteLRN(FileName=FileName,Data=Data$Data,Key=Data$Key,Header=Data$Header,DataDefined=Data$DataDefined,OutDirectory=OutDirectory)
  }
  if(is.vector(Data)){Data=as.matrix(Data)
    warning('Data is a vector, but it ought to be a matrix')
  }
  if(!is.matrix(Data)){
    stop('Data is not a matrix')
  }
  CurrentDir = getwd()
  setwd(OutDirectory)
  
  #TmpV = NULL
  # Try to open LRN if existing
  #try({
  #  TmpV = ReadLRN(FileName = FileName, InDirectory = OutDirectory)
  #})
  # Return if there is no LRN file
  #if(is.null(TmpV)){
  #  stop(paste0("File ", FileName, " does not exist. Use ReadLRN and not AppendLRN!"))
  #}
  
  # Open Connection
  if(substr(FileName, nchar(FileName)-3, nchar(FileName)) != ".lrn"){
    FileName = paste0(FileName, ".lrn")
  }
  #----------------------------------------------------------------------------#
  # Adjust number of data points
  Connection = file(FileName, "r+")        # Open Read + Write
  Line = readLines(Connection, n = 2)
  Marker = 1                               # Check if there is a commentary in line 1
  if(substr(Line[Marker], 1, 1) == "#"){   # Commentary in line 1 => number of data points in line 2
    Marker = 2                             # No commentary in line 1 => number of data points in line 1
  }                                        
  NumberLine = Line[Marker]
  
  NumData = as.numeric(gsub(".*?([0-9]+).*", "\\1", NumberLine))
  LengthLine = nchar(NumberLine)
  OpenPlaceHolders = LengthLine - 3 - nchar(NumData)
  NewNumData = NumData + length(Key)
  NewOpenPlaceHolders = OpenPlaceHolders - (nchar(NewNumData) - nchar(NumData))
  PlaceHolders = paste(rep("x", NewOpenPlaceHolders), sep="", collapse="") 
  PlaceHolderNumColumns = paste0(PlaceHolders, NewNumData)
  Line[Marker] = paste0("%\t ", PlaceHolderNumColumns)
  Line[1] = paste0(Line[1], "\n")
  Line[2] = paste0(Line[2], "\n")
  writeLines(text = Line, con = Connection, sep = "")
  close(Connection)
  #----------------------------------------------------------------------------#
  # Add data
  Connection = file(FileName, "r+")
  # Update number data written to LRN
  NCols = dim(Data)[2] + 1
  for(j in 1:length(Key)){
    write(x = c(as.integer(Key[j]), Data[j,]),
          file = FileName, append=TRUE, ncolumns = NCols, sep = "\t")
  }
  # Close Connection to opened file and end appending process
  close(Connection)
  #----------------------------------------------------------------------------#
  # Change back to origin working directory
  setwd(CurrentDir)
}
