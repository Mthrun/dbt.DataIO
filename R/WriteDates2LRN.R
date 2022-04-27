WriteDates2LRN=function(FileName, Data, DatesTime, Header, DataDefined=c(), OutDirectory=getwd(),  CommentOrDigits=NULL, Digits = NULL, scientific = F){
  # WriteDates2LRN(FileName,Data,Header,Time,DataDefined,OutDirectory,CommentOrDigits);
  # WriteDates2LRN(FileName,Data,Header,Time,c(),OutDirectory);
  # save data, column Header, and column type definition to a *.lrn file, converts TSAT::ReadDates to LRN for ALU
  #
  # INPUT
  # FileName                         name of the  file to be written
  # Data(1:n,1:d)                    matrix of data , cases in rows , variables in columns, may contain NaN, NA or any other missing number symbol
  #                                  for is.infinite(x) and  is.nan(x) all these x are written as NaN
  # DatesTime                        Dates that define the Key in the LRN, if R Dates, then "-" is just deleted, otherwise as-numeric is called
  #
  # OPTIONAL
  # OutDirectory                    the directory where to write into; if not given: current dir.
  #
  # Header(1:d) OR Header(1:d+1)    cell array or string matrix with column Header (d data columns) plus eventual header for Key as first entry
  #
  # DataDefined(1:d+1)              vector of column type: keys (0=ignore,1=data,3=class,9=key)
  #                                 default is leading 9 and d-times 1
  #
  # CommentOrDigits									either a string which is inserted as CommentOrDigits in the first line in the file
  #                                 if it is a number then it is the number of significant digits (after the "."), default 6
  # Digits                          number of significant digits
  # scientific                      flag wheather to use scientific notation, default false
  #author: MT 2021
  #example
  #ElectricityBRD=TSAT::ElectricityBRD
  #WriteDates2LRN("ElectricityBRD",ElectricityBRD$Mrd_KWh,ElectricityBRD$Time)
  
  if(lubridate::is.Date(DatesTime)){
    Key=as.numeric(gsub("-","",as.character(DatesTime)))
  }else{
    Key=as.numeric(DatesTime)
  }
  if(missing(Header))
    Header=colnames(Data)
  

  WriteLRN(FileName = FileName, Data = Data, Header = Header, Key=Key, DataDefined = DataDefined, OutDirectory = OutDirectory,  CommentOrDigits = CommentOrDigits, Digits = Digits, scientific = scientific)
}