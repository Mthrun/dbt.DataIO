WriteCSV = function(FileName, Data, Header, OutDirectory=getwd(),Numeric=TRUE)
# WriteCSV(FileName,Data,Header,OutDirectory)
# write a Coma-separated file inclunding Header == Names
#
# INPUT
# FileName                 name of the  file to be written
# Data(1:n,1:d)            the data to be written
#
# OPTIONAL
# Header(1:d,1:s) or []     string matrix with column names, if Header=NULL and data has colnames, they automatically define the header  	
# OutDirectory             the directory where to write into; if not given: current dir.
# Numeric                   if false, character dara can be written out
# author MT 2019
{
  checkFilename(FileName,Directory=OutDirectory,Extension='lrn',ReadOrWrite=FALSE,NameOfFunctionCalled='WriteCSV()')
  
  if(is.list(Data)){stop('Data is a list, but it ought to be a matrix')
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
  
  # if data is a list of length 3 -> data is the result of ReadLRN -> recursion
  if (mode(as.matrix(Data))=='list' && Header(Data)==c('Data', 'Key','DataHeader')){
    WriteLRN(FileName, Data$Data, Data$DataNames, OutDirectory)
  }
  else{
    
    
    # make sure extension is there ('.csv') 
    FileName = addext(FileName,'csv')
    
  HeaderGiven=TRUE
    n = nrow(Data)
    m = ncol(Data)
    if(!missing(Header)){
      if(is.null(Header)){
        if(!is.null(colnames(Data)))
          Header=colnames(Data)
      }
    }else{
      HeaderGiven=FALSE
    }
    # artificial Header if necessary
    # if a correct number of headers is given, use them
  if(isTRUE(HeaderGiven)){
      if(length(Header) != m){ 
        if(!is.null(Header)){warning(paste0('Length of Header ',length(Header),' unequal length of columns m = ',m,'. Setting Header to Default C1...Cm.'))}
        message()
        Header = paste('C',1:m,sep='')
      }
      
      # remove all Blanks from header
      Header = sub(' ', '_', Header)
    }
    ### write in file
    # write dimensions Number of lines & columns
    header = c(paste('%\t',n),paste('%\t',m))
    
    write.table(header, FileName, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
    
    # write 'Header'-line
    if(isTRUE(HeaderGiven)){
      cat('% ', file=FileName, append=TRUE)
      for(i in 1:length(Header))
        Header[i]=sub(' ','',Header[i]) #Blanks ersetzen
      cat(Header,'\n', file=FileName, append=TRUE, sep='\t')
    }
    if(isTRUE(Numeric)){
      if(mode(Data)!="numeric"){ #MT: Abfangen von Strings und character
        warning('Beware, non numeric values found. Trying automatic Translation into numeric data mode')
        mode(Data) <-"numeric"
      }
      
      DataFin = Data
      DataFin[!is.finite(DataFin)] = NaN
    }else{
      if(mode(Data)!="character"){ #MT: Abfangen von Strings und character
        warning('Beware, non character values found. Trying automatic Translation into character data mode')
        DataFin = Data
        mode(DataFin) <-"character"
      }
    }
    
    # write data
    write.table(DataFin, file=FileName, append=TRUE, quote=FALSE, sep=',', row.names=FALSE, col.names=FALSE, na='NaN',dec = '.',fileEncoding = "UTF-8")
  }
  
  setwd(CurrentDir)
}
