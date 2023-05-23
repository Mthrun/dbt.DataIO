ReadNAMES = function(FileName,InDirectory=getwd()){
  # Load *.names file
  # V<- ReadNAMES(FileName,InDirectory)
  # NamesKey = V$Key
  # Names    = V$Names
  # Rest     = V$FurtherTexts
  # Comment  = V$Comments
  #   
  #
  # INPUT
  # FileName                        filename of *.names file
  # OPTIONAL
  # InDirectory                       InDirectory where *.names file is, default: current dir
  #
  # OUTPUT
  # Key[1:d]                        vector, unique index from first column
  # Names[1:d]                      vector, Names contained in Column 2 as char(Names), without blanks
  # FurtherTexts [1:d,1:x]               vector or matrix, All in colums 3 and beyond 
  # Comments [1:ccc,]               vector or matrix,  string of all lines of Comments,  without the leading "#"
  
  #author: MT 04/2014

  if(grepl('.lrn',FileName)){
    stop('Please use ReadLRN()')
  }
  
  # if(grepl('.Data',FileName)){
  #   extensioncheck=FALSE
  #   stop('Please use ReadData()')
  # }
  ################################################################################################################
  ## Inputflow Kontrolle
  ################################################################################################################
  FileName  = addext(FileName,'names')
  checkFilename(FileName,Directory=InDirectory,Extension='names',ReadOrWrite=TRUE,NameOfFunctionCalled='ReadNAMES()')

  currentWD=getwd() #Aktuelles verzeichnis merken
  setwd(InDirectory) #Ins Verzeichnis wo sich Datei befindet wechseln

  ################################################################################################################
    #Beginn Daten einzulesen
    #Header einlesen mit HEADERSIZE
  ################################################################################################################
    
  tryCatch({
    
       check=T
      beginHeader = 1
      while(check){
        Header = readLines(FileName,n=beginHeader)[beginHeader]
        if(gregexpr('%',Header)[1]==1){#Prozentzeichen muss als erstes Zeichen einer Zeile kommen
          check=F
        }else{
          beginHeader = beginHeader+1 #Hier muss ich das dynamisch anpassen
        }
      }
      if(beginHeader>1){
        Comments=as.matrix(read.table(FileName, comment.char = "%", header=FALSE,  fill=TRUE, stringsAsFactors = FALSE, na.strings=c('NA','NaN'),nrows=beginHeader-1,blank.lines.skip=T))
      }else{
        Comments=NULL
      }
      if(!any(grepl('#',Comments))){
        Comments=NULL
      }
      try(
        if(!is.null(Comments)){
          Comments=sub('# ','',Comments)
          Comments=sub('#','',Comments)
          Comments=sub('\t','',Comments)
          Comments=paste(apply(Comments,1,paste,collapse=" "), collapse=" ")
        }
      )
      HeaderLines=readLines(FileName,n=beginHeader+2)
      
      leng_size=sum(lengths(regmatches(HeaderLines, gregexpr("%", HeaderLines))))
      ZahlPattern = "[0-9]+"
      
      if(leng_size==2){
        atomicVectorInd=regexpr(ZahlPattern, HeaderLines[beginHeader:(beginHeader+1)])
        StartRow = atomicVectorInd[1]
        StartCol = atomicVectorInd[2]
        EndRow = StartRow+attributes(atomicVectorInd)$match.length[1]-1
        EndCol = StartCol+attributes(atomicVectorInd)$match.length[2]-1
        rows=as.numeric(substr(HeaderLines[beginHeader],StartRow,EndRow))
        cols=as.numeric(substr(HeaderLines[beginHeader+1],StartCol,EndCol))
      }else{
        atomicVectorInd=regexpr(ZahlPattern, HeaderLines[beginHeader])
        StartRow = atomicVectorInd
        EndRow = StartRow+attributes(atomicVectorInd)$match.length-1 #
        rows=as.numeric(substr(HeaderLines[beginHeader],StartRow,EndRow)) # wie viele datenzeilen gibt 
        cols=NA
      }
      #Read Header
      Line=HeaderLines[beginHeader+2]#
        if(gregexpr('%',Line)[1]==1){#Prozentzeichen muss als erstes Zeichen einer Zeile kommen
          
        Line=sub('%','',Line)
        
        HeaderPatternStarts = "([[:alnum:]]|[[:punct:]])+"
        HeaderInd=gregexpr(HeaderPatternStarts,Line )
        Laengen = attributes(HeaderInd[[1]])$match.length
        Starts =   unlist(HeaderInd)
        Last = Starts+Laengen-1
        Header=substring(Line,Starts,Last)
        cols=length(Header)
        }else{
        #kein header da, legacy code
          Header=NULL #untern abgefangen
      }
    # cntnt = readLines(FileName)
    # # find end of head segment
    # nrOfRowsRow = 1
    # while(substr(cntnt[nrOfRowsRow], 1, 1) != "%"){
    #   nrOfRowsRow = nrOfRowsRow + 1
    # }
    # # QMS: Potenzielle Fehlergefahr
    # # invalid multibyte string, element 1
    # # Das kann an den deutschen Umlauten liegen
    # # Absicherung in Zukunft
    # Comments = ""
    # first = T
    # if(nrOfRowsRow>1){
    #   for(i in 1:(nrOfRowsRow-1)){
    #     if(substr(cntnt[i],1,1) == "#"){
    #       if(!first){
    #         Comments = paste0(Comments, '\n')
    #       }
    #       Comments = paste0(Comments, substr(cntnt[i],2, nchar(cntnt[i])))
    #     }
    #   }
    # }
    # 
    # ZahlPattern = "[0-9]+"
    # atomicVectorInd=regexpr(ZahlPattern, cntnt[nrOfRowsRow])
    # StartRow = atomicVectorInd
    # EndRow = StartRow+attributes(atomicVectorInd)$match.length-1 #
    # rows=as.numeric(substr(cntnt[nrOfRowsRow],StartRow,EndRow)) # wie viele datenzeilen gibt es
  },
  error = function(c){
    warning(c)
    stop("Header or Comments are not reasonably defined, see Subversion/PUB/ZFileFormatDocuments for further instructions")
  }
  )
  
  # 
  # content=readLines(FileName, warn = F)
  # x = content
  # contentTabs = stringr::str_locate_all(x[(beginHeader+1):length(x)],"\\t")
  # countTabs = sapply(contentTabs, nrow)
  # expectedCols = countTabs[1]
  # 
  # unexpectedCols = 4
  # for(unexpectedCols in setdiff(unique(countTabs), expectedCols)){
  #   warning(paste0("The following Lines have ", unexpectedCols, " Columns, instead of the ", expectedCols, " expected Columns from the first datarow: ",
  #   paste(which(countTabs == unexpectedCols) + beginHeader, collapse = ", ")), "\n The difference will be cut off or filled up with NaNs. This may lead to mismatched columns in the respective rows!")
  # }
  
  
  
  ################################################################################################################
  ## Auslesen des Datensatzes
  ################################################################################################################
  #gibt fehler meldung aus wenn columns kleiner als 5 Datensätze sind.
  #Grund ist eine C-Funktion die die ersten 5 Zeilen einliest um die Daten zubestimmen
  #mehr infos: http://stackoverflow.com/questions/5990654/incomplete-final-line-warning-when-trying-to-read-a-csv-file-into-r
  tryCatch({
    Data = read.table(FileName,
                      sep='\t',
                      quote = "", #To disable quoting altogether
                      comment.char = "%",   #comments are already read in, catch legacy names that has only one % and new names with two %
                      header=FALSE,         #Wenn die erste Zeile der Header ist. zB. bei csv
                      fill=T, 
                      flush = T,
                      stringsAsFactors = FALSE, 
                      na.strings=c(''),
                      skip=beginHeader)#,    #Die ersten HEADERSIZE zeilen werden nicht nochmal gelesen
    ncols=ncol(Data)
  if(!is.null(rows))
    if(rows!=nrow(Data))
      warning(paste0('ReadNAMES(): Number of rows ',nrow(Data),' does not equal number of rows in header ',rows))
  
  },
  error = function(c){
    print("Key, Names or Descriuption are not reasonably defined, see Subversion/PUB/ZFileFormatDocuments for further instructions")
  }
  )
  
  ################################################################################################################
  ## Outputflow Kontrolle
  ################################################################################################################
  if(is.finite(cols)){
    if(ncol(Data)!=cols){
      warning(paste('rows of Key+Names+FurtherDescription(if exists)',ncol(Data),'does not equal number of columns stated in the header',cols))
    }
  }#otherwise old format, do nothin
  KeyColumn=1
  Key = Data[,KeyColumn]  #Umspeichern in Key 
  
  Data=Data[,-(KeyColumn)]  #Key als Spalte Loeschen
  Data=data.frame(Data) #Dataframe mit einer spalte == liste -> muss wieder data.Frame sein
  
  #zurück wechseln ins alte Verzeichnis
  setwd(currentWD)
  
  
  # Names & FurtherTexts
    NamesColumn = 2-KeyColumn
    Names=Data[,NamesColumn]
  Names=as.character(Names)
  if(sum(!is.finite(suppressWarnings(as.numeric(Key))))==0)
  	mode(Key)='numeric'
  
  if(ncols>2){
    FurtherTexts=Data[,-NamesColumn]
    if(is.null(Header)){
      Header=c("Names",paste0("C",1:ncol(FurtherTexts)))
    }else{
      Header=Header[-1]#no key
    }
    FurtherTexts=as.matrix(FurtherTexts)

    HeaderFurtherText=Header[-1]#np neames
    
    if(length(HeaderFurtherText)==ncol(FurtherTexts))
      colnames(FurtherTexts)=HeaderFurtherText#tail(Header[-1],ncol(FurtherText))#no key -> "-1

    result=list(Key=Key,Names=Names,FurtherTexts=FurtherTexts,Header=Header,Comments=Comments)
  }else{
    if(is.null(Header)){
      Header="Names"
    }else{
    Header=Header[-1]#no key
    }
  
    result=list(Key=Key,Names=Names,Comments=Comments,Header=Header)
  }

  return (result)
}
