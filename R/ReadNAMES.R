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
  cntnt = readLines(FileName)
  # find end of head segment
  nrOfRowsRow = 1
  while(substr(cntnt[nrOfRowsRow], 1, 1) != "%") nrOfRowsRow = nrOfRowsRow + 1
  
  Comments = ""
  first = T
  if(nrOfRowsRow>1){
    for(i in 1:(nrOfRowsRow-1)){
      if(substr(cntnt[i],1,1) == "#"){
        if(!first) Comments = paste0(Comments, '\n')
        Comments = paste0(Comments, substr(cntnt[i],2, nchar(cntnt[i])))
      }
    }
  }

  ZahlPattern = "[0-9]+"
  atomicVectorInd=regexpr(ZahlPattern, cntnt[nrOfRowsRow])
  StartRow = atomicVectorInd
  EndRow = StartRow+attributes(atomicVectorInd)$match.length-1 #
  rows=as.numeric(substr(cntnt[nrOfRowsRow],StartRow,EndRow)) # wie viele datenzeilen gibt es
},
error = function(c){
  warning(c)
  stop("Header or Comments are not reasonably defined, see Subversion/PUB/ZFileFormatDocuments for further instructions")
}
)


content=readLines(FileName, warn = F)
x = content
contentTabs = stringr::str_locate_all(x[(nrOfRowsRow+1):length(x)],"\\t")
countTabs = sapply(contentTabs, nrow)
expectedCols = countTabs[1]

unexpectedCols = 4
for(unexpectedCols in setdiff(unique(countTabs), expectedCols)){
  warning(paste0("The following Lines have ", unexpectedCols, " Columns, instead of the ", expectedCols, " expected Columns from the first datarow: ",
  paste(which(countTabs == unexpectedCols) + nrOfRowsRow, collapse = ", ")), "\n The difference will be cut off or filled up with NaNs. This may lead to mismatched columns in the respective rows!")
}



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
                    comment.char = "",   #turn off the interpretation of comments altogether. '#' nun auch in Daten
                    header=FALSE,         #Wenn die erste Zeile der Header ist. zB. bei csv
                    fill=T, 
                    flush = T,
                    stringsAsFactors = FALSE, 
                    na.strings=c(''),
                    skip=nrOfRowsRow)#,    #Die ersten HEADERSIZE zeilen werden nicht nochmal gelesen
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
  result=list(Key=Key,Names=Names,FurtherTexts=as.matrix(FurtherTexts),Comments=Comments)
}else
  result=list(Key=Key,Names=Names,Comments=Comments)

  return (result)
}
