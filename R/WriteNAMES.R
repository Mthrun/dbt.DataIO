WriteNAMES = function(FileName, Names,  Key, FurtherTexts,Header, OutDirectory=getwd(),
                      Comments){   
  # WriteNAMES(FileName, Names)
  # WriteNAMES(FileName, Names, Key)
  # WriteNAMES(FileName, Names, Key, FurtherTexts, Header, OutDirectory,Comments)
  # Save names and eventual FurtherTexts to a *.names file.
  #
  # INPUT
  # FileName                  name of the  file to be written
  # Names[1:n]                vector with text without blanks to be put in each line
  #
  # OPTIONAL
  # OutDirectory              the OutDirectory where to write into; if not given: current dir.
  # Key[1:n]                  vector of row type: unique key for each line, by default: [1:n]' 
  # FurtherTexts[1:n,1:c]     string matrix with row FurtherTexts to be put in third column
  # Header[1:(c+1),]   optional   header for the Nmes and FurtherTexts: Array of chars, this line wil be insterted at top of file with leading #
  # Comments                  vector, Char Array or matrix, these lines will be insteted at top of file with leading #
  #                           Note: Also allowed: Comments='first line \n# second line'
  # 
  # Author: MT 04/2014
  # Edit: QS 2022 => code style + optional way of writing out WriteNAMES -> default: ALU, New and Nondefault: MCT
  # Edit: MCT 2023 -> Added conventional header to ReadNAMES, neuer Fehlerabfang bzgl Key  
  #----------------------------------------------------------------------------#
  ## Kontrolle des Inputflows 
  #----------------------------------------------------------------------------#
  
  if(is.null(FileName)){
    stop('WriteNAMES: FileName ist missing')
  }
  checkFilename(FileName, Directory = OutDirectory,        # Check FileName if correct extension, its existing, ...
                Extension = 'names', ReadOrWrite = FALSE,
                NameOfFunctionCalled='WriteNAMES()')
  
  if(!is.null(Names)){      # Pruefung welche Inputvariablen angegeben wurden
    rows=length(Names)
    Data = data.frame(Names)   
  }else{
    stop('WriteNAMES: Names ist missing')
  }
  
  #----------------------------------------------------------------------------#
  ## Abfang Fehler bezueglich Key
  #----------------------------------------------------------------------------#
  if(missing(Key)|is.null(Key)){#MT: Korrektur zur Keyerstellung
    Key = 1:rows
    warning('WriteNAMES: Key missing, generating Key as 1:n.')
  }else{#key set by user
      if(length(Key) != rows){
        stop('WriteNAMES():Length of Key isnt equal to the length of rows')
      }
  }
  
  if(missing(FurtherTexts)) FurtherTexts=NULL
  
  if(missing(Header)){
      Header=NULL #wird weiter unten generiert
  }else{
    # remove all Blanks from header
    Header = gsub('  ','_',Header)
    Header = sub(' ', '_', Header)
    Header = gsub('\n','_',Header)
    Header = gsub('\t','_',Header)
  }       


  if(!is.null(FurtherTexts)){ 
    if(!is.character(FurtherTexts)) stop("FurtherTexts must consist of Strings!")
    #MT: Einfache Anfuerungsstriche sind ein sonderzeichen, welches eine korrekte
    #    checksummen erstellen und ueberpruefung verhindert
    FurtherTexts = sub("'","",sub("'","",FurtherTexts))
    Data = cbind(Data,FurtherTexts)
    if(is.null(Header)){
      if(!is.null(colnames(FurtherTexts))){
        Header=c("Names",colnames(FurtherTexts))
      }else{#=NULL wird weiter unten abgefangen
        Header=Header=c("Names",paste0("C",1:ncol(FurtherTexts)))
      }
    }
  }else{
    if(is.null(Header)) Header="Names"
  }
  if(ncol(Data)<length(Header)){
    Header=Header[1:ncol(Data)]
    warning("WriteNAMES(): Header is longer than names plus furthertext (if exist) columns. Taking first elements.")
  }
  if(ncol(Data)>length(Header)){
    diff=ncol(Data)-length(Header)
    Header=c(Header,paste0("C",1:diff))
    warning("WriteNAMES(): Header is shorter than names plus furthertext (if exist) columns. Adding elements.")
  }
  
  if(ncol(Data) == 0 || nrow(Data) == 0 || typeof(Data) == 'NULL'){    # Zeilen anzahl bestimmen
    stop('WriteNAMES(): No Data to write') #stop wenn Data NULL ist
  }else{
    rows    = nrow(Data)
    columns = ncol(Data)
  }
  
  FileName  = addext(FileName,'names')    # richtige Datei endung
  suppressWarnings((OutDirectory = normalizePath(OutDirectory))) 
  currentWD = getwd()                     # Aktuelles verzeichnis merken
  setwd(OutDirectory)                     # Ins Verzeichnis wo sich Datei befindet wechseln


  if(!is.null(Key)){
    Keytmp = unique(Key)
    if(length(Keytmp)!=length(Key)){
      warning(paste0('WriteNAMES(): Key with length ',length(Key),' is not unique: ',length(Keytmp)))
    }else{
      Key=Keytmp
    }
  } #make key unique
  
  
  Data = data.frame(Key,Data)    #Key hinzufuegen zu ColumnNames 
    # ColumnNames = union(KeyName,ColumnNames)
  columns = columns + 1 #Wenn key Hinzugefuegt wurde, muss auch die Spalten Anzahl angepasst werden

  #ColumnNames zu Data
  #colnames(Data)=ColumnNames
  #----------------------------------------------------------------------------#
  # letzter Schritt ist die Klassen mit class() anzupassen:
  # Problem war: das entweder nur string oder nur Zahl gefordert war
  # R aber keine explizite Typen definition vorraus setzt. 
  #----------------------------------------------------------------------------#
  for(i in 1:columns){
    if(is.integer(Data[,i])){
      Data[,i]= as.numeric(Data[,i])
    }
    if(is.factor(Data[,i])){
      Data[,i]=as.character(Data[,i])
    }
    #else class(Data[,i])=='character' oder 'numeric' => bleibt
  }
  #----------------------------------------------------------------------------#
  ##---------------------Beginn mit schreiben in Datei--------------------------
  #----------------------------------------------------------------------------#
  #Anzahl Spalten und Zeilen schreiben
  #Size=c( paste0('%\t',rows) ) 
  
  # if(isTRUE(ALU)){                                                                 # ALUs way of WriteNAMES
  #   if(missing(Comments)){
  #     if(!missing(DescriptionHeader)){
  #       cat('#\t', file=FileName, append=TRUE)
  #       cat(DescriptionHeader,'\n',file=FileName, append=TRUE, sep='\t')
  #     }
  #     write.table(Size, file=FileName,quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
  #   }else{
  #     write.table(paste0('# ',Comments), file=FileName, quote=FALSE, sep='\t',row.names=FALSE, col.names=FALSE, na='NaN')
  #     if(!missing(DescriptionHeader)){
  #       cat('#', file=FileName, append=TRUE)
  #       cat(DescriptionHeader,'\n',file=FileName, append=TRUE, sep='\t')
  #     }
  #     write.table(Size, file=FileName,append=TRUE,quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
  #   }
  #   #Daten schreiben
  #   write.table(Data, file=FileName, append=TRUE, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
  # }else{                                                                           # MCT way of WriteNAMES
    if(!missing(Comments)){                                                        # Write comments if existing (1st row)
      Comments = paste0('# ',Comments)                                             # One row starting with '#'
      write.table(x = Comments, file = FileName, quote = FALSE, sep='\t',
                  row.names = FALSE, col.names = FALSE, na = 'NaN',append=FALSE)
      furtherappend=TRUE
    }else{
      furtherappend=FALSE
    }
  
    # if(!missing(DescriptionHeader)){                                               # Write header if existing (2nd row)
    #   DescriptionHeader = paste0('# ', paste0(DescriptionHeader, collapse = " "))  # One row starting with '#'
    #   #cat(DescriptionHeader,'\n',file=FileName, append=TRUE, sep='\t')
    #   write.table(x = DescriptionHeader, file = FileName, append = TRUE, quote = FALSE, sep='\t',
    #               row.names = FALSE, col.names = FALSE, na = 'NaN')
    # }
    # Datengroesse schreiben
      cols=ncol(Data)
      # write dimensions Number of lines & columns
      #header = c(paste('%\t',rows),paste('%\t',cols))
      #alfred moechte doch keine cols
      header = c(paste('%\t',rows))
      write.table(header, FileName, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN',append = furtherappend)
  
    # write.table(Size, file = FileName, append = furtherappend, quote = FALSE, sep='\t',     # Write size (3rd row)
    #             row.names = FALSE, col.names = FALSE, na = 'NaN')
    
    cat('% ', file=FileName, append=TRUE)
    cat(c("Key",Header),'\n', file=FileName, append=TRUE, sep='\t')
    
    # Daten schreiben
    write.table(Data, file = FileName, append = TRUE, quote = FALSE, sep='\t',
                row.names = FALSE, col.names = FALSE, na = 'NaN')
  #}
  
  ##------------Daten schreiben abgeschlossen-----------  
  #zurueck wechseln ins alte Verzeichnis
  setwd(currentWD)
  #  }
}
