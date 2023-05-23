ReadFCS = function(FileName, FilePath = "", Dataset = NULL,
                   VarsToCompensate = NULL, tryToCompensate = T,
                   Anonymize = F){
  # # Read fcs files
  # V = ReadFCS(InFileName, InDirectory)
  # V = ReadFCS(FileName, FilePath, Dataset = 2)
  # Data       = as.matrix(V$PlainData)
  # Header     = V$VarNames ; Header
  # 
  # INPUT
  # FileName             name of the file.
  #
  # OPTIONAL
  # FilePath             name of the file's directory.
  # Dataset              in FCS files, multiple datasets can be saved. If
  #                      another than the last shall be loaded, this gives its
  #                      index
  # VarsToCompensate     Vector of TRUE and FALSE for each variable in the data.
  #                      If TRUE, the variable will be compensated.
  #                      If not given, the function will try to find the right
  #                      variables itself. Boolean Vector the length of number
  #                      of variables.
  # tryToCompensate      tries to compensate the PlainData if a compensation
  #                      matrix can be found
  # Anonymize            tries(!) to remove common keywords containing
  #                      information about patients.
  #                      THIS IN NO WAY MAKES DATA SAVE TO GIVE OUT!
  #
  # OUTPUT
  # v$FCSData                     object of class flowFrame.
  #                               relates to flowCore::read.FCS
  # v$PlainData[1:n,1:d]          Data in a plain matrix format. This is the data as
  #                               it was found in the lmd/fcs file which might
  #                               already be compensated
  # CompensationMatrix[1:d2,1:d2] Compensation Matrix if existing
  # CompensatedData[1:n,1:d]      Matrix with compensated data
  # VarNames[1:d]                 variable names (e.g. "FS_PEAK_LIN", "FS",
  #                               "SS_PEAK_LIN", "SS", "TCRgd-FITC", "CD45-KrO")
  # VarIdentifiers[1:d]           detector names used by the machine to adress
  #                               the variable) (e.g. "FS-H", "FS-A", "SS-H",
  #                               "SS-A","FL1-A", "FL1-H", "FL2-A", "FL2-H",...,
  #                               "TIME")
  #                               
  # DeviceName                    Flowcytometer device name.
  # Cls[1:n]                      Classes from Clustering
  # 
  # Temporarily REMOVED INPUT  
  # Anonymize if T, then parts of FCS Data get overwritten by placeholders to remove links to patients
  FileName  = addext(FileName,'fcs');     #  checks for the right extension and adds it if necessary
  
  if(nchar(FilePath) > 0){
    if(substr(FilePath, nchar(FilePath), nchar(FilePath)) != '/')
      FilePath = paste0(FilePath, '/')
  }
  
  useDataset = 1
  # wenn dataset gegeben, dann benutze immer diesen
  # sonst springe bis zum letzten enthaltenen Datensatz
  
  # move to the correct dataset
  if(!is.null(Dataset)){
    useDataset = Dataset
    frame = flowCore::read.FCS(paste0(FilePath, FileName), dataset = useDataset)
  }
  
  if(is.null(Dataset)){
    # read the first dataset
    useDataset = 1
    
    frame = flowCore::read.FCS(paste0(FilePath, FileName), dataset = useDataset)
    # load further datasets as long as they are available
 #   while(frame@description$`$NEXTDATA` != 0){
 #     useDataset = useDataset + 1
 #     frame = flowCore::read.FCS(paste0(FilePath, FileName), dataset = useDataset)
 #   }
  }
  
  data = as.matrix(frame@exprs)
  print(paste0("ReadFCS: Dataset ", useDataset, " was read."))
  

  
  # load spillover matrix
  spill = frame@description$'SPILL'
  if(is.null(spill)) spill = frame@description$'$SPILLOVER'
  if(is.null(spill)){
      if(!is.null(frame@description[["$DFC1TO1"]])){
        tryCatch({
          # try to search for an alternative method to define Compensation Matrix
          tempComp = matrix(nrow = ncol(data), ncol = ncol(data))
          
          for(from in 1:ncol(data)){ # 10 eintraege
            for(to in 1:ncol(data)){
              n = paste0("$DFC",from,"TO", to)
              
              a = frame@description[[n]]
              if(!is.null(a)) tempComp[from,to] = a
            }
          }
          
          firstInvalidRow = sort(which(is.na(tempComp[,1])))[1]
          spill = apply( tempComp[1:(firstInvalidRow-1), 1:(firstInvalidRow-1)], 1:2, as.numeric) / 100
      },
      error = function(e){
        tryToCompensate = F
        warning("ReadFCS: Compensation Matrix could not be extracted!")})
    }
    else{
      tryToCompensate = F
      warning("ReadFCS: No Compensation Matrix found!")
    }
  }
 
  # go from spillover to compensation matrix
  compensation = NULL
  if(!is.null(spill)){
    for(i in 1:ncol(spill)){
      spill[i,i] = 1
    }
    
    FLAG_invertible=FALSE
    tryCatch({
      compensation = solve(spill)
      FLAG_invertible=TRUE
    },error=function(e){
      warning(e)
   })
   if(isFALSE(FLAG_invertible)){
     warning("ReadFCSNavios: spill matrix is not invertible.Using the Moore-Penrose generalized inverse.")
     compensation=MASS::ginv(spill)
   #   spill=spill+runif(length(spill),min = -0.0000001,max=0.0000001)
   #   compensation = solve(spill)
   #   minax=quantile(compensation)
   #   compensation[compensation<minax[1]]=0
   #   compensation[compensation>compensation[2]]=0
   #   compensation=round(compensation,3)
   #   warning("ReadFCS: spill matrix is not invertible. Adding some epsilon random noise to make matrix invertible.")
    }
  }#end is not null spill
  
  # search for names and descriptions
  parameterDescriptions = sapply(1:ncol(data), function(i){
    c( ifelse(is.null((a = frame@description[paste0("$P",i, "N")][[1]])), NA, a),
       ifelse(is.null((a = frame@description[paste0("$P",i, "S")][[1]])), NA, a))
  })
  
  
  parameterIdentifiers = parameterDescriptions[1,]
  parameterNames = parameterDescriptions[2,]
  
  if(all(is.na(parameterNames))) parameterNames = parameterIdentifiers
  
  
  # fix naming notation
  fixedParameterNames = gsub("_INT_LIN", "",gsub("/", "v",
                            gsub(" ", "_", parameterNames)))
  fixedParameterNames=gsub("CD_","CD",fixedParameterNames)
  #fixedParameterNames[fixedParameterNames == "FS_FS"] = "FS"
  #fixedParameterNames[fixedParameterNames == "SS_SS"] = "SS"
  #fixedParameterNames[fixedParameterNames == "TIME_TIME"] = "TIME"
  
  colnames(data) = fixedParameterNames
  
  CompensatedData = NULL
  if(tryToCompensate){
    if(is.null(VarsToCompensate)){
      # method 1: get value names from spillover matrix
      try({
        CompID = sapply(colnames(spill), function(n)which(colnames(frame@exprs) == n))
        VarsToCompensate = rep(F, ncol(data))
        VarsToCompensate[CompID] = T
      })
      if(is.null(VarsToCompensate)){ # method 1 did not work
        # method 2: try to identify the variables by interpreting their names
        VarNames = colnames(frame@exprs)
        VarsToCompensate = grepl("FL[[:digit:]]", VarNames)
      }
      
      # fixedParameterNames   /  VarNames 
      print(paste("The following variables WILL be compensated: ",
                  paste(paste0(fixedParameterNames[VarsToCompensate], "(", parameterIdentifiers[VarsToCompensate], ")"),
                        collapse = ", ")))
      
      print(paste("The following variables WILL NOT be compensated: ",
                  paste(paste0(fixedParameterNames[!VarsToCompensate], "(", parameterIdentifiers[!VarsToCompensate], ")"),
                        collapse = ", ")))
      
      colnames(spill) = fixedParameterNames[VarsToCompensate]
      
      applyCompensationMatrix = function(Data, Compensation, Columns){
        DataToCompensate = Data[,Columns]
        CompensatedData = DataToCompensate %*% Compensation
        #CompensatedData = DataToCompensate - spilloverAmount
        Data[,Columns] = CompensatedData
        return(Data)
      }
      CompensatedData = applyCompensationMatrix(data, compensation, VarsToCompensate)
      
    }
    else{
      if(ncol(data)!=length(VarsToCompensate)){
        warning("ReadFCS: The number of Variables does not match the length of the VarsToCompensate Vector. The 
              Data will therefore not be compensated. Please check the variable names in the output of this function.")
        }
    }
  }
  if(Anonymize){
     frame@description$"$FIL" = "John Doe"
     frame@description$"@SAMPLEID1" = "John Doe"
     frame@description$"$RUNNUMBER" = "Muster Strasse"
     frame@description$"@LOCATION" = "Muster Strasse"
     frame@description$"$INSTADDRESS" = "Muster Strasse"
     frame@description$"$GUID" = "Muster GUID"
     frame@description$"$FILENAME" = "Muster Filename"
  }
  if(!is.null(frame@description$"$CLS")){
    cls = frame@description$"$CLS"
  }else{
    cls = NULL
  }
  if(!is.null(compensation)){
    colnames(compensation) = fixedParameterNames[VarsToCompensate]
  }else{
    print("Compensation is null.")
  }
  DeviceName = frame@description$`$CYT`
  return(list(FCSData = frame,
              PlainData = data,
              CompensationMatrix = compensation,
              CompensatedData = CompensatedData, 
              VarNames = fixedParameterNames,
              VarIdentifiers = parameterIdentifiers,
              DeviceName = DeviceName,
              Cls = cls))
}




