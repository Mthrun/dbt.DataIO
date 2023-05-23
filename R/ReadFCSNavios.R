ReadFCSNavios = function(FileName, FilePath = "", VarsToCompensate = NULL,
                         tryToCompensate = TRUE,Anonymize = FALSE,Silent=FALSE){
# V = ReadFCSNavios(FileName, FilePath)
# Read lmd files containing fcs files
# 
# INPUT
# FileName             name of the file.
# FilePath             name of the file's directory
# VarsToCompensate     Vector of TRUE and FALSE for each variable in the data.
#                      If TRUE, the variable will be compensated.
#                      If not given, the function will try to find the right
#                      variables itself. Boolean Vector the length of number
#                      of variables.
# tryToCompensate      tries to compensate the PlainData if a compensation
#                      matrix can be found.
# Anonymize            tries(!) to remove common keywords containing
#                      information about patients.
#                      NOTE: it cannot be assured that all the data is anonymous
# OUTPUT
# V$FCSData                     object of class flowFrame.
#                               relates to flowCore::read.FCS.
# V$PlainData[1:n,1:d]          Data in a plain matrix format. This is the
#                               data as it was found in the lmd/fcs file which
#                               might already be compensated.
# V$CompensationMatrix[1:d2,1:d2] Compensation Matrix if it exists
# V$CompensatedData[1:n,1:d]    Matrix with compensated data.
# V$VarNames[1:d]                variable names (e.g. "FS_PEAK_LIN", "FS",
#                               "SS_PEAK_LIN", "SS", "TCRgd-FITC", "CD45-KrO")
# V$VarIdentifiers[1:d]         detector names used by the machine to adress
#                               the variable) (e.g. "FS-H", "FS-A", "SS-H",
#                               "SS-A","FL1-A", "FL1-H", "FL2-A", "FL2-H",...,
#                               "TIME")
# V$DeviceName                    Flowcytometer device name.
  
# 
# Temporarily REMOVED INPUT  
# Anonymize if T, then parts of FCS Data get overwritten by placeholders to
# remove links to patients
  
#FileName  = addext(FileName,'LMD');     #  checks for the right extension and adds it if necessary
  
  
  if(nchar(FilePath) > 0){
    if(substr(FilePath, nchar(FilePath), nchar(FilePath)) != '/')
      FilePath = paste0(FilePath, '/')
  }

  frame1 = flowCore::read.FCS(paste0(FilePath, FileName), dataset = 1)
  frame2 = flowCore::read.FCS(paste0(FilePath, FileName), dataset = 2)

  if(is.null(frame1)){
    print("Dataset 1 (compensated data) could not be read")
    return(list(FCSData = NULL,
                PlainData = NULL,
                CompensationMatrix = NULL,
                CompensatedData = NULL, 
                VarNames = NULL,
                VarIdentifiers = NULL,
                DeviceName = NULL))
  }
  
  if(is.null(frame2)){
    print("Dataset 2 (raw data) could not be read")
    return(list(FCSData = NULL,
                PlainData = NULL,
                CompensationMatrix = NULL,
                CompensatedData = NULL, 
                VarNames = NULL,
                VarIdentifiers = NULL,
                DeviceName = NULL))
  }
  DeviceName = frame1@description$`$CYT`
  data = as.matrix(frame2@exprs)

  # match addresses with description
  AddressToDescr = sapply(1:ncol(as.matrix(frame1@exprs)), function(p) c(frame1@description[[paste0("@P", p, "ADDRESS")]],
                                                                         frame1@description[[paste0("$P", p, "S")]]))
  # get descriptions for variables
  varDescr = sapply(1:ncol(data), function(p) AddressToDescr[2,AddressToDescr[1,] == frame2@description[[paste0("@P", p, "ADDRESS")]]])
  fixedVarDescr = gsub("_INT_LIN", "",gsub("/", "v", gsub(" ", "_", varDescr)))
  fixedVarDescr=gsub("CD_","CD",fixedVarDescr)
  
  if(!is.null(VarsToCompensate)){
    if(ncol(data)!=length(VarsToCompensate)){
      warning("ReadFCS: The number of Variables does not match the length of the VarsToCompensate Vector. The
              Data will therefore not be compensated. Please check the variable names in the output of this function.")
      tryToCompensate = F
    }
  }
  
  # load spillover matrix
  spill = frame2@description$'$SPILLOVER'

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
      # ForceSpill           Boolean: If TRUE: adds some epsilon noise to spill matrix if it is not invertible to compensation matrix
      #                               Compensation values outside of 1%-99% quantile will be set to zero
      # spill=spill+runif(length(spill),min = -0.0000001,max=0.0000001)
      # compensation = solve(spill)
      # minax=quantile(compensation)
      # compensation[compensation<minax[1]]=0
      # compensation[compensation>compensation[2]]=0
      # compensation=round(compensation,3)
    }
  }#end is not null spill

  # search for names
  varNames = sapply(1:ncol(data), function(i) ifelse(is.null((a = frame2@description[paste0("$P",i, "N")][[1]])), NA, a))

  # fix naming notation
 
  colnames(data) = fixedVarDescr

  CompensatedData = NULL
  if(is.null(spill)){
    print("Spillovermatrix missing: No compensation can be performed.")
    return(list(FCSData = NULL,
                PlainData = data,
                CompensationMatrix = NULL,
                CompensatedData = NULL, 
                VarNames = fixedVarDescr,
                VarIdentifiers = varNames,
                DeviceName = DeviceName))
  }else{
    if(tryToCompensate){
      if(is.null(VarsToCompensate)){
        # find Values to compensate
        VarsToCompensate = grepl("FL[[:digit:]]", varNames)
      }
      if(isFALSE(Silent)){
        print(paste("The following variables WILL be compensated: ",
                    paste(fixedVarDescr[VarsToCompensate], collapse = ", ")))
        print(paste("The following variables WILL NOT be compensated: ",
                    paste(fixedVarDescr[!VarsToCompensate], collapse = ", ")))
      }
      colnames(spill) = fixedVarDescr[VarsToCompensate]
      rownames(spill) = fixedVarDescr[VarsToCompensate]
      colnames(compensation) = fixedVarDescr[VarsToCompensate]
      
      applyCompensationMatrix = function(Data, Compensation, Columns){
        DataToCompensate = Data[,Columns]
        CompensatedData = DataToCompensate %*% Compensation
        Data[,Columns] = CompensatedData
        return(Data)
      }
      CompensatedData = applyCompensationMatrix(data, compensation, VarsToCompensate)
    }
  }
  

  if(Anonymize){
    frame@description$"$FIL"         = "John Doe"
    frame@description$"@SAMPLEID1"   = "John Doe"
    frame@description$"$RUNNUMBER"   = "Muster Strasse"
    frame@description$"@LOCATION"    = "Muster Strasse"
    frame@description$"$INSTADDRESS" = "Muster Strasse"
    frame@description$"$GUID"        = "Muster GUID"
    frame@description$"$FILENAME"    = "Muster Filename"
  }
  

  return(list(FCSData = frame1,
              PlainData = data,
              CompensationMatrix = compensation,
              CompensatedData = CompensatedData, 
              VarNames = fixedVarDescr,
              VarIdentifiers = varNames,
              DeviceName = DeviceName))
}