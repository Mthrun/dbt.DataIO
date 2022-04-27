ReadFCSInfo = function(FileName, FilePath = ""){
  # V = ReadFCSInfo(FileName, FilePath)
  # INPUT
  # FileName
  # FilePath
  # OUTPUT
  # ParamaeterDescription     String Matrix containing Name, Description and Scale of the dataset. In case of multiple datasets in the
  #                               same file, a list of matrices is returned.
  FileName  = addext(FileName,'fcs');     #  checks for the right extension and adds it if necessary
  
  if(nchar(FilePath) > 0){
    if(substr(FilePath, nchar(FilePath), nchar(FilePath)) != '/')
      FilePath = paste0(FilePath, '/')
  }
  
  readOutDescription = function(frame){
    V = frame
    # count number of parameters
    for(nrOfPar in 1:1000){
      p = nrOfPar
      if(is.null(V@description[paste0("$P",p, "N")][[1]]) &
         is.null(V@description[paste0("$P",p, "S")][[1]]) &
         is.null(V@description[paste0("@P",p, "C")][[1]])){
        nrOfPar = nrOfPar - 1
        break
      }
    }
    
    res = matrix(ncol = 3, nrow = nrOfPar)
    colnames(res) = c('Name', 'Description', 'Scale')
    
    for(p in 1:nrOfPar){
        
      v1 = V@description[paste0("$P",p, "N")][[1]]
      v2 = V@description[paste0("$P",p, "S")][[1]]
      v3 = V@description[paste0("@P",p, "C")][[1]]
      
      res[p,] = c(ifelse(is.null(v1), 'NA', v1),
                  ifelse(is.null(v2), 'NA', v2),
                  ifelse(is.null(v3), 'NA', v3))
      
    }
    return(res)
  }
  
  useDataset = 1
  
  frame = flowCore::read.FCS(paste0(FilePath, FileName), dataset = useDataset)
  
  if(frame@description$`$NEXTDATA` != 0){
    print("ReadFCSInfo: There is more than one dataset found in file. Result will be a list!")
    results = list(readOutDescription(frame))
  } else{
    results = readOutDescription(frame)
  }
  
  while(frame@description$`$NEXTDATA` != 0){
    useDataset = useDataset + 1
    frame = flowCore::read.FCS(paste0(FilePath, FileName), dataset = useDataset)
    results = c(results, list( readOutDescription(frame)))
  }
  
  return(results)
}
