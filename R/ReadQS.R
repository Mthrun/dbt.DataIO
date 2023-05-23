ReadQS = function(FileName, InDirectory=getwd(), NThreads = 1, Strict = FALSE){
  # DESCRIPTION
  # Reads compressed data from serialized format in parallelized manner for fast
  # and storage efficient data input/output
  # 
  # INPUT
  # FileName       Character with single file name
  # 
  # OPTIONAL
  # InDirectory    Character with directory path (Default: InDirectory=getwd())
  # Strict         Boolean (Default: Strict = FALSE).
  #                    TRUE:  Throw error
  #                    FALSE: Report warning
  # NThreads       Integer defining the number of threads used for parallel
  #                computations. (Default: NThreads=1).
  # 
  # OUTPUT
  # QSFile    Numeric matrix or list of numeric matrices
  # 
  # Author: QMS 2022
  
  
  # Save current directory to return back at the end of process
  CurrentDir = getwd()
  
  #----------------------------------------------------------------------------#
  # Input: Error capturing
  FileName  = addext(FileName,'qs');     #  checks for the right extension and adds it if necessary
  
  
  #----------------------------------------------------------------------------#
  # Read (main functionality of method)
  
  # Change to destination
  setwd(InDirectory)
  
  # See qs functionality
  QSFile = qs::qread(file = FileName, strict = Strict, nthreads = NThreads)

  # Return to original directory
  setwd(CurrentDir)
  #----------------------------------------------------------------------------#
  # THE END
  return(QSFile)
}
