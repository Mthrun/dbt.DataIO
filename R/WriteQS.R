WriteQS = function(FileName, Data, Names = NULL,OutDirectory = getwd(), NThreads = 1,
                   Preset = "high",
                   Algorithm = "zstd", Compress_level = 4L,
                   Shuffle_control = 15L, Check_hash = TRUE
                   ){
  # DESCRIPTION
  # Compresses data to serialized format in parallelized manner for fast and
  # storage efficient data input/output
  # 
  # INPUT
  # FileName          Character with single file name
  # Data
  #     [1:n, 1:m]    Numeric matrix
  #     [1:l]         List of l numeric matrices of dimensions [1:n, 1:m]
  # 
  # OPTIONAL
  # Names[1:l]        Character vector with
  # Preset            One of "fast", "balanced", "high" (default), "archive",
  #                   "uncompressed" or "custom". See section Presets for details.
  # Algorithm         Ignored unless preset = "custom". Compression algorithm
  #                   used: "lz4", "zstd", "lz4hc", "zstd_stream" or
  #                   "uncompressed".
  # Compress_level    Character name. Ignored unless preset = "custom". The 
  #                   compression level used.
  #                   For lz4, this number must be > 1 (higher is less compressed).
  #                   For zstd, a number between -50 to 22 (higher is more
  #                   compressed). Due to the format of qs, there is very little
  #                   benefit to compression levels > 5 or so.
  # Shuffle_control   Integer. Ignored unless preset = "custom". An integer 
  #                   setting the use of byte shuffle compression. A value 
  #                   between 0 and 15 (default 15). See section Byte shuffling
  #                   for details.
  # Check_hash        Boolean, Default TRUE, compute a hash which can be used to
  #                   verify file integrity during serialization.
  # NThreads          Integer defining the number of threads used for parallel
  #                   computations
  # OutDirectory      Character with directory path
  #                   (Default: OutDirectory=getwd())
  # 
  # OUTPUT
  # -
  # 
  # Author: QMS 2022
  
  # Save current directory to return back at the end of process
  CurrentDir = getwd()
  FileName=addext(FileName,'qs')
  #----------------------------------------------------------------------------#
  # Input: Error capturing
  
  # Check FileName (see dbt.DataIO functionality)
  dbt.DataIO::checkFilename(FileName = FileName, Directory = OutDirectory,
                            Extension = "qs", ReadOrWrite = FALSE,
                            NameOfFunctionCalled = "WriteQS()")
  
  
  # Ensure Data is numeric matrix or list of numeric matrices
  if((!is.matrix(Data)) & (!is.list(Data))){
    stop("Parameter Data must be a matrix or a list of matrices")
  }
  
  # Data is matrix: capture errors
  if(is.matrix(Data)){
    if(!is.numeric(Data)){
      stop("Parameter Data must be a numeric matrix")
    }
  }
  
  # Data is list: capture errors (consider parameter Names)
  if(is.list(Data)){
    for(i in 1:length(Data)){
      if(!is.numeric(Data[[i]])){
        warning("Parameter Data should be a list of numeric matrices")
        break;
      }
    }
    # Ensure Names contains unique names only and matches length of list Data
    if(is.null(Names)){
      if(is.null(names(Data)))
        Names = paste0(1:length(Data))
      else
        Names = names(Data)
    }
    if(length(Names) != length(Data)){
      stop("Parameter Names must have the same length as the list for parameter Data")
    }
    if(length(Names) != length(unique(Names))){
      stop("Parameter Names must be a unique list of names")
    }
    names(Data) = Names
  }
  
  #----------------------------------------------------------------------------#
  # Write (main functionality of method)
  
  # Change to destination
  setwd(OutDirectory)
  
  #if(file.exists(FileName)){ # Delete file if it exists
  #  file.remove(FileName)
  #}
  
  # See qs functionality
  qs::qsave(x = Data, file = FileName, preset = Preset, algorithm = Algorithm,
            compress_level = Compress_level, shuffle_control = Shuffle_control,
            check_hash = Check_hash, nthreads = NThreads)
  
  # Return to original directory
  setwd(CurrentDir)
  #----------------------------------------------------------------------------#
  # THE END
}
