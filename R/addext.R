#' add file extensions to file names
#' 
#' \code{addext} intern \code{DataIO} function, checks for the right extension
#' and adds it if necessary.
#' 
#' Files have special formats and therefor need to be saved with the correct
#' extensions. \code{addext} is simply an auxiliary function to guarantee
#' correct file name extensions
#' 
#' @param filename string, to be checked for correct extension
#' @param extension Correct extension for filename.
#' @return Function returns a string containing the filename and the correct
#' extension.
#' @author Michael Thrun
#' @keywords file
#' @examples
#' 
#' addext('hallo.data','data') #  'hallo.data'
#' addext('hallo','data')     #  'hallo.data'
#' 
#' 
addext = function(filename,extension){
  TmpRes = unlist(strsplit(filename, "[.]"))
  if(is.null(TmpRes)){                   # No filename given
    stop("Filename is null.")
    return()
  }else{
    if(length(TmpRes) == 1){             # Adding extension required
      filename = paste(filename,'.',extension,sep="")
    }else if (length(TmpRes) > 2){       # Too many points in filename
      stop("Two many points in filename.")
      return()
    }else{
      if(TmpRes[2] != extension){        # Extension already existing which is not requested extension
        stop("Wrong extension.")
        return()
      }
      #else                              # Requested extension already existing
    }
  }
  return(filename)
}

