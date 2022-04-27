GetDeviceName <- function(flowFrame){
  # V = GetDeviceName(flowCoreFrame)
  # 
  # INPUT
  #
  # flowFrame    Object of flowFrame-class {flowCore}.
  # 
  # OUTPUT
  # 
  # DeviceName   String name of flowcytometer device.
  # 
  # Extracts the name of the flowcytometer device from flowcytometer data.
  # This data must be given as object of flowFrame-class {flowCore}.
  # This data can stem from a FCS or a LMD file.
  # Author: QS 2021
  DeviceName = flowFrame@description[["$CYT"]][[1]]
  return(list("DeviceName"=DeviceName))
}
