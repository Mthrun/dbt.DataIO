TrimHeader = function(Header,UnderlineReplacement = " "){
# Header = TrimHeader(Header)
# deblank Header replace _ by " " and remove  
# 
# INPUT
# Header               a string array
#
# OPTIONAL
# UnderlineReplacement replaces "_" in Header default:UnderlineReplacement =' '; 
#
# OUTPUT
# Header               the trimmed Header
  
  Header = deblank(Header)
  Header = gsub("_", UnderlineReplacement, Header)
  return(Header)
}


