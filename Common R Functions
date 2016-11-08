getLoad <- function(x, table, column, loadName){
 # browser()
  ind <- toupper(as.character(x[1]))
  
  colRef <- which(colnames(table) == loadName)
  rowRef <- match(ind,toupper(table[,column]))
  
  if(is.na(colRef) | is.na(rowRef)){
    return(0)
  }else {
    return(table[rowRef, colRef])
  }
}

getAreaCode <- function(x){
  secondDigit <- substring(x[1], 2,2)
  
  if(is.na(as.numeric(secondDigit))){
    return(substring(x[1],1,2))
  }else{
    return(substring(x[1],1,1))
  }
  
}


getPostcodeSector <- function(x){
  sections <- strsplit(x," ")[[1]]
  return(paste(sections[1],substring(sections[2],1,1), sep = " "))
}





getPostcodeDistrict <- function(x){
  sections <- strsplit(x," ")[[1]]
  return(paste(sections[1], sep = " "))
}
