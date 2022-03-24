#' Get PubChem SID/CID value from an ChEMBL ID using PubChem PUG API
#'
#' @param id an ChEMBL ID 
#' @param type "cid" or "sid" PubChem ID types
#' @param debug a boolean, print debug information? (Default: TRUE)
#' @param maxTries number of times to re-try query
#' 
#' @return a PubChem CID/SID or NA, otherwise
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getPubchemFromChembl("CHEMBL192", "cid") #Should be: 135398744
#' 
#' @concept rcellminerPubchem
#' @export  
getPubchemFromChembl <- function(id, type=c("cid", "sid"), debug=TRUE, maxTries=3) {
  type <- match.arg(type)
  
  cid <- NULL
  
  if(debug) {
    cat("ID: ", id, "\n")
  }
  
  if(type == "cid") {
    url <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/ChEMBL/", id, "/cids/TXT", sep="")		
  } else {
    url <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/ChEMBL/", id, "/sids/TXT", sep="")				
  }
  
  if(debug) {		
    cat("URL: ", url, "\n")
  }	
  
  numTries <- 0
  repeat{
    numTries <- numTries+1
    results <- try(getURL(url), silent=TRUE)
    downloadProblem <- inherits(results, "try-error")
    if(!downloadProblem | numTries==maxTries)
      break
    Sys.sleep(runif(1, 5, 10))#Sleep for a few seconds and try again
    cat("Try again", numTries+1, "\n")
  }
  if(numTries==maxTries & downloadProblem){
    cat("Could not get \"", url, "\" after ", maxTries, " tries\n", sep="")
    browser()
  }
  
  
  results_vec <- strsplit(results, "\n")[[1]]
  
  if(debug) {		
    cat("Results: ", results, "\n")
    cat("Results_vec: ", results_vec, "\n")
  }	
  
  if(length(results_vec) >= 1 && is.na(str_match(results_vec[1], "Status"))) {
    cid <- c(cid, results_vec[1])			
  } else {
    cid <- c(cid, NA)
  }
  
  if(is.null(cid)) {
    return(NA)
  } else {
    return(cid) 
  }
}
