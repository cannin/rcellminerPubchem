#' Get PubChem SID/CID value from an NSC using PubChem PUG API
#'
#' @param id an NSC ID 
#' @param type "cid" or "sid" PubChem ID types
#' @param debug a boolean, print debug information? (Default: TRUE)
#' @param maxTries number of times to re-try query
#' 
#' @return a PubChem CID/SID or NA, otherwise
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getPubchemFromNsc("94600", "cid") #Should be: 24360
#' getPubchemFromNsc("94600", "sid") #Should be: 399733
#' 
#' @concept rcellminerPubchem
#' @export  
getPubchemFromNsc <- function(id, type="cid", debug=TRUE, cachePath=NULL) {
  result <- NULL
  
  if(is.null(cachePath)) {
    setCacheRootPath()
  } else {
    setCacheRootPath(cachePath)
  }
  
  GETCached <- addMemoization(GET)
  
  if(debug) {
    cat("ID: ", id, "\n")
  }
  
  if(type == "cid") {
    url <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/DTP.NCI/", id, "/cids/TXT", sep="")		
  } else if(type == "sid") {
    url <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/DTP.NCI/", id, "/sids/TXT", sep="")				
  } else {
    stop("ERROR: Unknown type argument")
  }
  
  if(debug) {		
    cat("URL: ", url, "\n")
  }	
  
  # content() can conflict with BioBase::content()
  results <- suppressMessages(url %>% GETCached() %>% content("text"))
  resultsVec <- strsplit(results, "\n")[[1]]

	if(debug) {		
		cat("Results: ", results, "\n")
		cat("Results Vec: ", resultsVec, "\n")
	}	

  if(length(resultsVec) > 1) {
    stop("ERROR: Multiple CIDs returned")
  }
  
	if(length(resultsVec) == 1 && is.na(str_match(resultsVec[1], "Status"))) {
		cid <- resultsVec[1]	
	}
		
  return(cid) 
}
