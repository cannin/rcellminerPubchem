#' Get PubChem SID/CID value from an NSC using PubChem PUG API
#'
#' @param id an NSC ID 
#' @param type "cid" or "sid" PubChem ID types
#' @param debug a boolean, print debug information? (Default: TRUE)
#' @param cachePath path for cache files
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
    url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/DTP.NCI/", id, "/cids/TXT")		
  } else if(type == "sid") {
    url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/DTP.NCI/", id, "/sids/TXT")				
  } else {
    stop("ERROR: Unknown type argument")
  }
  
  if(debug) {		
    cat("URL: ", url, "\n")
  }	
  
  # content() can conflict with BioBase::content()
  Sys.setenv("PREFIX_SIMPLERCACHE"=paste0("getPubchemFromNsc_", id))
  
  #results <- suppressMessages(url %>% GETCached() %>% content("text"))
  
  key <- list(fcn=deparse(GET), url=url)
  callHash <- digest(key)
  
  # Set sleep based on whether the call will be retrieved from cache or executed
  if(length(dir(cacheDir, pattern=callHash)) == 0) {
    sleep <- ceiling(runif(1, 0, 3))    
  } else {
    sleep <- 0
  }

  tmpHash <- capture.output({ tmp <- url %>% GETCached(url=.) %>% content("text") })
  resultsHash <- tmpHash %>% trimws %>% sub("keyHash:  ", "", .)
  results <- paste(tmp, collapse=" ")
  
  cat("sleep: ", sleep, " callHash: ", callHash, " resultsHash: ", resultsHash, " Results: ", results, "\n")
  
  Sys.setenv("SLEEP_SIMPLERCACHE"=as.character(sleep))
  
  if(grepl('Status: 503', results)) {
    badCacheFile <- dir(cacheDir, pattern=resultsHash)
    file.remove(file.path(cacheDir, badCacheFile))
    
    cat("ERROR: BAD REQUEST: Cache file removed\n")
  } 
  
  if(grepl('Status: 404', results)) {
    Sys.setenv("SLEEP_SIMPLERCACHE"=as.character(0))
  }
  
  resultsVec <- strsplit(results, "\n")[[1]]

	if(debug) {		
		cat("Results: ", results, "\n")
		cat("Results Vec: ", resultsVec, "\n")
	}	

	if(length(resultsVec) == 1 && !startsWith(resultsVec, "Status")) {
		cid <- resultsVec
	} else {
	  cid <- NA
	}
		
  return(cid) 
}
