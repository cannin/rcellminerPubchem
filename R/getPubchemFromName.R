#' Get PubChem CID value from a name using PubChem PUG API
#'
#' @param id a name
#' @param forceFirst a boolean, if multiple entries are returned grab the first? 
#' @param debug a boolean, print debug information? (Default: TRUE)
#' @param cachePath path for cache files
#' 
#' @return a PubChem CID or NA, otherwise
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getPubchemFromNsc("trichostatin a") #Should be: 444732
#' getPubchemFromNsc("aspirin") #Should be: 2244
#' 
#' @importFrom slugify slugify
#' @importFrom simpleRCache addMemoization setCacheRootPath
#' @importFrom digest digest
#' @importFrom httr GET
#' 
#' @concept rcellminerPubchem
#' @export  
getPubchemFromName <- function(id, forceFirst=FALSE, debug=TRUE, cachePath=NULL) {
  if(is.null(cachePath)) {
    setCacheRootPath()
  } else {
    setCacheRootPath(cachePath)
  }
  
  GETCached <- addMemoization(GET)
  
  if(debug) {
    cat("\nID: ", id, "\n")
  }

  tmpId <- URLencode(id)
  url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", tmpId, "/cids/TXT")	

  if(debug) {		
    cat("URL: ", url, "\n")
  }	
  
  # content() can conflict with BioBase::content()
  slug <- slugify(id)
  Sys.setenv("PREFIX_SIMPLERCACHE"=paste0("getPubchemFromName_", slug))
  
  #results <- suppressMessages(url %>% GETCached() %>% content("text"))
  
  key <- list(fcn=deparse(GET), url=url)
  callHash <- digest(key)
  
  # Set sleep based on whether the call will be retrieved from cache or executed
  if(length(dir(cacheDir, pattern=callHash)) == 0) {
    sleep <- ceiling(runif(1, 0, 3))    
  } else {
    sleep <- 0
  }

  tmpHash <- capture.output({ tmp <- url %>% GETCached(url=.) %>% content("text", encoding="UTF-8") })
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
	} else if(forceFirst && !startsWith(resultsVec, "Status")) {
	  cid <- resultsVec[1]
	} else {
	  cid <- NA
	}
		
  return(cid) 
}
