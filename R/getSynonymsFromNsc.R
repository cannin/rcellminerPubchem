#' Get PubChem Synonyms for a Given Name using PubChem PUG API
#'
#' @param name a name
#' @param debug a boolean, show debugging information (default: TRUE)
#' @return the name used by PubChem
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getSynonymsFromNSC("609699")
#' 
#' @concept rcellminerPubchem
#' @export
getSynonymsFromNSC <- function(id, debug=TRUE, maxItems=10, cachePath=NULL) {
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

	url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/DTP.NCI/", id, "/cids/TXT")
	
	if(debug) {		
	  cat("URL: ", url, "\n")
	}	
	
	results <- suppressMessages(url %>% GETCached() %>% content("text"))
	results_vec <- strsplit(results, "\n")[[1]]
	
	if(debug) {		
	  cat("Results: ", results, "\n")
	  cat("Results_vec: ", results_vec, "\n")
	}	
		
	url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", results_vec, "/synonyms/TXT")
	
	if(debug) {		
		cat("URL: ", url, "\n")
	}	

	results <- suppressMessages(url %>% GETCached() %>% content("text"))
	results_vec <- strsplit(results, "\n")[[1]]

	if(debug) {		
		cat("Results: ", results, "\n")
		cat("Results_vec: ", results_vec, "\n")
	}	

	# Check that there are results and that the first result is unlikely to be a HTTP issue
	if(length(results_vec) >= 1 && is.na(str_match(results_vec[1], "Status")) && !grepl("xml", results_vec[1])) {
	  if(length(results_vec) < maxItems) {
	    maxItems <- length(results_vec)
	  }
		result <- c(result, paste(results_vec[1:maxItems], collapse=";"))			
	} else {
	  result <- c(result, NA)
	}
		
	if(is.null(result)) {
		return(NA)
	} else {
		return(result) 
	}
}
