#' Get PubChem XML for a Given Name using PubChem PUG API
#'
#' @param name a name
#' @param debug a boolean, show debugging information (default: TRUE)
#' @return the name used by PubChem
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getPubchemXmlFromName("vemurafenib")
#' 
#' @concept rcellminerPubchem
#' @export
getPubchemXmlFromName <- function(id, debug=TRUE, cachePath=NULL) {
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

	tmpId <- URLencode(id)
	url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", tmpId, "/cids/TXT")	
	
	if(debug) {		
	  cat("URL: ", url, "\n")
	}	
	
	results <- suppressMessages(url %>% GETCached() %>% content("text"))
	results_vec <- strsplit(results, "\n")[[1]]
	
	if(debug) {		
	  cat("Results: ", results, "\n")
	  cat("Results_vec: ", results_vec, "\n")
	}	
		
	
	url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/", results_vec, "/XML/")
	
	if(debug) {		
		cat("URL: ", url, "\n")
	}	

	result <- suppressMessages(url %>% GETCached() %>% content("text"))

	if(is.null(result)) {
		return(NA)
	} else {
		return(result) 
	}
}
