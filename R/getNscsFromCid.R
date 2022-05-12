#' Get NSCs from PubChem CID
#' 
#' @param cid a string with the PubChem CID
#' @param debug a boolean, print debug information? (Default: TRUE)
#' @return a vector of NSCs 
#' 
#' @examples 
#' results <- getNscsFromCid("5284373", FALSE)
#' 
#' @concept rcellminerPubchem
#' @export
getNscsFromCid <- function(cid, debug=TRUE) {
	nscs <- NULL 
	
	if(debug) {
		cat("CID: ", cid, "\n")
	}
	
	# Checks on CID
	if(is.na(cid)) {
	    return(NA)
	}
	
	url <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", cid, "/synonyms/txt", sep="")
	
	results <- getURL(url)
	results_vec <- strsplit(results, "\n")[[1]]

	if(debug) {		
		cat("URL: ", url, "\n")
	}
	
	for(i in 1:length(results_vec)) {
		if(debug) {
			cat("Result: ", results_vec[i], "\n")
		}
		
		if(length(grep("NSC", results_vec[i])) == 1) {
			if(debug) {
				cat("NSC: ", results_vec[i], "\n")
			}
			
			nsc <- str_match(results_vec[i], "\\d+")
			nscs <- c(nscs, nsc)
		} 
	}
	
	nscs <- unique(nscs)
	
	if(is.null(nscs)) {
		return(NA)
	} else {
		return(nscs) 
	}
}
