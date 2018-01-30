#' Get CasRN value from an NSC using PubChem PUG API
#'
#' @param nsc an NSC ID 
#' @param debug a boolean, show debugging information (default: TRUE)
#' @return a CasRN ID or NA, otherwise
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getCasrnFromNsc("733625")
#' 
#' @concept rcellminerPubchem
#' @export
getCasrnFromNsc <- function(nsc, debug=TRUE) {
	require(RCurl)
	require(stringr) 
	
	casrn <- NULL
	
	if(debug) {
		cat("NSC: ", nsc, "\n")
	}

	url <- paste("http://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/DTP.NCI/", nsc, "/xrefs/RN/TXT", sep="")

	if(debug) {		
		cat("URL: ", url, "\n")
	}	

	results <- getURL(url)
	results_vec <- strsplit(results, "\n")[[1]]

	if(debug) {		
		cat("Results: ", results, "\n")
		cat("Results_vec: ", results_vec, "\n")
	}	

	if(length(results_vec) >= 1 && is.na(str_match(results_vec[1], "Status"))) {
		casrn <- c(casrn, results_vec[1])			
	} else {
		casrn <- c(casrn, NA)
	}
		
	if(is.null(casrn)) {
		return(NA)
	} else {
		return(casrn) 
	}
}
