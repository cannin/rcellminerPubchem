#' Get Smiles from CASRN using PubChem PUG API
#'
#' @param nsc an NSC ID 
#' 
#' @return a PubChem CID/SID or NA, otherwise
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getSmilesFromCasrn("119413-54-6")
#' 
#' @concept rcellminerPubchem
#' @export  
getSmilesFromCasrn <- function(casrn, debug=TRUE) {
  warning("USE WITH CAUTION")
  
	smiles <- NULL
	
	if(debug) {
		cat("CasRN: ", casrn, "\n")
	}

	url <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/xref/RN/", casrn, "/property/CanonicalSMILES/TXT", sep="")

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
		smiles <- c(smiles, results_vec[1])			
	} else {
		smiles <- c(smiles, NA)
	}
		
	if(is.null(smiles)) {
		return(NA)
	} else {
		return(smiles) 
	}
}


