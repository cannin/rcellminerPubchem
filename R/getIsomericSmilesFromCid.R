#' Get IsomericSmiles from PubChem CID using PubChem PUG API
#'
#' @param id a PubChem CID
#' 
#' @return a SMILES or NA, otherwise
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getIsomericSmilesFromCid("1030")
#' 
#' @concept rcellminerPubchem
#' @export  
getIsomericSmilesFromCid <- function(id, debug=TRUE) {
	smiles <- NULL
	
	if(debug) {
		cat("ID: ", id, "\n")
	}

	url <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", id, "/property/IsomericSMILES/TXT", sep="")

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


