#' Get NSC value from an CASRN ID using PubChem PUG API
#'
#' @param nsc an NSC ID 
#' @param debug a boolean, show debugging information (default: TRUE)
#' 
#' @return a CasRN ID or NA, otherwise
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getCasrnFromNsc("733625")
#' 
#' @concept rcellminerPubchem
#' @export
getNscsFromCasrn <- function(casrn, debug=TRUE) {
	nscs <- NULL 
	
	if(debug) {
		cat("CasRN: ", casrn, "\n")
	}
	
	url <- paste("http://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/xref/RN/", casrn, "/xrefs/SBURL/TXT", sep="")
	
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
	
	if(is.null(nscs)) {
		return(NA)
	} else {
		return(nscs) 
	}
}