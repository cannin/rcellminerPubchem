#' Get NSC from Name using PubChem PUG API
#'
#' @param name a name
#' @param debug a boolean, show debugging information (default: TRUE)
#' @return the name used by PubChem
#' 
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getNscFromName("17-AAG")
#' 
#' @concept rcellminerPubchem
#' @export
#' 
#' @importFrom httr GET content
getNscFromName <- function(id, debug=TRUE, cachePath=NULL) {
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
	
	url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", id, "/synonyms/TXT")
	
	if(debug) {		
		cat("URL: ", url, "\n")
	}	

	# content() can conflict with BioBase::content()
	results <- suppressMessages(url %>% GETCached() %>% content("text"))
	results_vec <- strsplit(results, "\n")[[1]]

	if(debug) {		
		cat("Results: ", results, "\n")
		cat("Results_vec: ", results_vec, "\n")
	}	
	
	idx <- which(grepl("^NSC", results_vec))

	# Check that there are results and that the first result is unlikely to be a HTTP issue
	if(length(results_vec) >= 1 && is.na(str_match(results_vec[1], "Status")) && 
	   !grepl("xml", results_vec[1]) && length(idx) >= 1 &&
	   any(tolower(results_vec) == tolower(id))) {
	  tmp <- gsub("[^[:alnum:]]", "", results_vec[idx])
	  idx <- which(grepl("^NSC", tmp))
	  tmp <- unique(tmp[idx])
		result <- c(result, tmp)			
	} else {
	  result <- c(result, NA)
	}
		
	if(is.null(result)) {
		return(NA)
	} else {
		return(result) 
	}
}
