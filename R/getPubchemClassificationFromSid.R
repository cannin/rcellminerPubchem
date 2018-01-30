#' Determine if PubChem has Use Classifications for a PubChem SID
#' 
#' @param sid the ID
#' @param debug a boolean, whether to show debug information
#' 
#' @return returns a vector where the first number is the CID and three additional 
#'   entries that denote the existence of the following MESH Terms: 
#'   Pharmacologic Actions: 68020228
#    Specialty Uses of Chemicals: 68020313
#    Toxic Actions: 68004786
#'   
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getPubchemClassificationFromSid("53790588")
#' 
#' @concept rcellminerPubchem
#' @export   
getPubchemClassificationFromSid <- function(sid, debug=TRUE) {
	if(debug) {
		cat("SID: ", sid, "\n")
	}
	
	# Get the CID first
	url <- paste("http://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sid/", sid, "/cids/TXT", sep="")

	if(debug) {		
		cat("URL: ", url, "\n")
	}
	
	results <- getURL(url)
	cid <- strsplit(results, "\n")[[1]]

	# http://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/266552/classification/JSON?classification_type=original
	url <- paste("http://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", cid, "/classification/JSON?classification_type=original", sep="")

	if(debug) {		
		cat("URL: ", url, "\n")
	}
	
	results <- getURL(url)
			
	# Look for the following MESH identifiers 
	# Pharmacologic Actions: 68020228
	# Specialty Uses of Chemicals: 68020313
	# Toxic Actions: 68004786

	chemActUses <- cid

	if(length(grep("68020228", results)) == 1) {
		chemActUses <- c(chemActUses, TRUE)	
	} else {
		chemActUses <- c(chemActUses, FALSE)
	}

	if(length(grep("68020313", results)) == 1) {
		chemActUses <- c(chemActUses, TRUE)		
	} else {
		chemActUses <- c(chemActUses, FALSE)
	}

	if(length(grep("68004786", results)) == 1) {
		chemActUses <- c(chemActUses, TRUE)	
	} else {
		chemActUses <- c(chemActUses, FALSE)
	}
	
	return(chemActUses) 
}
