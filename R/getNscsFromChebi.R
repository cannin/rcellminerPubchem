#' Get NSCs from CHEBI ID
#' 
#' @param id the ID
#' @param debug a boolean, whether to show debug information
#' @param cachePath a path for caching results, if not specified the default cache  
#'   path for simpleRCache will be used 
#'   
#' @return returns a data.frame with all CIDs for the CHEBI ID; an annotation 
#'   of if the CID represents the whole compound "standardized" or a component 
#'   "component"; and a comma-delimited string of NSCs for the CIDs
#'   
#' @details "component" compounds could represent things like the ubiquitous HCl 
#'   or the compound of interest
#'   
#' @author Augustin Luna (augustin@mail.nih.gov)
#' 
#' @examples
#' getNscsFromChebi("CHEBI:9710")
#' 
#' @concept rcellminerPubchem
#' @export   
getNscsFromChebi <- function(id, debug=TRUE, cachePath=NULL) {
  #id <- "CHEBI:9710" 
  #id <- "CHEBI:15422" 
  #id <- "CHEBI:10003"
  #debug <- TRUE
  
  nscs <- NULL 
  
  if(is.null(cachePath)) {
    setCacheRootPath()
  } else {
    setCacheRootPath(cachePath)
  }

  GETCached <- addMemoization(GET)
  
  if(debug) {
    cat("ID: ", id, "\n")
  }
  
  url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sourceid/CHEBI/", id, "/XML")
  
  results <- url %>% GETCached() %>% content("text")
  
  #results <- getURL(url)
  doc <- xmlInternalTreeParse(results)
  
  # Set namespaces (XPath expressions will not work otherwise)
  namespaces <- c(ns="http://www.ncbi.nlm.nih.gov")
  
  # Parsing
  path <- "//ns:PC-CompoundType_id_cid"
  cids <- xpathSApply(doc, path, namespaces=namespaces, xmlValue)
  
  if(debug) { cat("CIDS: ", length(cids), "\n") }
  
  # Exit if there are no CIDs
  emptyDf <- data.frame(cid=NA, cmpdType=NA, nsc=NA)
  
  if(length(cids) == 0) {
    if(debug){ cat("HIT\n") }
    return(emptyDf)
  }
  
  path <- "//ns:PC-CompoundType_type"
  cmpdTypes <- xpathSApply(doc, path, namespaces=namespaces, xmlGetAttr, 'value')
  
  if(cmpdTypes[1] == "deposited") {
    cmpdTypes <- cmpdTypes[2:length(cmpdTypes)]
  }
  
  df <- data.frame(cid=cids, cmpdType=cmpdTypes, stringsAsFactors=FALSE)
  
  # Remove duplicates
  df <- unique(df)
  
  if(all(cids[1] == cids)) {
    df <- df[1,]
  }
  
  # Add a blank column for NSCs
  df <- cbind(df, nsc=rep(NA, nrow(df)))
  
  nrow(df)
  
  for(j in 1:nrow(df)) {
    cat("A: ",  df$cid[j], "\n")
    
    # Make sure nscs is NULL
    nscs <- NULL 
    
    url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", df$cid[j], "/synonyms/TXT")
    results <- url %>% GETCached() %>% content("text")

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
    
    tmp <- setdiff(nscs, "")
    nscs <- paste(tmp, collapse=",")
    
    # Replace with NA if no NSCs
    if(nscs == "") {
      nscs <- NA
    }
    
    df$nsc[j] <- nscs
  }
	
	if(all(is.na(df$nsc))) {
		return(emptyDf)
	} else {
		return(df) 
	}
}