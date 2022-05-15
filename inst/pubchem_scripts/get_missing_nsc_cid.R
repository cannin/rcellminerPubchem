library(digest)
library(rcellminerPubchem)
library(rcellminer)
library(curl)

httr::set_config(httr::config(http_version = 2))

Sys.setenv("DEBUG_SIMPLERCACHE"="TRUE")
cacheDir <- "cache"
pubchem_type <- "cid"
debug <- TRUE

# LOAD DATA ----
drugAnnot <- getFeatureAnnot(rcellminerData::drugData)[["drug"]]

getPubchemFromNsc("758188", type=pubchem_type, debug=debug, cachePath=cacheDir)
as.numeric(Sys.getenv("SLEEP_SIMPLERCACHE"))

pb <- txtProgressBar(min=1, max=nrow(drugAnnot), style=3)

for(i in 1:nrow(drugAnnot)) {
#for(i in 1:1000) {
  setTxtProgressBar(pb, i)
  
  #i <- 2
  nsc <- drugAnnot$NSC[i]
  pubchem <- drugAnnot$PUBCHEM_ID[i]
  
  if(!is.na(pubchem)) {
    next
  }
  
  tryCatch({
    #nsc <- "10660"
    cid <- getPubchemFromNsc(nsc, type=pubchem_type, debug=debug, cache=cacheDir)
    drugAnnot$PUBCHEM_ID[i] <- cid   
    
    sleep <- as.numeric(Sys.getenv("SLEEP_SIMPLERCACHE"))
    sleep
    Sys.sleep(sleep)    
  }, error = function(e) {
    message("ERROR: NSC: ", nsc, " MSG: ", e$message, "\n")
    write(nsc, file="errored_nscs.txt", append = TRUE)
  })
}

saveRDS(drugAnnot, "drugAnnot_pubchem_20220515.rds")



