library(rcellminer)

# LOAD DATA ----
#drugAnnot <- getFeatureAnnot(rcellminerData::drugData)[["drug"]]
drugAnnot <- readRDS("drugAnnot_pubchem_20220515.rds")
tmpDat <- drugAnnot[!is.na(drugAnnot$PUBCHEM_ID),]

# DOWNLOAD DATA ----
for(i in 1:nrow(tmpDat)) {
#for(i in 1:100) {
  curId <- tmpDat$PUBCHEM_ID[i]
  
  curFile <- paste0("./cache/pubchem_", curId, ".xml")
  url <- paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/', curId, '/XML')
  
  if(!file.exists(curFile)) {
    download.file(url, curFile)
    Sys.sleep(0.3)
  }
}
