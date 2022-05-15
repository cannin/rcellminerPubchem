library(rcellminer)
library(magrittr)
library(xml2)

# LOAD DATA ----
#drugAnnot <- getFeatureAnnot(rcellminerData::drugData)[["drug"]]
drugAnnot <- readRDS("drugAnnot_pubchem_20220515.rds")
tmpDat <- drugAnnot[!is.na(drugAnnot$PUBCHEM_ID),]

# EXTRACT DATA ----
fileVec <- list.files("cache", pattern="xml", full.names = TRUE)
length(fileVec)

results <- data.frame(file=character(0), 
                      CLINICAL_TRIAL_USA=numeric(0), 
                      FDA_APPROVAL=logical(0), 
                      PUBMED_CITATIONS=numeric(0), 
                      MOA_ATC=character(0), 
                      stringsAsFactors=FALSE)

pb <- txtProgressBar(min=1, max=length(fileVec), style=3)

for(i in 1:length(fileVec)) {
  #file <- 'cache/pubchem_42611257.xml'
  setTxtProgressBar(pb, i)
  
  file <- fileVec[i]
  
  tryCatch({
    # READ FILE ----
    doc <- read_xml(file, encoding="latin-1")
    
    ## Rename default XML namespace 
    ns <- xml_ns_rename(xml_ns(doc), d1 = "pug")
    
    # EXTRACT INFO ----
    ## Clinical Trials (US)
    val <- xml_find_all(doc, ".//pug:ExternalTableName[text() = 'clinicaltrials']/following-sibling::pug:ExternalTableNumRows", ns) %>% 
      xml_text %>% 
      as.numeric
    val
    clinicalTrialUsa <- ifelse(length(val) == 1, val, NA)
    
    ## FDA Approval 
    val <- xml_find_all(doc, ".//pug:ExternalTableName[contains(text(), 'fdaorangebook')]/following-sibling::pug:ExternalTableNumRows", ns) %>% 
      xml_text %>% 
      as.numeric
    val 
    fdaApproval <- ifelse(length(val) == 1 && val >= 1, TRUE, FALSE)
    
    ## Pubmed Citations
    val <- xml_find_all(doc, ".//pug:ExternalTableName[contains(text(), 'collection=pubmed')]/following-sibling::pug:ExternalTableNumRows", ns) %>% 
      xml_text %>% 
      as.numeric
    val
    pubmedCitations <- ifelse(length(val) == 1, val, NA)
    
    ## Mechanism of Action (ATC Code)
    # NOTE: Description https://www.whocc.no/atc_ddd_index/?code=L01EC01
    val <- xml_find_all(doc, ".//pug:Name[text() = 'ATC Code']/following-sibling::pug:Value/pug:StringWithMarkup/pug:Markup/pug:Length[text() = '5']/parent::pug:Markup/preceding-sibling::pug:String", ns) %>% 
      xml_text
    val
    moaStrAtc <- ifelse(length(val) == 1, val, NA) 
    
    tmpResults <- data.frame(file=file, 
                             CLINICAL_TRIAL_USA=clinicalTrialUsa, 
                             FDA_APPROVAL=fdaApproval, 
                             PUBMED_CITATIONS=pubmedCitations, 
                             MOA_ATC=moaStrAtc, 
                             stringsAsFactors=FALSE)
    
    results <- rbind(results, tmpResults)    
  }, error = function(e){
    message("\nERROR: FILE: ", file, " MSG: ", e$message, "\n")
  })
}

# MERGE DATA ----
results$PUBCHEM_CID <- results$file %>% gsub(".xml", "", .) %>% gsub("cache/pubchem_", "", .)
x1 <- merge(drugAnnot, results, by.x="PUBCHEM_ID", by.y="PUBCHEM_CID", all.x=TRUE)
x1 <- x1[, !(names(x1) %in% "file")]

# SUMMARY ----
x1$fda_cdb <- ifelse(x1$FDA_STATUS == "FDA approved", TRUE, FALSE)
x1$fda_cdb <- ifelse(!is.na(x1$fda_cdb), x1$fda_cdb, FALSE) %>% as.factor

x1$fda_pug <- ifelse(x1$FDA_APPROVAL, TRUE, FALSE)
x1$fda_pug <- ifelse(!is.na(x1$fda_pug), x1$fda_pug, FALSE) %>% as.factor

caret::confusionMatrix(x1$fda_pug, x1$fda_cdb)

x1$trial_cdb <- ifelse(x1$FDA_STATUS == "Clinical trial", TRUE, FALSE) 
x1$trial_cdb <- ifelse(!is.na(x1$trial_cdb), x1$trial_cdb, FALSE) %>% as.factor

x1$trial_pug <- ifelse(!is.na(x1$CLINICAL_TRIAL_USA) & x1$CLINICAL_TRIAL_USA > 1, TRUE, FALSE)
x1$trial_pug <- ifelse(!is.na(x1$trial_pug), x1$trial_pug, FALSE) %>% as.factor

caret::confusionMatrix(x1$trial_pug, x1$trial_cdb)

x1$moa_cdb <- ifelse(x1$MOA != "", TRUE, FALSE) 
x1$moa_cdb <- ifelse(!is.na(x1$moa_cdb), x1$moa_cdb, FALSE)%>% as.factor

x1$moa_pug <- ifelse(!is.na(x1$MOA_ATC), TRUE, FALSE) %>% as.factor
caret::confusionMatrix(x1$moa_pug, x1$moa_cdb)

write.table(x1, "drugAnnot_pubchem_annot_20220515.txt", sep="\t", row.names=FALSE, quote = FALSE)


