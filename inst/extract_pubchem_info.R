library(xml2)
library(magrittr)

# NOTE: Example Uses Vemurafenib, PubChem CID: 42611257
url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/42611257/XML"
#download.file(url, "~/Downloads/pubchem_42611257.xml")

# READ FILE ----
doc <- read_xml('~/Downloads/pubchem_42611257.xml')

## Rename default XML namespace 
ns <- xml_ns_rename(xml_ns(doc), d1 = "pug")

# EXTRACT INFO ----
## Clinical Trials (US)
val <- xml_find_all(doc, ".//pug:ExternalTableName[text() = 'clinicaltrials']/following-sibling::pug:ExternalTableNumRows", ns) %>% 
  xml_text %>% 
  as.numeric
val

## Clinical Trials (Worldwide)
val <- xml_find_all(doc, ".//pug:ExternalTableName[contains(text(), 'clinicaltrials')]/following-sibling::pug:ExternalTableNumRows", ns) %>% 
  xml_text %>% 
  as.numeric
val

## FDA Approval 
val <- xml_find_all(doc, ".//pug:ExternalTableName[contains(text(), 'fdaorangebook')]/following-sibling::pug:ExternalTableNumRows", ns) %>% 
  xml_text %>% 
  as.numeric
val 

## Pubmed Citations
val <- xml_find_all(doc, ".//pug:ExternalTableName[contains(text(), 'collection=pubmed')]/following-sibling::pug:ExternalTableNumRows", ns) %>% 
  xml_text %>% 
  as.numeric
val

## Mechanism of Action (ATC Code)
# NOTE: Description https://www.whocc.no/atc_ddd_index/?code=L01EC01
val <- xml_find_all(doc, ".//pug:Name[text() = 'ATC Code']/following-sibling::pug:Value/pug:StringWithMarkup/pug:Markup/pug:Length[text() = '5']/parent::pug:Markup/preceding-sibling::pug:String", ns) %>% 
  xml_text
val
