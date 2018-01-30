library(paxtoolsr)
library(rcellminerPubchem)
library(simpleRCache)
library(rcellminer)
library(data.table)

setCacheRootPath()
getNscsFromChebiCached <- addMemoization(getNscsFromChebi)

workDir <- file.path(.lmp, "gene_set_pathway_analysis", "data")

sif <- downloadPc2("PathwayCommons.8.All.BINARY_SIF.hgnc.txt.sif.gz")
dtNet <- downloadPc2("PathwayCommons.8.All.EXTENDED_BINARY_SIF.hgnc.txt.gz")

matchEdges <- dtNet$edges

ids <- unique(c(sif[,1], sif[,3]))

chebiIds <- grep("^CHEBI", ids, value=TRUE)

results <- list()

for(i in 1:length(chebiIds)) {
    chebiId <- chebiIds[i]
    
    cat("CHEBI: ", chebiId, "\n")
    
    tryCatch({
        results[[chebiId]] <- getNscsFromChebiCached(chebiId)
    }, error = function(e) {
        source("convertPc.R")
    })
}

# Add CHEBI ID column to results
results2 <- results

for(i in 1:length(results)) {
    r1 <- results[[i]]
    
    if(!is.null(r1)) {
        chebiId <- names(results[i])
        
        chebiCol <- rep(chebiId, nrow(results[[i]]))
        results2[[i]] <- cbind(chebiId=chebiCol, results[[i]])
    } else {
        results2[[i]] <- data.frame(chebiId=chebiCol, cid=NA, cmpdType=NA, nsc=NA)
    }
}

rDf <- do.call("rbind", results2)
write.table(rDf, file=file.path(workDir, "rDf.txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)

nscs <- NULL 

for(i in 1:length(results)) {
    r1 <- results[[i]]
    
    # Check that the entire entry is not NA
    if(!is.vector(r1) && !is.na(r1)) {
        for(j in 1:nrow(r1)) {
            rT1 <- r1[j, "nsc"]
            
            # Check that each PubChem to NSC mapping is not NA
            if(!is.na(rT1)) {
                rT2 <- unlist(strsplit(rT1, ","))
                nscs <- c(nscs, rT2)    
            }
        }      
    }
}

nscs <- unique(nscs)

# Extract data from rcellminer on the NSCs
drugAnnot <- getFeatureAnnot(rcellminerData::drugData)[["drug"]]
dA1 <- drugAnnot[drugAnnot$NSC %in% nscs, c("NSC", "NAME", "FDA_STATUS")]

# Only 334 NSCs picked up
nrow(dA1)

## Ignore NSCs with blank FDA_STATUS
dA2 <- dA1[dA1$FDA_STATUS != "-", ]

dAR <- NULL
dARBool <- NULL

for(i in 1:nrow(dA2)) {
    nsc <- dA2$NSC[i]
    status <- dA2$FDA_STATUS[i]
    name <- dA2$NAME[i]
    
    idx <- which(grepl(paste0("\\b", nsc, "\\b"), rDf$nsc))
    
    # as.vector because results returned as data.frame
    x1 <- as.vector(rDf$chebiId[idx])
    
    # Convert to data.frame from data.table to make filtering easier
    x2 <- setDF(matchEdges)
    x3 <- x2[which(x2$PARTICIPANT_A %in% x1), 1:3]
    
    # Append rcellminer columns to Pathway Commons data
    x5 <- rep(nsc, nrow(x3))
    x7 <- rep(name, nrow(x3))
    x8 <- rep(status, nrow(x3))
    x6 <- cbind(x3, NSC=x5, NAME=x7, STATUS=x8)
    
    if(nrow(x3) > 0) {
        dARBool <- c(dARBool, TRUE) 
    } else {
        dARBool <- c(dARBool, FALSE)
    }
    
    dAR <- rbind(dAR, x6)
}

dAR <- unique(dAR)

tmpDAR <- dAR[which(!is.na(dAR$STATUS)), c("PARTICIPANT_A", "NSC", "NAME", "STATUS")]
tmpDAR <- unique(tmpDAR)

write.table(tmpDAR, file=file.path(workDir, "dAR.txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)

allFdaCtNscs <- read.table(file.path(workDir, "fdaClinNscsChecklist.txt"), header=TRUE, sep="\t")

