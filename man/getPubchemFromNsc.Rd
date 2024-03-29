% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPubchemFromNsc.R
\name{getPubchemFromNsc}
\alias{getPubchemFromNsc}
\title{Get PubChem SID/CID value from an NSC using PubChem PUG API}
\usage{
getPubchemFromNsc(id, type = "cid", debug = TRUE, cachePath = NULL)
}
\arguments{
\item{id}{an NSC ID}

\item{type}{"cid" or "sid" PubChem ID types}

\item{debug}{a boolean, print debug information? (Default: TRUE)}

\item{cachePath}{path for cache files}
}
\value{
a PubChem CID/SID or NA, otherwise
}
\description{
Get PubChem SID/CID value from an NSC using PubChem PUG API
}
\examples{
getPubchemFromNsc("94600", "cid") #Should be: 24360
getPubchemFromNsc("94600", "sid") #Should be: 399733

}
\author{
Augustin Luna (augustin@mail.nih.gov)
}
\concept{rcellminerPubchem}
