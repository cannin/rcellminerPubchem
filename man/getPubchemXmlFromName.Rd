% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPubchemXmlFromName.R
\name{getPubchemXmlFromName}
\alias{getPubchemXmlFromName}
\title{Get PubChem XML for a Given Name using PubChem PUG API}
\usage{
getPubchemXmlFromName(id, debug = TRUE, cachePath = NULL)
}
\arguments{
\item{debug}{a boolean, show debugging information (default: TRUE)}

\item{name}{a name}
}
\value{
the name used by PubChem
}
\description{
Get PubChem XML for a Given Name using PubChem PUG API
}
\examples{
getPubchemXmlFromName("vemurafenib")

}
\author{
Augustin Luna (augustin@mail.nih.gov)
}
\concept{rcellminerPubchem}
