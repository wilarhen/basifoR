\name{getNFI}
\alias{getNFI}
\title{Read SNFI data from Spanish province}
\description{This function processes Spanish provinces to download and process
data sets from the 2nd to the 4th stages of the Spanish National
Forest Inventory (SNFI) from the SNFI website
("http://www.miteco.gob.es").}
\usage{getNFI(provincia, nfi.nr = 4, 
    ...)}
\arguments{
  \item{provincia}{Either a \code{character} or \code{numeric}
specifying the code of a Spanish province.}
  \item{nfi.nr}{A \code{numeric} value indicating the stage of
the NFI. Default is set to 4.}
  \item{\dots}{Additional arguments used in \code{\link{readNFI}}.}
}

\value{\code{data.frame}. This function returns a database of the NFI.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Process SNFI data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNFI data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)

# Read and process the data (first 100 rows)
get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100,]

# Display structure of the data
str(get_ifn4p45)

## Alternatively, download data from 'www.miteco.gob.es'
## Specify province name/number to read the data:

## donttest{
## Compute dendrometrics for Toledo (code 45) for NFI 4, height >= 8
## dendromet_ifn4p45 <- getNFI(provincia=45,nfi=4)
## Display first few rows
## str(dendromet_ifn4p45)
## }
}
