\name{readNFI}
\alias{readNFI}
\title{Read SNFI data from path}
\description{This function can read compressed data (\code{.zip}) from the 
Spanish National Forest Inventory (SNFI). It can process either 
\code{URLs} to data stored on the SNFI web page 
(\code{"http://www.miteco.gob.es"}) or paths to locally stored files.
To read SNFI using codes of Spanish provinces, use \code{\link{getNFI}}.}
\usage{readNFI(nfi, dt.nm = "PCMayores", 
    ...)}
\arguments{
  \item{nfi}{\code{character}. URL or local path to a compressed
file (\code{.zip}) containing SNFI data, or to a
decompressed file with these supported extensions.}
  \item{dt.nm}{\code{character}. Name of a dataset
stored in the imported NFI data. Defaults
to \code{'PCMayores'} (3rd NFI) or
\code{'PIESMA'} (2nd NFI).}
  \item{\dots}{Additional arguments for \code{\link{fetchNFI}}.}
}

\value{\code{data.frame}. A data base  of the NFI.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Retrieval of a data base from the second stage of the SNFI

 \donttest{
 path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
 url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
 rnfi <- readNFI(url2)
 head(rnfi,3)
 }


}
