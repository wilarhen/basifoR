\name{readNFI}
\alias{readNFI}
\title{Read NFI data}
\description{This function can read compressed data (\code{.zip}) of the
Spanish National Forest Inventory (SNFI). It can process either
\code{URLs} to data stored in the SNFI web page
(\code{"http://www.miteco.gob.es"}) or paths to files locally
stored.}
\usage{readNFI(nfi, dt.nm = "PCMayores", 
    ...)}
\arguments{
  \item{nfi}{\code{character}.  \code{URL/path} to a compressed file
of the SNFI (\code{.zip}) having data of either .dbf,
.mdb, or .accdb, file extensions, or to a decompressed
file with these supported extensions.}
  \item{dt.nm}{\code{character}. Name of a data set
stored in the imported NFI data. Default
reads \code{'PCMayores'} (3rd NFI) or
\code{'PIESMA'} (2nd NFI).}
  \item{\dots}{Additional arguments in \code{\link{fetchNFI}}.}
}
\details{Compressed data having file
extensions other than \code{.dbf} or
\code{.mdb} are not supported. Most data bases
in \code{2nd} and \code{3rd} stages of the
SNFI can be imported directly from
\code{http://www.miteco.gob.es} using
appropriate \code{URLs}. Data sets from 2nd
SNFI are imported using
\code{\link{read.dbf}}. Data from latter
stages are imported using either
\code{\link{RODBC}} (Windows) or
\code{\link{mdb.get}} (unix-alike
systems). On Windows, a driver for Office
2010 can be installed via the installer
\code{'AccessDatabaseEngine.exe'} available
from Microsoft. In the
case of unix-alike systems, the linux
dependence \code{mdbtools} must be installed.}
\value{\code{data.frame}. A data base  of the NFI.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")
read_ifn4p45 <- readNFI(ifn4p45)[1:100,]
head(read_ifn4p45)

## Retrieval of a data base from the second stage of the second SNFI:

## donttest{
## path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
## url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
## rnfi <- readNFI(url2)
## head(rnfi,3)
## }


}
