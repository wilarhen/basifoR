\name{readNFI}
\alias{readNFI}
\title{Read SNF data from path}
\description{This function can read compressed data (\code{.zip}) from the 
Spanish National Forest Inventory (SNF). It can process either 
\code{URLs} to data stored on the SNF web page 
(\code{"http://www.miteco.gob.es"}) or paths to locally stored files.}
\usage{readNFI(nfi, nfi.nr = 4, 
    dt.nm = "PCMayores", 
    file_ext = NULL, 
    file_name = NULL, 
    ...)}
\arguments{
  \item{nfi}{\code{character}. URL or local path to a compressed
file (\code{.zip}) containing SNF data, or to a
decompressed file with these supported extensions.}
  \item{nfi.nr}{nfi.nr }
  \item{dt.nm}{\code{character}. Name of a dataset
stored in the imported NFI data. Defaults
to \code{'PCMayores'} (3rd NFI) or
\code{'PIESMA'} (2nd NFI).}
  \item{file_ext}{\code{character}. Optional file extension(s)
passed to \code{\link{fetchNFI}}. Use
\code{"csv"} to read zipped csv files.}
  \item{file_name}{\code{character}. Optional file names passed
to \code{\link{fetchNFI}}.}
  \item{\dots}{Additional arguments for \code{\link{fetchNFI}}.}
}

\value{\code{data.frame} with numeric columns converted from factors back
to numeric, while preserving the format of character columns.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## donttest{
### Retrieval of a database from the second stage of the SNF using a URL resource

## ifn2_path <-
## '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
## ifn2_url <- httr::modify_url("https://www.miteco.gob.es", path
## = ifn2_path)

## read_ifn2 <- readNFI(ifn2_url)

## str(read_ifn2) }

## French NFI tree table read from the official web resource
## f <- "https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2024_en.zip"
## arbre <- readNFI(f, file_ext = "csv", file_name = "ARBRE")
## str(arbre)

}
