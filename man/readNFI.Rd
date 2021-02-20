\name{readNFI}
\alias{readNFI}
\title{Read NFI data}
\description{This function can retrieve data sets in the Spanish National
Forest Inventory (NFI); this processes either remote URLs or local
file paths to compressed (.zip) NFI files.}
\usage{readNFI(nfi, dt.nm = "PCMayores")}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to
a compressed file of the NFI (.zip) having data of
either .dbf or .mdb file extensions.}
  \item{dt.nm}{\code{character}. Name of a data set
stored in the imported NFI data. Default
reads \code{'PCMayores'} (3rd NFI) or
\code{'PIESMA'} (2nd NFI).}
}
\details{Compressed data having file
extensions other than \code{.dbf} or
\code{.mdb} are not supported. Most data in
2nd and 3rd NFIs can be imported directly from
\code{http://mapama.gob.es}
using links to the compressed files. Data in
the 4th NFI must be read from local
paths. Data in second NFI are imported using
\code{\link{read.dbf}}. Data in latter NFIs
are imported using either \code{\link{RODBC}}
(Windows) or \code{\link{mdb.get}} (unix-alike
systems). In the former case, a 32-bit access
driver should be installed in the system, and
the package must be implemented using a 32-bit
R version. In the case of unix-alike systems,
dependence \code{mdb-tools} must be installed.}
\value{\code{data.frame}. A data base  of the NFI.}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
head(rmad)
}
