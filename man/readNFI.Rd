\name{readNFI}
\alias{readNFI}
\title{Read NFI data}
\description{This function can download into the R environmet Data sets of the
Spanish National Forest Inventory (NFI) using either remote or
local paths to compressed (.zip) files.}
\usage{readNFI(url, dt.nm = "PCMayores")}
\arguments{
  \item{url}{\code{character} or \code{data.frame}.  URL/path to
a compressed file of the NFI (.zip) having data of
either .dbf or .mdb file extensions.}
  \item{dt.nm}{\code{character}. Name of a data set
stored in the imported data. Default
reads \code{'PCMayores'} (3rd NFI) or
\code{'PIESMA'} (2nd NFI).}
}
\details{Compressed files having data
extensions other than .dbf or .mdb are not
supported. Some data sets of the 2nd NFI as
well ass most of the data in the 3rd NFI can
be imported directly from
\href{https://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/cartografia_informacion_disp.aspx}{http://mapama.gob.es}
using links to the compressed files. The
compressed files of the 4th NFI should be read
from local paths. The .dfb formats in
compressed data of the second NFI are imported
using \code{\link{read.dbf}}. The .mdb formats
in latter NFIs are imported using either
\code{\link{RODBC}} (Windows) or
\code{\link{mdb.get}} (unix-alike systems). In
the former case, a 32-bit access driver should
be installed in the system, and the package
should be implemented using a 32-bit R
version. In the case of unix-alike systems the
package mdb-tools should be installed.}
\value{\code{data.frame}. A data base  of the NFI.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>, Cristobal Ordonez <a_cristo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
head(rmad)
}
