\name{urlToTemp}
\alias{urlToTemp}
\title{Temporary NFI data }
\description{This function is implemented by other routines of the package to
decompress \code{.zip} files with data sets of the SNFI. }
\usage{urlToTemp(url., dir = tempdir(), 
    file_ext = c("mdb", 
        "DBF", "accdb"), 
    timeOut = timeout(60))}
\arguments{
  \item{url.}{\code{character}.  URL/path to a compressed file of the
SNFI (.zip) having data of either .dbf or .mdb file
extensions.}
  \item{dir}{\code{character}. File path}
  \item{file_ext}{\code{character}. Supported
file extension.}
  \item{timeOut}{\code{request}. Maximum request time,
see \code{\link{timeout}}. Default
\code{timeout(60)}}
}
\details{The data sets are decompressed in a
temporary file of the local
machine. Compressed data contains file
extensions in argument \code{file_ext}.}
\value{\code{character}. Path to the NFI data (.mdb, .DBF, or .accdb)
stored in a temporary file}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
tfmad <- urlToTemp(madridNFI)
tfmad

## Internet resources fail gracefully with an informative message if
## the resource is not available or has changed (and not give a check
## warning nor error):

path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)

tfmad <- urlToTemp(url2, timeOut=timeout(1))


}
