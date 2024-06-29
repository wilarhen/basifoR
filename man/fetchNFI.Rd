\name{fetchNFI}
\alias{fetchNFI}
\title{Fetch SNFI Data }
\description{This function fetches and decompresses data sets from the Spanish
National Forest Inventory (SNFI) stored in \code{.zip} files,
whether they are local or remote (URL-based).}
\usage{fetchNFI(url., dir = tempdir(), 
    file_ext = c("mdb", 
        "DBF", "accdb"), 
    timeOut = timeout(60))}
\arguments{
  \item{url.}{\code{character}. Specifies the URL/path to a
compressed SNFI (.zip).}
  \item{dir}{\code{character}. Directory where the fetched
file will be stored.}
  \item{file_ext}{\code{character}. Supported
file extensions.}
  \item{timeOut}{\code{request}. Maximum request time,
see \code{\link{timeout}}. Default is
\code{timeout(60)}.}
}
\details{The data should be in files with
extensions specified in the \code{file_ext}
argument.}
\value{\code{character}. Returns the path to the fetched and decompressed
NFI data (.mdb, .DBF, or .accdb) stored in a temporary file.}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    tfmad <- fetchNFI(madridNFI)
    tfmad

## Internet resources will gracefully fail with an informative message
## if the resource is not available or has changed (without causing a
## check warning or error):
    
    path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
    url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)

    tfmad <- fetchNFI(url2, timeOut=timeout(1))


}
