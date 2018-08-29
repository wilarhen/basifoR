\name{urlToTemp}
\alias{urlToTemp}
\title{Temporary NFI data }
\description{This function is implemented by other routines of the package to
decompress .zip data of the NFI using either URLs or local
paths.}
\usage{urlToTemp(url.)}
\arguments{
  \item{url.}{\code{character}.  Paths to the data in the temporary file.}
}
\details{The decompressed data is stored in
a temporary file of the local
machine. Compressed data containing file
extensions other than .mdb or .dbf are not
supported.}
\value{\code{character}. Path to the NFI data (.mdb or .dbf) stored in a
temporary file}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>, Cristobal Ordonez <a_cristo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
tfmad <- urlToTemp(madridNFI)
tfmad
}
