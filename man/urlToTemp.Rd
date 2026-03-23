\name{urlToTemp}
\alias{urlToTemp}
\title{Temporary NFI data }
\description{This function is implemented by other routines of the package to
decompress \code{.zip} files with data sets of the SNF. \code{}}
\usage{urlToTemp(url., timeOut = timeout(60), 
    ...)}
\arguments{
  \item{url.}{\code{character}. \code{} URL/path to a compressed file of the
SNF (.zip) having data of either .dbf or .mdb file
extensions..}
  \item{timeOut}{\code{request}. Maximum request time,
see \code{\link{timeout}}. Default
\code{timeout(60)}}
  \item{\dots}{Additional arguments used in \code{\link{fetchNFI}}.}
}
\details{This function is deprecated and
will be made defunct; use the replacement
function \code{\link{fetchNFI}}.}
\value{\code{character}. Path to the NFI data (.mdb or .dbf) stored in a
temporary file}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)
print(fetch_ifn4p45)


}
