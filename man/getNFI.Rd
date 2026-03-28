\name{getNFI}
\alias{getNFI}
\title{Resolve and fetch Spanish NFI archives for a province}
\description{Resolve a Spanish province identifier to the corresponding archive URL for the requested Spanish National Forest Inventory (SNFI) stage and download/extract the matching files with \code{\link{fetchNFI}}. When \code{provincia} is already a local \code{.zip} path or a remote archive URL, the function skips province resolution and fetches that archive directly.}
\usage{getNFI(provincia, nfi.nr = 4, 
    ...)}
\arguments{
  \item{provincia}{\code{character} or \code{numeric}. Spanish province code or province name to resolve, or a local \code{.zip} path / remote archive URL to fetch directly.}
  \item{nfi.nr}{\code{numeric}(1). Spanish National Forest Inventory stage to resolve when \code{provincia} is a province identifier. Supported values are \code{2}, \code{3}, and \code{4}. Ignored when \code{provincia} already points to an archive.}
  \item{\dots}{Additional arguments passed to \code{\link{fetchNFI}}, such as \code{file_ext}, \code{file_name}, \code{dir}, or \code{timeOut}.}
}
\details{\code{getNFI()} is a light wrapper around the internal stage-specific resolvers \code{nfi2()}, \code{nfi3()}, and \code{nfi4()} plus \code{\link{fetchNFI}}. For province inputs, it first translates \code{provincia} to the archive URL that corresponds to \code{nfi.nr}. It then downloads the archive when needed, extracts the requested files, and returns the extracted paths.

Use \code{nfi.nr = 2}, \code{3}, or \code{4} to target the corresponding SNFI stage. Use \code{...} to restrict the extracted files, for example with \code{file_ext} or \code{file_name} as supported by \code{\link{fetchNFI}}. This function fetches files; use \code{\link{readNFI}} to import the extracted tables into R.}
\value{\code{character}. Paths to extracted inventory files returned by \code{\link{fetchNFI}}. The result is typically a vector of \code{.mdb}, \code{.accdb}, \code{.dbf}, or other requested files inside the retrieved archive.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Fetch bundled Toledo data from the example archive shipped with basifoR
toledo_zip <- system.file("Ifn4_Toledo.zip", package = "basifoR")

files <- getNFI(toledo_zip)

## Show the extracted filenames
basename(files)
}
