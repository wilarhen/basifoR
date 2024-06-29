\name{getNFI}
\alias{getNFI}
\title{get NFI data}
\description{This function downloads data sets from the 2nd to the 4th stages of
the Spanish National Forest Inventory (SNFI) from the SNFI
website ("http://www.miteco.gob.es").}
\usage{getNFI(provincia, nfi = 4, 
    ...)}
\arguments{
  \item{provincia}{Either a \code{character} or
\code{numeric}. Specifies the code of a Spanish
province.}
  \item{nfi}{A \code{numeric} value. Indicates the stage of the
NFI. Default is set to 4.}
  \item{\dots}{Arguments used in \code{\link{readNFI}}.}
}

\value{\code{data.frame}. This function returns a database of the NFI.}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
head(rmad)


## This will download a database from "http://www.miteco.gob.es"
## corresponding to the Spanish province of 'Madrid' from the
## second stage of the NFI:

## donttest{
## rnfi <- getNFI(28, nfi = 2)[1:100,]
## str(rnfi,3)
## }


}
