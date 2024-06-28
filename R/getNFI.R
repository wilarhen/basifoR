getNFI <- structure(function#get NFI data
### This function can download data sets of the Spanish National
### Forest Inventory (SNFI) stored in the SNFI web page
### (\code{"http://www.miteco.gob.es"}).
                      ##details<< 
(
    prov,  ##<<\code{character} or \code{numeric}. Code of a Province
           ##of SPain.
    nfi = 4, ##<< \code{numeric}. Stage of the NFI. Default 4.
    ... ##<< Arguments in \code{\link{readNFI}}.
    
) {
    if(is.numeric(nfi))
        nfi <- paste0('nfi',nfi)
    url_ <- do.call(nfi, list(prov = prov))
    ## return(url_)
    read <- readNFI(url_, ...)
return(read)
### \code{data.frame}. A data base  of the NFI.
}, ex = function(){
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
head(rmad)

## Downloading a data base from "http://www.miteco.gob.es" corresponding to the Spanish province of 'Madrid' established during the second stage of the NFI:

## donttest{
## rnfi <- getNFI(28, nfi = 2)[1:100,]
## str(rnfi,3)
## }


})

