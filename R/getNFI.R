getNFI <- structure(function#get NFI data
### This function downloads data sets from the 2nd to the 4th stages of
### the Spanish National Forest Inventory (SNFI) from the SNFI
### website ("http://www.miteco.gob.es").
(
    provincia,  ##<< Either a \code{character} or
                ##\code{numeric}. Specifies the code of a Spanish
                ##province.
    nfi = 4, ##<< A \code{numeric} value. Indicates the stage of the
             ##NFI. Default is set to 4.
    ... ##<< Arguments used in \code{\link{readNFI}}.
) {
    ## if(is.numeric(nfi))
    if(!is.na(find_provincia_or_codigo(provincia))){
        nfi <- paste0('nfi',nfi)
        provincia <- do.call(nfi, list(prov = provincia))}
    read <- readNFI(provincia, ...)
    return(read)
### \code{data.frame}. This function returns a database of the NFI.
}, ex = function(){
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    rmad <- readNFI(madridNFI)[1:100,]
    head(rmad)


    ## This will download a database from "http://www.miteco.gob.es"
    ## corresponding to the Spanish province of 'Madrid' from the
    ## second stage of the NFI:
    
    ## donttest{
    ## rnfi <- getNFI(provincia = 28, nfi = 2)[1:100,]
    ## str(rnfi,3)
    ## }


})

