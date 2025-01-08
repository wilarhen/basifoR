getNFI <- structure(function#Get SNF data from Spanish province
### This function processes Spanish provinces to download and process
### data sets from the 2nd to the 4th stages of the Spanish National
### Forest Inventory (SNF) from the SNF website
### ("http://www.miteco.gob.es").
(
    provincia,  ##<< Either a \code{character} or \code{numeric}
                ##specifying the code of a Spanish province.
    nfi.nr = 4, ##<< A \code{numeric} value indicating the stage of
                ##the SNF. Default is set to the forth stage of the SNF.
    ... ##<< Additional arguments used in \code{\link{readNFI}}.
) {
    if(is.null(provincia))
        return(provincia)
        nfi <- paste0('nfi',nfi.nr)
    provincia <- do.call(nfi, list(prov = provincia))
## }
    read <- readNFI(provincia, ...)
    return(read)
### \code{data.frame} with numeric columns converted from factors back
### to numeric, while preserving the format of character columns.
}, ex = function(){
    ## Process SNF data for Toledo stored locally
    # Path to Toledo data file in 'basifoR' package
    ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")
    
    # Decompress SNF data from the specified file path or URL
    fetch_ifn4p45 <- fetchNFI(ifn4p45)
    
    # Read and process the data (first 100 rows)
    get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100,]
    
    # Display structure of the data
    str(get_ifn4p45)
    
    ## Alternatively, download data from 'www.miteco.gob.es'
    ## Specify province name/number to read the data:
    
    ## donttest{
    ### Compute dendrometrics for Toledo (code 45) for NFI 4
    ## dendromet_ifn4p45 <- getNFI(provincia=45,nfi=4)
    ### Display first few rows
    ## str(dendromet_ifn4p45)
    ## }
})

