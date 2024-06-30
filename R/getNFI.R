getNFI <- structure(function#get NFI data
### This function downloads data sets from the 2nd to the 4th stages of
### the Spanish National Forest Inventory (SNFI) from the SNFI
### website ("http://www.miteco.gob.es").
(
    provincia,  ##<< Either a \code{character} or
                ##\code{numeric}. Specifies the code of a Spanish
                ##province.
    nfi.nr = 4, ##<< A \code{numeric} value. Indicates the stage of
                ##the NFI. Default is set to 4.
    ... ##<< Arguments used in \code{\link{readNFI}}.
) {
    if(is.null(provincia))
        return(provincia)
    if(!is.na(find_provincia_or_codigo(provincia)) &&
       !is_decompressed(provincia)){
        nfi <- paste0('nfi',nfi.nr)
        provincia <- do.call(nfi, list(prov = provincia))}
    read <- readNFI(provincia, ...)
    return(read)
### \code{data.frame}. This function returns a database of the NFI.
}, ex = function(){
## Process SNFI data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Download and decompress SNFI data
read_ifn4p45 <- fetchNFI(ifn4p45)

# Read and process the data (first 100 rows)
get_ifn4p45 <- getNFI(read_ifn4p45)[1:100,]

# Display structure of the data
str(get_ifn4p45)
    
## Alternatively, download data from 'www.miteco.gob.es'
## Specify province name/number to read the data:

## \donttest{
## Compute dendrometrics for Toledo (code 45) for NFI 4, height >= 8
## dendromet_ifn4p45 <- getNFI(provincia=45,nfi=4)
## Display first few rows
## str(dendromet_ifn4p45)
## }

    
})

