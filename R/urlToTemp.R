urlToTemp <- structure(function#Temporary NFI data 
### This function is implemented by other routines of the package to
### decompress .zip data of the NFI using either URLs or local
### paths.
                       ##details<< The decompressed data is stored in
                       ##a temporary file of the local
                       ##machine. Compressed data containing file
                       ##extensions other than .mdb or .dbf are not
                       ##supported.
(
    url.  ##<<\code{character}.  Paths to the data in the temporary file.
) {
    temp <- tempfile()
    is.remote <- grepl('www.mapama.gob.es',url.)
    if(is.remote)
        download.file(url.,temp)
    if(!is.remote)
        file.copy(url.,temp)
    con <- unzip(temp,
                 exdir = tempdir(),
                 list = TRUE)
    con <- unzip(temp,
                 exdir = tempdir(),
                 files = NULL)
    supr.only <- c('mdb','DBF')
    tos <- grepl(paste(supr.only,
                       collapse = "|"), con)
    con <- con[tos]
    return(tryCatch(
        con,error = function(e) NULL))
### \code{character}. Path to the NFI data (.mdb or .dbf) stored in a
### temporary file
}, ex = function(){
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="Rbasifor")
tfmad <- urlToTemp(madridNFI)
tfmad
})
