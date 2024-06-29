urlToTemp <- structure(function#Temporary NFI data 
### This function is implemented by other routines of the package to
### decompress \code{.zip} files with data sets of the SNFI. 
                       ##details<< The data sets are decompressed in a
                       ##temporary file of the local
                       ##machine. Compressed data contains file
                       ##extensions in argument \code{file_ext}.
(
    url.,  ##<<\code{character}.  URL/path to a compressed file of the
           ##SNFI (.zip) having data of either .dbf or .mdb file
           ##extensions.
    dir = tempdir(), ##<<\code{character}. File path
    file_ext = c('mdb','DBF', 'accdb'), ##<<\code{character}. Supported
                                        ##file extension.
    timeOut = timeout(60) ##<<\code{request}. Maximum request time,
                           ##see \code{\link{timeout}}. Default
                           ##\code{timeout(60)}
) {
    if(is.null(url.))
        return(NULL)
    ## temp <- tempfile()
    temp <- tempfile(tmpdir = dir)
    is.remote <- grepl('^https?://',url.)
    if(is.remote){
        gf <- gracefully_fail(url., timeOut = timeOut)
        if(is.null(gf))
            return(gf)
        download.file(url.,temp)
    }
        if(!is.remote)
        file.copy(url.,temp)
    con <- unzip(temp,
                 ## exdir = tempdir(),
                 exdir = dir,
                 list = TRUE)
    con <- unzip(temp,
                 ## exdir = tempdir(),
                 exdir = dir,
                 files = NULL)
    supr.only <- file_ext
    tos <- grepl(paste(supr.only,
                       collapse = "|"), con)
    con  <- tryCatch(
        con[tos],error = function(e) NULL)
    file.remove(temp)
    return(con)
    
### \code{character}. Path to the NFI data (.mdb, .DBF, or .accdb)
### stored in a temporary file
}, ex = function(){
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
tfmad <- urlToTemp(madridNFI)
tfmad

## Internet resources fail gracefully with an informative message if
## the resource is not available or has changed (and not give a check
## warning nor error):

path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)

tfmad <- urlToTemp(url2, timeOut=timeout(1))


})
