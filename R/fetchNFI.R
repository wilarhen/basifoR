fetchNFI <- structure(function#Fetch SNFI Data 
### This function fetches and decompresses data sets from the Spanish
### National Forest Inventory (SNFI) stored in \code{.zip} files,
### whether they are local or remote (URL-based).
                      ##details<< The data should be in files with
                      ##extensions specified in the \code{file_ext}
                      ##argument.
(
    url.,  ##<<\code{character}. Specifies the URL/path to a
           ##compressed SNFI (.zip).
    dir = tempdir(), ##<<\code{character}. Directory where the fetched
                     ##file will be stored.
    file_ext = c('mdb','DBF', 'accdb'), ##<<\code{character}. Supported
                                        ##file extensions.
    timeOut = timeout(60) ##<<\code{request}. Maximum request time,
                          ##see \code{\link{timeout}}. Default is
                          ##\code{timeout(60)}.
) {
    if(is.null(url.))
        return(NULL)
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
### \code{character}. Returns the path to the fetched and decompressed
### NFI data (.mdb, .DBF, or .accdb) stored in a temporary file.
}, ex = function(){
    ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")
    tfmad <- fetchNFI(ifn4p45)
    tfmad

## Internet resources will gracefully fail with an informative message
## if the resource is not available or has changed (without causing a
## check warning or error):
    
    path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
    url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)

    tfmad <- fetchNFI(url2, timeOut=timeout(1))


})
