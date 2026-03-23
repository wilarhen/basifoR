urlToTemp <- structure(function#Temporary NFI data 
### This function is implemented by other routines of the package to
### decompress \code{.zip} files with data sets of the SNF. 
                       ##details<< This function is deprecated and
                       ##will be made defunct; use the replacement
                       ##function \code{\link{fetchNFI}}.
(
    url.,  ##<<\code{character}.  URL/path to a compressed file of the
           ##SNF (.zip) having data of either .dbf or .mdb file
           ##extensions..
    timeOut = timeout(60), ##<<\code{request}. Maximum request time,
                          ##see \code{\link[httr]{timeout}}. Default
                          ##\code{timeout(60)}
    ... ##<< Additional arguments used in \code{\link{fetchNFI}}.
) {
    .Deprecated("fetchNFI")
    NewFunc <- fetchNFI(url. = url., timeOut = timeOut, ...)
    return(NewFunc)
    ## if(is.null(url.))
    ##     return(NULL)
    ## temp <- tempfile()
    ## is.remote <- grepl('http',url.)
    ## if(is.remote){
    ##     gf <- gracefully_fail(url., timeOut = timeOut)
    ##     if(is.null(gf))
    ##         return(gf)
    ##     download.file(url.,temp)
    ## }
    ##     if(!is.remote)
    ##     file.copy(url.,temp)
    ## con <- unzip(temp,
    ##              exdir = tempdir(),
    ##              list = TRUE)
    ## con <- unzip(temp,
    ##              exdir = tempdir(),
    ##              files = NULL)
    ## supr.only <- c('mdb','DBF')
    ## tos <- grepl(paste(supr.only,
    ##                    collapse = "|"), con)
    ## con <- con[tos]
    ## return(tryCatch(
    ##     con,error = function(e) NULL))
### \code{character}. Path to the NFI data (.mdb or .dbf) stored in a
### temporary file
}, ex = function(){
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)
print(fetch_ifn4p45)


})
