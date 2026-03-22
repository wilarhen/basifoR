fetchNFI <- structure(function#Fetch SNF Data 
### This function can download and decompress data sets from the Spanish
### National Forest Inventory (SNF) stored in \code{.zip} files,
### whether they are local or remote (URL-based).
                      ##details<< The data should be in files with
                      ##extensions specified in the \code{file_ext}
                      ##argument.
(
    url.,  ##<<\code{character}. Specifies the URL/path to a
           ##compressed SNF (.zip).
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
    ## temp <- tempfile(tmpdir = dir)
    ## is.remote <- grepl('^https?://',url.)
    ## if(is.remote){
    ##     gf <- gracefully_fail(url., timeOut = timeOut)
    ##     if(is.null(gf))
    ##         return(gf)
    ##     download.file(url.,temp)
    ## }
    ## if(!is.remote)
    ##     file.copy(url.,temp)
    ## con <- unzip(temp,
    ##              ## exdir = tempdir(),
    ##              exdir = dir,
    ##              list = TRUE)
    ## con <- unzip(temp,
    ##              ## exdir = tempdir(),
    ##              exdir = dir,
    ##              files = NULL)
    ## supr.only <- file_ext
    ## tos <- grepl(paste(supr.only,
    ##                    collapse = "|"), con)
    ## con  <- tryCatch(
    ##     con[tos],error = function(e) NULL)
    ## file.remove(temp)
    ## return(con)
is.remote <- grepl("^https?://", url.)

if (is.remote) {
    gf <- gracefully_fail(url., timeOut = timeOut)
    if (is.null(gf))
        return(gf)
    zipfile <- file.path(dir, basename(url.))
} else {
    zipfile <- normalizePath(url., mustWork = FALSE)
}

outdir <- file.path(
    dir,
    tools::file_path_sans_ext(basename(zipfile))
)

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

pat <- paste0("\\.(", paste(file_ext, collapse = "|"), ")$")
cached <- list.files(
    outdir,
    pattern = pat,
    full.names = TRUE,
    ignore.case = TRUE
)

if (length(cached) > 0L)
    return(cached)

if (is.remote && !file.exists(zipfile)) {
    download.file(url., zipfile, mode = "wb")
}

con <- unzip(zipfile, exdir = outdir, files = NULL)

tos <- grepl(pat, con, ignore.case = TRUE)
con <- tryCatch(con[tos], error = function(e) NULL)

return(file.path(outdir, basename(con)))
### \code{character}. Returns the path to the fetched and decompressed
### NFI data (.mdb, .DBF, or .accdb) stored in a temporary file.
}, ex = function(){
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)
print(fetch_ifn4p45)
    
})
