<<<<<<< HEAD
readNFI <- structure(function#Read NFI data
### This function can retrieve data sets of the Spanish National
### Forest Inventory (SNFI). It can process either \code{URLs} to data
### stored in the SNFI web page (\code{"http://www.miteco.gob.es"}) or
### paths to files locally stored.
                      ##details<< Compressed data having file
                      ##extensions other than \code{.dbf} or
                      ##\code{.mdb} are not supported. Most data bases
                      ##in \code{2nd} and \code{3rd} stages of the
                      ##SNFI can be imported directly from
                      ##\code{http://www.miteco.gob.es} using
                      ##appropriate \code{URLs}. Data sets from 2nd
                      ##SNFI are imported using
                      ##\code{\link[foreign]{read.dbf}}. Data from latter
                      ##stages are imported using either
                      ##\code{\link[RODBC]{odbcConnect}} (Windows) or
                      ##\code{\link[Hmisc]{mdb.get}} (unix-alike
                      ##systems). Data from 4th SNFI must be read from
                      ##local paths.  On Windows, a driver for Office
                      ##2010 can be installed via the installer
                      ##\code{'AccessDatabaseEngine.exe'} available
                      ##from Microsoft, and the package must be
                      ##implemented using a 32-bit R version. In the
                      ##case of unix-alike systems, the linux
                      ##dependence \code{mdbtools} must be installed.
(
    nfi,  ##<<\code{character} or \code{data.frame}.  \code{URL/path}
          ##to a compressed file of the SNFI (\code{.zip}) having data
          ##of either .dbf or .mdb file extensions.
    dt.nm = 'PCMayores', ##<< \code{character}. Name of a data set
                         ##stored in the imported NFI data. Default
                         ##reads \code{'PCMayores'} (3rd NFI) or
=======
readNFI <- structure(function#Read SNF data from path
### This function can read compressed data (\code{.zip}) from the 
### Spanish National Forest Inventory (SNF). It can process either 
### \code{URLs} to data stored on the SNF web page 
### (\code{"http://www.miteco.gob.es"}) or paths to locally stored files.
                     ## details<< Compressed data files with
                     ## extensions other than \code{.dbf} \code{.mdb}
                     ## (Linux only), or \code{.accdb} are not
                     ## supported.  Most databases in the 2nd and 3rd
                     ## stages of the SNF can be imported directly
                     ## from \code{http://www.miteco.gob.es} using
                     ## appropriate URLs.  Data sets from the 2nd
                     ## stage of SNF are imported using
                     ## \code{\link{read.dbf}}. Data from later stages
                     ## are imported using either \code{\link{RODBC}}
                     ## (Windows) or \code{\link{mdb.get}} (Unix-like
                     ## systems).  On Windows, install the Office
                     ## driver via \code{'AccessDatabaseEngine.exe'}
                     ## from Microsoft.  On Unix-like systems, install
                     ## the \code{mdbtools} dependency.
(                                                                                                                                                       
    nfi,  ##<< \code{character}. URL or local path to a compressed
          ##file (\code{.zip}) containing SNF data, or to a
          ##decompressed file with these supported extensions.
    nfi.nr = 4,
    dt.nm = 'PCMayores', ##<< \code{character}. Name of a dataset
                         ##stored in the imported NFI data. Defaults
                         ##to \code{'PCMayores'} (3rd NFI) or
>>>>>>> basifoR_0.7.1
                         ##\code{'PIESMA'} (2nd NFI).
    ... ##<< Additional arguments in \code{\link{urlToTemp}}.
    
) {
<<<<<<< HEAD
    imp <- urlToTemp(nfi, ...)
    fwin <- function(x, dt.nm){
        ife <- RODBC::odbcConnectAccess(x) 
=======
    imp <- nfi


## find_code__ <- function(input_value, is.ifn4, df) {
##   result <- df$codigo[
##     grepl(input_value, ignore.case = TRUE, df$codigo) | 
##     grepl(input_value, ignore.case = TRUE, df$provincia) | 
##     grepl(input_value, ignore.case = TRUE, df$codigo2) |
##     grepl(input_value, ignore.case = TRUE, df$provincia_0) |
##     grepl(input_value, ignore.case = TRUE, df$provincia_1)
##     ][1L]
  
##   if(is.ifn4){
##       result <- df$provincia_1[
##                        grepl(paste0('^',result,'$'), df$codigo,
##                              ignore.case = TRUE)]}
##   if(length(result) == 0)
##       result <- NA
##   return(result)
## }

    is.ifn4 <- nfi.nr == 4

    is_zip_path <- function(x) {
        is.character(x) &&
            length(x) == 1L &&
            !is.na(x) &&
            tolower(tools::file_ext(x)) == "zip"
    }
    
    ## code_match <- NA_character_
    ## if (is.character(imp) && length(imp) == 1L) {
        code_match <- find_code__(imp, is.ifn4 = is.ifn4, df = procods)
        pr. <- find_code__(imp, FALSE, df = procods)
    ## }

    
    if (!is.na(code_match) && !is_zip_path(imp)) {
        nfi. <- paste0("nfi", nfi.nr)
        imp <- do.call(nfi., list(prov = imp))
        imp <- fetchNFI(imp, ...)
    } else if (is_zip_path(imp)) {
        imp <- fetchNFI(imp, ...)
        nfi.nr  <- get_ifn_nr(imp)
    }

  if(is.null (imp))
        return(imp)
    fwin <- function(x, dt.nm){
        ife <-RODBC::odbcConnectAccess2007(x, rows_at_time = 1)
>>>>>>> basifoR_0.7.1
        on.exit(odbcClose(ife))
        ifc <- Map(function(x)
            sqlFetch(ife, sqtable = x), dt.nm)
        return(ifc)
    }
    fmdb <- function(x,dt.nm){
        ## mdb.get(x,tables = dt.nm)
        tryCatch(mdb.get(x,tables = dt.nm),
                 error = function(e) NULL)
    }
    fdbf <- function(x,dt.nm){
        x <- x[grepl(dt.nm, x)]
        read.dbf(x)
    }
    is_dbf <- all(grepl('.DBF',imp))
    is_mdb <- all(grepl('.mdb',imp))
    is_win <- Sys.info()['sysname']%in%'Windows'
<<<<<<< HEAD
    is_i386 <- grepl('i386',R.Version()['system'])
    if(is_mdb){
        if(is_win & !is_i386){
            print('Access driver needed: change to R i386!')
            return(NULL)
        }
        if(is_win & is_i386){
=======

    if(is_mdb){
        if(is_win){
>>>>>>> basifoR_0.7.1
            fnim <- 'fwin'
        } else {
            fnim <- 'fmdb'
        }
    }
    if(is_dbf){
        fnim <- 'fdbf'
    }
    dt.nm. <- dt.nm
    may. <- grepl('mayores',dt.nm, ignore.case = TRUE)
    may2. <- grepl('dbf', fnim)
    if(may. & !may2.){
        dt.nm. <- unique(c(dt.nm,'PCDatosMap')) 
    }
    if(may2. & dt.nm%in%'PCMayores')
        dt.nm. <- 'PIESMA'    
    dset <- tryCatch(do.call(fnim, list(imp, dt.nm.)),
                     error = function(e) NULL)
    if(is.null(dset))
        return(dset)
    if(!may. & !dt.nm.[1]%in%'PIESMA')
        return(dset)
    if(may. & !may2.){
        pr. <- unique(dset$'PCDatosMap'$'Provincia') 
        dset <- dset[[dt.nm]]
    }
    if(dt.nm.[1]%in%'PIESMA')
        pr. <- unique(dset$'PROVINCIA')
    attributes(dset) <- c(attributes(dset), list(pr. = pr.))
<<<<<<< HEAD
        class(dset) <- append('readNFI',class(dset))
=======
    attr(dset, "nfi.nr") <- nfi.nr
    if('provincia'%in%tolower(names(dset))){
        dset <- data.frame(nfi.nr = nfi.nr, dset)
        names(dset)[tolower(names(dset)) == "provincia"] <- "pr"
    } else{
    dset <- data.frame(nfi.nr = nfi.nr, pr = pr., dset)}
    class(dset) <- append('readNFI',class(dset))
>>>>>>> basifoR_0.7.1
    return(dset)
### \code{data.frame}. A data base  of the NFI.
}, ex = function(){
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
head(rmad)

## Retrieval of a data base from the second stage of the second SNFI:

## donttest{
## path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
## url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
## rnfi <- readNFI(url2)
## head(rnfi,3)
## }


})
