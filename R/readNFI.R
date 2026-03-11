readNFI <- structure(function#Read SNF data from path
### This function can read compressed data (\code{.zip}) from the 
### Spanish National Forest Inventory (SNF). It can process either 
### \code{URLs} to data stored on the SNF web page 
### (\code{"http://www.miteco.gob.es"}) or paths to locally stored files.
### To read SNF using codes of Spanish provinces, use \code{\link{getNFI}}.
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
                         ##\code{'PIESMA'} (2nd NFI).
    ... ##<< Additional arguments for \code{\link{fetchNFI}}.
) {
    imp <- nfi


find_code__ <- function(input_value, is.ifn4, df) {
  result <- df$codigo[
    grepl(input_value, df$codigo) | 
    grepl(input_value, df$provincia) | 
    grepl(input_value, df$codigo2) |
    grepl(input_value, df$provincia_0) |
    grepl(input_value, df$provincia_1)
    ][1L]
  if(is.ifn4){
      result <- df$provincia_1[
                       grepl(paste0('^',result,'$'), df$codigo,
                             ignore.case = TRUE)]}
  if(length(result) == 0)
      result <- NA
      ## if(is.na(result) & complain){
      ##     warning(paste0("Spanish province '", input_value, "' not found!\n"),
      ##             call. = FALSE)
      ##   return(invisible(NULL))}
  ## }
  # Return the result
  return(result)
}

    is.ifn4 <- nfi.nr == 4 
    ## if(!is.na(find_code_(imp, is.ifn4 = is.ifn4, df = procods, complain = FALSE)) && !file_exten(imp) == 'zip'){
    if(!is.na(find_code__(imp, is.ifn4 = is.ifn4, df = procods)) && !file_exten(imp) == 'zip'){
    nfi. <- paste0('nfi',nfi.nr)
    ## isifn4  <- nfi.nr == 4
    ## if(is.na(find_code_(provincia, is.ifn4 = isifn4, df = procods, complain = FALSE))){
    ##     return(provincia)
    ## }else{
    imp <- do.call(nfi., list(prov = imp))
        imp <- fetchNFI(imp, ...)
        
    }
    
    if(length(imp)!=0 && file_exten(imp) == 'zip')
        imp <- fetchNFI(nfi, ...)
    if(is.null (imp))
        return(imp)
    fwin <- function(x, dt.nm){
                                        # ife <- RODBC::odbcConnectAccess(x) 
        ife <-RODBC::odbcConnectAccess2007(x, rows_at_time = 1)
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
    ## is_mdb <- all(grepl('.mdb',imp))
    is_mdb <- all(grepl('\\.mdb$|\\.accdb$',imp))
    is_win <- Sys.info()['sysname']%in%'Windows'
                                        # is_i386 <- grepl('i386',R.Version()['system'])
    if(is_mdb){
        if(is_win){
                                        # if(is_win & !is_i386){
                                        #     print('Access driver needed: change to R i386!')
                                        #     return(NULL)
                                        # }
                                        # if(is_win & is_i386){
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
    dset <- convert_factors_to_numeric(dset)
    attributes(dset) <- c(attributes(dset), list(pr. = pr.))
    class(dset) <- append('readNFI',class(dset))
    return(dset)
### \code{data.frame} with numeric columns converted from factors back
### to numeric, while preserving the format of character columns.
}, ex = function(){    
    ## donttest{
    ### Retrieval of a database from the second stage of the SNF using a URL resource
    
    ## ifn2_path <-
    ## '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
    ## ifn2_url <- httr::modify_url("https://www.miteco.gob.es", path
    ## = ifn2_path)
    
    ##read_ifn2 <- readNFI(ifn2_url)

    ##str(read_ifn2) }
    
    
})
