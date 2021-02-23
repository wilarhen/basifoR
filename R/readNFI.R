readNFI <- structure(function#Read NFI data
### This function can retrieve data sets in the Spanish National
### Forest Inventory (NFI); this processes either remote URLs or local
### file paths to compressed (.zip) NFI files.
                      ##details<< Compressed data having file
                      ##extensions other than \code{.dbf} or
                      ##\code{.mdb} are not supported. Most data in
                      ##2nd and 3rd NFIs can be imported directly from
                      ##\code{http://mapama.gob.es}
                      ##using links to the compressed files. Data in
                      ##the 4th NFI must be read from local
                      ##paths. Data in second NFI are imported using
                      ##\code{\link{read.dbf}}. Data in latter NFIs
                      ##are imported using either \code{\link{RODBC}}
                      ##(Windows) or \code{\link{mdb.get}} (unix-alike
                      ##systems). In the former case, a 32-bit access
                      ##driver should be installed in the system, and
                      ##the package must be implemented using a 32-bit
                      ##R version. In the case of unix-alike systems,
                      ##dependence \code{mdb-tools} must be installed.
(
        nfi,  ##<<\code{character} or \code{data.frame}.  URL/path to
              ##a compressed file of the NFI (.zip) having data of
              ##either .dbf or .mdb file extensions.
    dt.nm = 'PCMayores' ##<< \code{character}. Name of a data set
                        ##stored in the imported NFI data. Default
                        ##reads \code{'PCMayores'} (3rd NFI) or
                        ##\code{'PIESMA'} (2nd NFI).
    
) {
    imp <- urlToTemp(nfi)
    fwin <- function(x, dt.nm){
        ife <- RODBC::odbcConnectAccess(x) 
        on.exit(odbcClose(ife))
        ifc <- Map(function(x)
            sqlFetch(ife, sqtable = x), dt.nm)
        return(ifc)
    }
    fmdb <- function(x,dt.nm){
        mdb.get(x,tables = dt.nm)
    }
    fdbf <- function(x,dt.nm){
        x <- x[grepl(dt.nm, x)]
        read.dbf(x)
    }
    is_dbf <- all(grepl('.DBF',imp))
    is_mdb <- all(grepl('.mdb',imp))
    is_win <- Sys.info()['sysname']%in%'Windows'
    is_i386 <- grepl('i386',R.Version()['system'])
    if(is_mdb){
        if(is_win & !is_i386){
            print('Access driver: change to R i386!')
            return(NULL)
        }
        if(is_win & is_i386){
            fnim <- 'fwin'
        } else {
            fnim <- 'fmdb'
        }
    }
    if(is_dbf){
        fnim <- 'fdbf'
    }
    ## dset <- tryCatch(do.call(fnim, list(imp, dt.nm)),
    ##                  error = function(e) NULL)
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
    if(!may. & !dt.nm.[1]%in%'PIESMA')
        return(dset)
    if(may. & !may2.){
        pr. <- unique(dset$'PCDatosMap'$'Provincia') 
        dset <- dset[[dt.nm]]
    }
    if(dt.nm.[1]%in%'PIESMA')
        pr. <- unique(dset$'PROVINCIA')
    attributes(dset) <- c(attributes(dset), list(pr. = pr.))
        class(dset) <- append('readNFI',class(dset))
    return(dset)
### \code{data.frame}. A data base  of the NFI.
}, ex = function(){
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
head(rmad)
})
