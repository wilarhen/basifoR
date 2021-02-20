readNFI <- structure(function#Read NFI data
### This function can retrieve data sets of the
### Spanish National Forest Inventory (NFI) using either remote or
### local paths to compressed (.zip) files.
                      ##details<< Compressed files having data
                      ##extensions other than .dbf or .mdb are not
                      ##supported. Some data sets of the 2nd NFI as
                      ##well ass most of the data in the 3rd NFI can
                      ##be imported directly from
                      ##\href{https://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/cartografia_informacion_disp.aspx}{http://mapama.gob.es}
                      ##using links to the compressed files. The
                      ##compressed files of the 4th NFI must be read
                      ##from local paths. The \code{.dbf} formats in
                      ##compressed data of the second NFI are imported
                      ##using \code{\link{read.dbf}}. The .mdb formats
                      ##in latter NFIs are imported using either
                      ##\code{\link{RODBC}} (Windows) or
                      ##\code{\link{mdb.get}} (unix-alike systems). In
                      ##the former case, a 32-bit access driver should
                      ##be installed in the system, and the package
                      ##should be implemented using a 32-bit R
                      ##version. In the case of unix-alike systems the
                      ##package mdb-tools should be installed.
(
        url,  ##<<\code{character} or \code{data.frame}.  URL/path to
              ##a compressed file of the NFI (.zip) having data of
              ##either .dbf or .mdb file extensions.
    dt.nm = 'PCMayores' ##<< \code{character}. Name of a data set
                        ##stored in the imported NFI data. Default
                        ##reads \code{'PCMayores'} (3rd NFI) or
                        ##\code{'PIESMA'} (2nd NFI).
    
) {
    imp <- urlToTemp(url)
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
            stop('Access driver: R system is not i386')
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
