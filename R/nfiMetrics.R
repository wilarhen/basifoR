nfiMetrics <- structure(function#Tree metrics from NFI data
### This function recursively implements \code{\link{dbhMetric}} on
### data bases of the Spanish National Forest Inventory (NFI) to
### derive a variety of tree metrics.  To compute all in-package
### metrics, run function \code{\link{dendroMetrics}}.
(
    nfi,  ##<<\code{character} or \code{data.frame}.  URL/path to a
          ##compressed file of the NFI (.zip) having data of either
          ##.dbf or .mdb file extensions, or a data frame such as that
          ##produced by \code{\link{readNFI}}.
    var = c('d','h','ba','n','Hd'), ##<<\code{character}. Metrics. These
                                    ##can be five: \code{(1)} the mean
                                    ##diameter \code{'d'}; \code{(2)}
                                    ##the tree height \code{'h'};
                                    ##\code{(3)} the basal area
                                    ##\code{'ba'}; \code{(4)} the
                                    ##number of trees per hectare
                                    ##\code{'n'}; and \code{(5)} the
                                    ##dominant height \code{'Hd'}, see
                                    ##Details section in
                                    ##\code{\link{dbhMetric}} for
                                    ##better understanding of the
                                    ##metrics units. Default
                                    ##\code{c('pr','d','h','ba','n','Hd')}.
    levels = c('esta','espe'), ##<<\code{character}. levels at which
                              ##the metrics are computed. Pattern
                              ##matching is supported. Cases are
                              ##ignored. Default
                              ##\code{c('esta','espe')} matches both
                              ##the sample plot \code{'Estadillos'}
                              ##and tree species \code{'Especie'}.,
        ... ##<< Additional arguments in \code{\link{readNFI}}.

) {
        ## if(is.null(nfi)|is.character(nfi)|is.numeric(nfi)){
            nfi. <- nfi
        ## ## nfi <- readNFI(nfi, ...)
        ## nfi <- getNFI(nfi, ...)
        if(is.null(nfi.))
            return(nfi)
            ## }
            
            if(!inherits(nfi., "readNFI"))
                nfi <- readNFI(nfi, ...)
nfi_nr <- attr(nfi, "nfi.nr")
                
    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)}

        var. <- var[!var%in%'Hd']
    fdn <- function(dbh, var){
        if(var%in%c('d','n','ba'))
            dm <- apply(dbh[,fc(dbh,c('Dn','Diamet'))],1,
                        function(x)dbhMetric(x,var))
        if(var%in%'h'){
            ht <- fc(dbh,c('altura','Ht'))
            dm <- as.numeric(as.character(dbh[,ht]))}
        return(dm)}
    
        dmt <- mapply(function(y)
            fdn(nfi,y), y = var.)
        if(!is.null(attr(nfi,'pr.')))
        dmt <- cbind(pr = attr(nfi,'pr.'), dmt)
        ## nms <- flev(nfi, levels)
        ## nm.. <- c(nms, colnames(dmt))
        ## dmt <- data.frame(nfi[,nms], dmt)
        ## names(dmt) <- nm..

nm_all <- names(nfi)

match_cols <- function(want, nm_all) {
    out <- nm_all[tolower(nm_all) %in% tolower(want)]
    unique(out)
}

id_cols <- match_cols(c("nfi.nr", "provincia"), nm_all)

nms_raw <- flev(nfi, levels)
nms_raw <- nms_raw[!is.na(nms_raw)]
nms <- match_cols(nms_raw, nm_all)

keep_cols <- unique(c(id_cols, nms))
keep_cols <- keep_cols[keep_cols %in% nm_all]

if(length(keep_cols) == 0) {
    dmt <- data.frame(dmt, check.names = FALSE)
} else {
    dmt <- data.frame(nfi[, keep_cols, drop = FALSE], dmt, check.names = FALSE)
}

        if('Hd'%in%var){
            needed <- c('h','d','n')
            nd <- paste(needed, collapse = '?,')
            if(!all(needed%in%var))
                stop(paste0('Hd: missing variables: var = c(',nd,'?, ...)'))
            spl <- split(dmt, dmt[,nms], drop = TRUE)
            dmhe <- Map(function(y)
                cbind(y, Hd = tryCatch(domheight(y$'h',y$'d',y$'n'),
                                       error = function(e) NA)), spl)
            dmt <- do.call('rbind', dmhe) 
            rownames(dmt) <- NULL}
        attr(dmt, "nfi.nr") <- nfi_nr

        dmt <- conv_units(dmt)
### \code{data.frame} containing columns which match the strings in
### \code{levels}, plus the variables defined in \code{var}, including
### the province \code{pr} (\code{dimensionless}), the diameter
### \code{d} (\code{'mm'}), the tree height \code{h} (\code{'dm'}),
### the basal area \code{ba} (\code{'m2 tree-1'}), the number of trees
### by hectare \code{n} (\code{dimensionless}), and the tree dominant
### height \code{Hd} (\code{'m'}).
}, ex = function(){
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)

# Read and process the data (first 100 rows)
get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100,]

# Compute some metrics
metrics_ifn4p45 <- nfiMetrics(get_ifn4p45)

# see metric units
    attr(metrics_ifn4p45,'units')
})
