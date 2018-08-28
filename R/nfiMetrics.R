nfiMetrics <- structure(function#Tree metrics from NFI data
### This function recursively implements \code{\link{dbhMetric}} on
### NFI data, deriving metrics required to compute over bark volumes,
### according to units/parameters of the Spanish National Forest
### Inventory (NFI). Metric units of the outputs are described in
### the Value section.  Use \code{\link{metrics2Vol}} to directly derive
### over bark volumes.
(
        dbh,  ##<<\code{character} or \code{data.frame}.  URL/path to
              ##a compressed file of the NFI (.zip) having data of
              ##either .dbf or .mdb file extensions, or a data frame
              ##such as that produced by \code{\link{readNFI}}.
    var = c('pr','d','h','ba','n'), ##<<\code{character}. Variables to
                                    ##be derived. These can be five:
                                    ##The provincial unit of the data
                                    ##set \code{'pr'}, the mean
                                    ##diameters \code{'d'}, the tree
                                    ##heights \code{'h'}, the number
                                    ##of trees per hectare \code{'n'},
                                    ##and the basal areas \code{'ba'},
                                    ##see Details section in
                                    ##\code{\link{dbhMetric}} for
                                    ##better understanding of the
                                    ##metrics units. Default
                                    ##\code{c('pr','d','h','ba','n')}.
    append = c('esta','espe') ##<<\code{character}. Vector of strings
                              ##matching names of columns in
                              ##\code{dbh} to be appended to the
                              ##output. Cases are ignored. Default
                              ##\code{c('esta','espe')} matchs columns
                              ##of codes of sample units
                              ##\code{'Estadillos'} and tree species
                              ##\code{'Especie'}.
) {
        if(is.character(dbh)){
        dbh <- readNFI(dbh)
    }

    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)}

    fdn <- function(dbh, var){
        if(var%in%c('d','n','ba'))
            dm <- apply(dbh[,fc(dbh,c('Dn','Diamet'))],1,
                        function(x)dbhMetric(x,var))
        if(var%in%'h'){
            ht <- fc(dbh,c('altura','Ht'))
            dbh[,ht] <- as.numeric(as.character(dbh[,ht]))
            dm <- conv_unit(dbh[,ht],
                            from = 'm', to = 'dm')}
        if(var%in%'pr'){
            dm <- rep(attr(dbh,'pr.'), nrow(dbh))
        }
        return(dm)}
    dmt <- mapply(function(y)
        fdn(dbh,y), y = var)
    nma <- names(dbh)
    app <- paste(append, collapse = '|')
    gap <- grepl(app,nma, ignore.case = TRUE)
    nms <- nma[gap]

    dmt <- data.frame(dbh[,nms], dmt)
    return(dmt)
### \code{data.frame} containing the columns in \code{append}, plus
### the variables in \code{var}: the province \code{pr},
### (\code{dimensionless}), the diameter \code{d} (\code{'mm'}), the
### tree height \code{h} (\code{'dm'}), the basal area
### (\code{ba},\code{'m2 tree-1'}), and the number of trees by hectare
### (\code{n}, \code{dimensionless}).
}, ex = function(){
## seconf NFI
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="Rbasifor")
rmad <- readNFI(madridNFI)[1:10,]
head(rmad,3)
})
