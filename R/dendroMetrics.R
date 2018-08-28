dendroMetrics <- structure(function#Summarize dendrometrics
### This function can control most of the in-package routines. The
### function computes and summarizes dendrometrics using data of the
### Spanish National Forest Inventory (NFI). This also transforms the
### metric units of the summaries, see details.
                           ##details<< Units of measurement of the
                           ##derived dendrometrics are transformed to
                           ##stand units and the data is summarized
                           ##using levels in
                           ##\code{summ.vr}. Consequently the metric
                           ##units of the summarized variables do not
                           ##correspond to the metric units of data
                           ##from \code{\link{metrics2Vol}}.  The
                           ##summaries contain following variables: 1)
                           ##three factor columns: the province
                           ##(\code{pr}), the tree species
                           ##(\code{'ESPECIE'} or \code{'Especie'},
                           ##depending on whether the data belongs to
                           ##2nd or 3rd NFIs), the sample units
                           ##(\code{'ESTADILLO'} or
                           ##\code{'Estadillo'}); 2) the tree basal
                           ##area (\code{ba}, \code{'m2 ha-1'}); 3)
                           ##the average diameter (\code{d},
                           ##\code{'cm'}); 4) the quadratic mean
                           ##diameter (\code{dg}, \code{'cm'}); 5) The
                           ##average tree height (\code{h},
                           ##\code{'m'}); 6) the number of trees by
                           ##hectare (\code{n}, dimensionless), and
                           ##the over bark volume (\code{v}, \code{'m3
                           ##ha-1'}). Subsets of the summaries are
                           ##computed using logical expressions in
                           ##\code{cut.dt}, see syntax in
                           ##\code{\link{Logic}}.
(
         mmd,  ##<<\code{character} or \code{data.frame}.  URL/path to
              ##a compressed file of the NFI (.zip), having data of
              ##either .dbf or .mdb file extensions, or a data frame
              ##such as that produced by \code{\link{metrics2Vol}}.
    summ.vr = 'Estadillo', ##<< \code{character} or \code{NULL}. Name
                           ##of a Column used to summarize the
                           ##outputs. If \code{NULL} then output from
                           ##\code{\link{metrics2Vol}} is
                           ##returned. Default \code{'Estadillo'}
    cut.dt = 'd == d', ##<< \code{character}. Logical condition used
                       ##to subset the output. Default \code{'d == d'}
                       ##avoids subsetting.
    report = FALSE, ##<< \code{logical}. Print a report of the output
                    ##in the current working directory.
    ... ##<< Additional arguments in \code{\link{metrics2Vol}}.
) {
    if(is.character(mmd)){
        mmd <- metrics2Vol(mmd, ...)
    }
    if(is.null(summ.vr)){
        mmd <- subset(mmd,
                      eval(parse(text = cut.dt)))
        if(report)
            write.csv(mmd, file = 'report.csv')
        return(mmd)
    }
    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)}
    summ.vr <- fc(mmd, summ.vr)
    msp <- split(mmd, mmd[summ.vr])
    msp <- Filter(nrow, msp)
    fsun <- function(dt){
    un. <- data.frame(
        var = c('d','h','ba','v'),
        frm.= c('mm', 'dm', 'm2','dm3'), 
        to. = c('cm','m', 'm2','m3'))
    un.. <- subset(
        un., get('var')%in%names(dt))
    un.. <- lapply(un.., as.character)
    unv <- un..$'var'
    cols <- lapply(seq_len(ncol(dt[,unv])),
                   function(x)dt[,unv][x])
    ncu <- mapply(function(x,y,z)
        conv_unit(x, from = y, to = z),
        x = cols, y = un..$'frm.', z = un..$'to.')
    if(!is.matrix(ncu))
        nun <- do.call('cbind', ncu)
    dt[,unv] <- nun
    dt[,unv] <- dt[,unv] * dt[,'n'] 
    summ <- apply(dt[,c(unv,'n')], 2,
                  sum, na.rm = TRUE)
    summ[c('d','h')] <- summ[c('d','h')]/summ['n'] 
    summ['dg'] <- sqrt((4E4 * summ['ba']/summ['n'])/pi)
    summ <- summ[order(names(summ))]
    summ <- sapply(summ,function(x) round(x,3))
    summ <- t(as.matrix(summ))
    fcs. <- !names(dt)%in%c(unv,'n')
    fcs <- dt[1,fcs.]
    resd <- cbind(fcs, summ)
    return(resd)}
    resm <- Map(function(x)
        fsun(x), x= msp)
    resm <- Reduce('rbind',resm)
    resm <- data.frame(resm)
    resm <- subset(resm,
                   eval(parse(text = cut.dt)))
    if(report)
        write.csv(resm, file = 'report.csv')
    return(resm)
### \code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
### \code{\link{metrics2Vol}}, or a summary of the variables, see
### Details section.
}, ex = function(){
    ## Local NFI Data from the province of Madrid
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="Rbasifor")
    rmad <- readNFI(madridNFI)[1:100,]
    mmad <- nfiMetrics(rmad)
    vmad <- metrics2Vol(mmad)
    dmad <- dendroMetrics(vmad, cut.dt = 'h > 8')
    head(dmad)


})
