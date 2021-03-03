dendroMetrics <- structure(function#Summarize dendrometrics

###This function can summarize dendrometric data from the Spanish
###National Forest Inventory (SNFI). It can also control most other
###routines of the package. Outputs are formated into stand
###units, see the Details section.
                           ##details<< Outputs can be summarized using
                           ## levels in \code{summ.vr}. The outputs
                           ## include: 1) Categorical columns
                           ## formulated in argument \code{summ.vr};
                           ## 2) the tree basal area (\code{ba},
                           ## \code{'m2 ha-1'}); 3) the average
                           ## diameter (\code{d}, \code{'cm'}); 4) the
                           ## quadratic mean diameter (\code{dg},
                           ## \code{'cm'}); 5) The average tree height
                           ## (\code{h}, \code{'m'}); 6) the number of
                           ## trees by hectare (\code{n},
                           ## 'dimensionless'), and the over bark
                           ## volume (\code{v}, \code{'m3 ha-1'}). The
                           ## output summary can be subsetted using
                           ## logical expressions in argument
                           ## \code{'cut.dt'}, see syntax in
                           ## \code{\link{Logic}}.
(
    nfi, ##<<\code{character} or \code{data.frame}.  URL/path to a
          ##compressed SNFI file (.zip) having data of either
          ##.dbf or .mdb file extensions; or data frame such as that
          ##produced by \code{\link{nfiMetrics}}; or data frame such
          ##as that produced by \code{\link{readNFI}}.
    summ.vr = 'Estadillo', ##<< \code{character} or \code{NULL}. Name
                           ##of a Categorical variables in the SNFI
                           ##data used to summarize the outputs. If
                           ##\code{NULL} then output from
                           ##\code{\link{metrics2Vol}} is
                           ##returned. Default \code{'Estadillo'}
                           ##processes sample plots.
    cut.dt = 'd == d', ##<< \code{character}. Logical condition used
                       ##to subset the output. Default \code{'d == d'}
                       ##avoids subsetting.
    report = FALSE, ##<< \code{logical}. Print a report of the output
                    ##in the current working directory.
    ... ##<< Additional arguments in \code{\link{metrics2Vol}} or
        ##\code{\link{nfiMetrics}} or \code{\link{readNFI}}.
) {
    if(is.null(nfi) | is.character(nfi) | inherits(nfi, 'readNFI')){
        nfi. <- nfi
        nfi <- metrics2Vol(nfi, levels = ...)
        if(is.null(nfi.))
            return(nfi)
    }
    frm. <- names(attr(nfi, 'units'))
    if(is.null(summ.vr)){
        nfi <- subset(nfi,
                      eval(parse(text = cut.dt)))
        attributes(nfi) <- c(attributes(nfi), list(units = frm.))
    
        if(report)
            write.csv(nfi, file = 'report.csv', row.names = FALSE)
        return(nfi)
    }
    ## fc <- function(dt, cl.){
    ##     nt. <- paste(cl., collapse = '|')
    ##     nt.. <- grep(nt., names(dt),
    ##                  ignore.case = TRUE)
    ##     cl.nm <- sort(names(dt)[nt..],
    ##                   decreasing = TRUE)
    ##     return(cl.nm)}
    ## summ.vr <- fc(nfi, summ.vr)
        summ.vr <- flev(nfi, summ.vr)

    var <- getOption('units1')[getOption('units1')%in%names(nfi)]
    frm. <- names(attr(nfi, 'units'))
    to. <- names(var)
    var. <- var[var!='n']
    nfi <- conv_units(nfi, var = var, un = to.)

    msp <- split(nfi, nfi[summ.vr])
    msp <- Filter('nrow', msp)


    fsum <- function(dt){
        dt[,var.] <- dt[,var.] * dt[,'n'] 
        summ <- apply(dt[,var], 2,
                      sum, na.rm = TRUE)
        ## summ['Hd'] <- domheight(summ['h'],summ['d'],summ['n'])
        summ[c('d','h', 'Hd')] <- summ[c('d','h','Hd')]/summ['n'] 
        summ['dg'] <- sqrt((4E4 * summ['ba']/summ['n'])/pi)
        summ <- summ[order(names(summ))]
        summ <- sapply(summ,function(x) round(x,3))
        summ <- t(as.matrix(summ))
        fcs. <- names(dt)[!names(dt)%in%var]
        fcs <- dt[1,fcs.]
        resd <- cbind(fcs, summ)}

    resm <- Map(function(x)
        fsum(x), x= msp)
    resm <- Reduce('rbind',resm)
    resm <- data.frame(resm)
    resm <- subset(resm,
                   eval(parse(text = cut.dt)))
    rownames(resm) <- NULL
    if(report)
        write.csv(resm, file = 'report.csv', row.names = FALSE)

    ## dgcm <- 'dg'
    ## names(dgcm) <- 'cm'
    attr. <- c(names(attr(nfi,'units')), 'cm')
    attributes(resm) <- c(attributes(resm), list(units = attr.))

    return(resm)
### \code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
### \code{\link{metrics2Vol}}, or a summary of the variables, see
### Details section.
}, ex = function(){
    ## Local NFI Data from the province of Madrid
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    rmad <- readNFI(madridNFI)[1:100,]
    mmad <- nfiMetrics(rmad)
    vmad <- metrics2Vol(mmad)
    dmad <- dendroMetrics(vmad, cut.dt = 'h > 8')
    head(dmad)
## see SI units
attr(dmad,'units')

    
})
