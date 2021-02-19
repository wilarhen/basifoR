metrics2Vol <- structure(function#Tree volumes in NFI data
### This function computes over bark volumes (\code{dm3}) by deriving
### tree metrics from NFI data and matching the metrics with volume
### parameters established in 2nd NFI. To derive dendrometric
### summaries use \code{\link{dendroMetrics}}.
                         ##details<< The volumes are computed deriving
                         ##metrics with \code{\link{nfiMetrics}} and
                         ##matching these to volume parameters
                         ##developed in second NFI. The data are
                         ##matched using two factors: the provincial
                         ##unit (\code{'pr'}) and the tree species
                         ##(\code{'Especie'}). The functions supports
                         ##parameters in two models: \code{'v ~ par1 +
                         ##par2 * (d^2) * h'}, and \code{v ~ par1 *
                         ##(d^par2) * (h^par3)}.
(
    dbm,  ##<<\code{character} or \code{data.frame}.  URL/path to a
          ##compressed file of the NFI (.zip) having data of either
          ##.dbf or .mdb file extensions, or a data frame such as that
          ##produced by \code{\link{nfiMetrics}}.
    fc. = 'freq', ##<< \code{character}. A Cubication form. Default
                  ##\code{'freq'} implements the most frequent form
                  ##matching the data.
    all.col = FALSE, ##<< \code{logical}. Maintain the columns used to
                     ##compute the volumes. Default \code{FALSE}.
    ... ##<<Additional arguments in \code{\link{nfiMetrics}}
) {
    if(is.character(dbm)){
        dbm <- na.omit(nfiMetrics(dbm, ...))
    }
        
    mds <- c('1'  = 'v ~ par1 + par2 * (d^2) * h',
             '11' = 'v ~ par1 * (d^par2) * (h^par3)')
    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)}
    fmdV <- function(mdb2, ntm = c('pr','espe')){
        ## data(parEqVcc, envir = environment())
        load('parEqVcc.RData')
        vt <- merge(mdb2, parEqVcc,
                    by.x = fc(mdb2, ntm),
                    by.y = fc(parEqVcc, ntm),
                    all.x = TRUE)
        return(vt)}
    feV <- function(vt, md){
        fvarin <- function(fun,ind = TRUE){
            fun <- formula(fun)
            allv <- all.vars(fun,
                             functions = FALSE) 
            yvar <- all.vars(update(fun, . ~ 1),
                             functions = FALSE)
            inds <- allv[!allv%in%yvar]
            if(!ind)inds <- yvar
            return(inds)}
        fev <- function(fun, md){
            e <- list2env(as.list(md))
            y <- eval(parse(text=fun), e)
            return(y)}
        ind <- fvarin(md)
        dep <- fvarin(md, F)
        sbs <- paste(dep,'~', sep = '|') 
        md. <- gsub(sbs,'',md)
        md. <- gsub(' ','',md.)
        vt. <- vt[,ind]
        vl <- apply(vt.,1,function(x)fev(md.,x))
        vl <- cbind(vt, vl)
        names(vl) <- c(names(vt),dep)
        return(vl)}
    vt <- fmdV(dbm)
    lvs <- levels(as.factor(vt$'Modelo'))
    spm <- split(vt, vt[,'Modelo'])
    nms. <- names(spm)
    mds. <- mds[nms.]
    mmod <- Map(function(x,y)
        feV(x,y),x = spm, y = mds.)
    mmd <- do.call('rbind', mmod)
    tex <- fc(mmd,c('mod','par')) 
    if(!all.col)
        mmd <- mmd[,!names(mmd)%in%tex]
    ffreq <- function(df){
        tm <- data.frame(table(df$'fc'))
        tm <- subset(tm,get('Freq')%in%max(get('Freq')))
        tm <- as.character(tm$'Var1')[1]
        return(tm)    
    }
    if(fc.%in%'freq')
        fc. <- ffreq(mmd)
    mmd <- subset(mmd, fc%in%as.factor(fc.))
    if(!all.col)
        mmd <- mmd[,!names(mmd)%in%'fc']
    rownames(mmd) <- NULL
    return(mmd)
### \code{data.frame}. Either short or expanded data, depending on the
### \code{all.col} argument.  The short data contains the volumes
### (\code{v}, \code{'dm3'}) plus the tree metrics defined in
### \code{\link{nfiMetrics}}, see value of such a function to better
### understand the metric units. The expanded data contains additional
### columns used to compute the volumes.
}, ex = function(){
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    rmad <- readNFI(madridNFI)[1:100,]
    mmad <- nfiMetrics(rmad)
    vmad <- metrics2Vol(mmad)
    head(vmad)
})
