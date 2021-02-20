metrics2Vol <- structure(function#Tree volumes in NFI data
### This function computes over bark volumes (\code{dm3}) by deriving
### tree metrics from NFI data and matching the metrics with volume
### parameters established in 2nd NFI. To derive dendrometric
### summaries use \code{\link{dendroMetrics}}.
                         ##details<< The volumes are computed using
                         ##parameters derived in the second NFI. The
                         ##functions used have the forms \code{'v ~
                         ##par1 + par2 * (d^2) * h'}, and \code{'v ~
                         ##par1 * (d^par2) * (h^par3)'}. Parameter of
                         ##both functions depend on provincial units,
                         ##tree species, diameters, and tree
                         ##heights. Consequently, objects from
                         ##\code{\link{nfiMetrics}} must incorporate
                         ##such variables.
(
    dbm,  ##<<\code{character} or \code{data.frame}.  URL/path to a
          ##compressed file of the NFI (.zip) having data of either
          ##.dbf or .mdb file extensions; or data frame such as that
          ##produced by \code{\link{nfiMetrics}}; or data frame such
          ##as that produced by \code{\link{readNFI}}.
    fc. = 'freq', ##<< \code{character}. A Cubication form. Default
                  ##\code{'freq'} implements the most frequent form
                  ##matching the data.
    keep.var = FALSE, ##<< \code{logical}. Maintain the columns used to
                     ##compute the volumes. Default \code{FALSE}.
    ... ##<< Additional arguments in \code{\link{metrics2Vol}} or
        ##\code{\link{nfiMetrics}} or \code{\link{readNFI}}.
) {
    if(is.character(dbm) | inherits(dbm, 'readNFI')){
        dbm <- na.omit(nfiMetrics(dbm, ...))
    }
spec. <- names(dbm)[grepl('spec', names(dbm), ignore.case = TRUE)]
var <- c('pr','h','d')
needed <- c('Especie/ESPECIE', var)
nd <- paste(needed, collapse = '?,')
    if(!all(length(spec.) != 0 & var%in%names(dbm))){
        warning("nfiMetrics: change arguments 'var'and/or 'levels'")
    stop(paste0('v: missing variables: dbm[,c(',nd,'?, ...)]'))
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
    fmdV <- function(mdb2, ntm = c('pr','spec')){
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
    if(!keep.var)
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
    if(!keep.var)
        mmd <- mmd[,!names(mmd)%in%'fc']
    rownames(mmd) <- NULL
    return(mmd)
### \code{data.frame}. Either short or expanded data, depending on the
### \code{keep.var} argument.  The short data contains the volumes
### (\code{v}, \code{'dm3'}) plus the tree metrics defined in
### \code{\link{nfiMetrics}}, see value of such a function to better
### understand the metric units. The expanded data contains additional
### columns used to compute the volumes.
}, ex = function(){
    madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
    rmad <- readNFI(madridNFI)[1:10,]
    vmad <- metrics2Vol(rmad)
    head(vmad)
})
