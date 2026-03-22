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
                               ##and tree species \code{'Especie'}.
    design = snfi_design(),
    ... ##<< Additional arguments in \code{\link{readNFI}}.

) {
    nfi. <- nfi
    if(is.null(nfi.))
        return(nfi)

    if(!inherits(nfi., "readNFI"))
        nfi <- readNFI(nfi, ...)

    nfi_nr <- attr(nfi, "nfi.nr")

    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)
    }

    var. <- var[!var %in% 'Hd']

    diam_cols <- character(0)
    ht_cols <- character(0)

    if(any(var. %in% c('d', 'n', 'ba')))
        diam_cols <- fc(nfi, c('Dn', 'Diamet'))

    if(any(var. %in% 'h'))
        ht_cols <- fc(nfi, c('altura', 'Ht'))

    get_numeric_matrix <- function(dt, cols) {
        if(length(cols) == 0L)
            return(NULL)

        x <- dt[, cols, drop = FALSE]
        x <- lapply(x, function(z) as.numeric(as.character(z)))
        x <- as.data.frame(x, check.names = FALSE,
                           stringsAsFactors = FALSE)
        as.matrix(x)
    }

    trees_per_ha_vec <- function(dbh_cm, design) {

        if (inherits(design, "concentric_design")) {
            out <- rep(NA_real_, length(dbh_cm))
            ok <- !is.na(dbh_cm) & dbh_cm >= design$min_dbh_cm[1]
            if (any(ok)) {
                idx <- findInterval(dbh_cm[ok], design$min_dbh_cm)
                out[ok] <- design$sf[idx]
            }
            return(out)
        }

        vapply(dbh_cm,
               function(x) trees_per_ha(design = design, dbh_cm = x),
               numeric(1))
    }
    
    diam_mm <- NULL
    diam_cm <- NULL
    trees_ha <- NULL
    ht_dm <- NULL
    

    if(length(diam_cols) > 0L) {
        diam_mat <- get_numeric_matrix(nfi, diam_cols)
        diam_mat[diam_mat == 0] <- NA_real_

        if(ncol(diam_mat) == 1L) {
            diam_mm <- diam_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(diam_mat))
            diam_mm <- rowMeans(diam_mat, na.rm = TRUE)
            diam_mm[nn == 0L] <- NA_real_
        }

        if(any(var. %in% c('ba', 'n')))
            diam_cm <- conv_unit(diam_mm, from = 'mm', to = 'cm')

        if(any(var. %in% 'n'))
            trees_ha <- trees_per_ha_vec(diam_cm, design)
    }

    if(length(ht_cols) > 0L) {
        ht_mat <- get_numeric_matrix(nfi, ht_cols)
        ht_mat[ht_mat == 0] <- NA_real_

        if(ncol(ht_mat) == 1L) {
            ht_m <- ht_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(ht_mat))
            ht_m <- rowMeans(ht_mat, na.rm = TRUE)
            ht_m[nn == 0L] <- NA_real_
        }

        ht_dm <- conv_unit(ht_m, from = 'm', to = 'dm')
    }
    
    fdn <- function(dbh, var){
        if(var %in% 'd') {
            if(length(diam_cols) > 0L)
                return(diam_mm)
            return(apply(dbh[, fc(dbh, c('Dn', 'Diamet')), drop = FALSE], 1,
                         function(x) dbhMetric(x, var)))
        }

        if(var %in% 'ba') {
            if(length(diam_cols) > 0L)
                return(pi * diam_cm^2 * (4 * 1E4)^-1)
            return(apply(dbh[, fc(dbh, c('Dn', 'Diamet')), drop = FALSE], 1,
                         function(x) dbhMetric(x, var)))
        }

        if(var %in% 'n') {
            if(length(diam_cols) > 0L)
                return(trees_ha)
            return(apply(dbh[, fc(dbh, c('Dn', 'Diamet')), drop = FALSE], 1,
                         function(x) dbhMetric(x, var, design = design)))
        }

        if(var %in% 'h') {
            if(length(ht_cols) > 0L)
                return(ht_dm)
            return(apply(dbh[, fc(dbh, c('altura', 'Ht')), drop = FALSE], 1,
                         function(x) dbhMetric(x, var, design = design)))
        }
    }

    metric_list <- vector('list', length(var.))
    if(length(var.) > 0L) {
        for(i in seq_along(var.))
            metric_list[[i]] <- fdn(nfi, var.[i])
    }

    dmt <- data.frame(metric_list, check.names = FALSE,
                      stringsAsFactors = FALSE)
    if(length(var.) > 0L)
        names(dmt) <- var.

    if(!is.null(attr(nfi, 'pr.')))
        dmt <- cbind(pr = attr(nfi, 'pr.'), dmt)

    nm_all <- names(nfi)

    match_cols <- function(want, nm_all) {
        out <- nm_all[tolower(nm_all) %in% tolower(want)]
        unique(out)
    }

    id_cols <- match_cols(c('nfi.nr', 'pr'), nm_all)

    nms_raw <- flev(nfi, levels)
    nms_raw <- nms_raw[!is.na(nms_raw)]
    nms <- match_cols(nms_raw, nm_all)

    keep_cols <- unique(c(id_cols, nms))
    keep_cols <- keep_cols[keep_cols %in% nm_all]

    if(length(keep_cols) == 0L) {
        dmt <- data.frame(dmt, check.names = FALSE)
    } else {
        dmt <- data.frame(nfi[, keep_cols, drop = FALSE], dmt,
                          check.names = FALSE)
    }

    if('Hd' %in% var) {
        needed <- c('h', 'd', 'n')
        nd <- paste(needed, collapse = '?,')
        if(!all(needed %in% var))
            stop(paste0('Hd: missing variables: var = c(', nd, '?, ...)'))
        spl <- split(dmt, dmt[, nms], drop = TRUE)
        dmhe <- Map(function(y)
            cbind(y, Hd = tryCatch(domheight(y$'h', y$'d', y$'n'),
                                   error = function(e) NA)), spl)
        dmt <- do.call('rbind', dmhe)
        rownames(dmt) <- NULL
    }

    attr(dmt, 'nfi.nr') <- nfi_nr
    dmt <- conv_units(dmt)

    class(dmt) <- append('nfiMetrics', class(dmt))
    return(dmt)

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
