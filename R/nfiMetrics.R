nfiMetrics <- structure(function#Tree-level metrics from Spanish NFI records
### Compute tree-level metrics from Spanish National Forest Inventory
### records by applying \code{\link{dbhMetric}}-style logic to NFI
### tables or to files that \code{\link{readNFI}} can import.  The
### function returns the requested metrics together with the grouping
### columns selected through \code{levels}.  To derive the full set of
### package metrics in one call, see \code{\link{dendroMetrics}}.
(
    nfi,  ##<<\code{character} or \code{data.frame}. Input NFI data.
          ##Supply either \code{(1)} a path or URL to a compressed NFI
          ##archive readable by \code{\link{readNFI}}, or \code{(2)} a
          ##data frame already returned by \code{\link{readNFI}}.
    var = c('d','h','ba','n','Hd'), ##<<\code{character}. Metrics to compute.
                                    ##Supported values are \code{'d'}
                                    ##(diameter), \code{'h'} (height),
                                    ##\code{'ba'} (basal area), \code{'n'}
                                    ##(trees per hectare), and \code{'Hd'}
                                    ##(dominant height).  \code{'Hd'}
                                    ##requires \code{'h'}, \code{'d'}, and
                                    ##\code{'n'} to be present in
                                    ##\code{var}.
    levels = c('esta','espe'), ##<<\code{character}. Grouping columns
                               ##kept in the output.  The function
                               ##supports partial matching, and
                               ##ignores case.  The default usually
                               ##keeps plot- and species-level
                               ##identifiers.
    design = snfi_design(), ##<< Sampling design used when computing
                            ##\code{'n'}. Pass the default
                            ##\code{\link{snfi_design}}, another
                            ##\code{concentric_design}, or any
                            ##\code{inventory_design} supported by
                            ##\code{trees_per_ha()}. The returned
                            ##object stores a \code{design_meta}
                            ##attribute with the design used to derive
                            ##\code{'n'}.
    ... ##<< Additional arguments passed to \code{\link{readNFI}} when
        ##\code{nfi} is not already a \code{readNFI} object.

) {
    ## Return early on NULL input to preserve the previous behaviour.
    nfi. <- nfi
    if(is.null(nfi.))
        return(nfi)

    ## Import the data only when the input is not already a readNFI object.
    if(!inherits(nfi., "readNFI"))
        nfi <- readNFI(nfi, ...)

    nfi_nr <- attr(nfi, "nfi.nr")

    ## Find columns whose names match one or more search patterns.
    fc <- function(dt, cl.){
        nt. <- paste(cl., collapse = '|')
        nt.. <- grep(nt., names(dt),
                     ignore.case = TRUE)
        cl.nm <- sort(names(dt)[nt..],
                      decreasing = TRUE)
        return(cl.nm)
    }

    ## Resolve measurement columns conservatively to avoid averaging
    ## unrelated DBH or height fields before tier assignment. Prefer an
    ## exact alias first; otherwise allow numbered repeats of the same
    ## base field (for example Dn1, Dn2) and only then average them.
    resolve_measure_cols <- function(dt, aliases) {
        nm0 <- names(dt)
        nml <- tolower(nm0)
        ali <- tolower(aliases)

        ## Prefer an exact alias, in declared order.
        ii <- match(ali, nml)
        ii <- ii[!is.na(ii)]
        if(length(ii) > 0L)
            return(nm0[ii[1L]])

        ## Otherwise allow numbered repeats of the same base field.
        hits <- integer(0)
        for(a in ali) {
            rx <- paste0('^', a, '([._]?[0-9]+)?$')
            hits <- c(hits, grep(rx, nml, perl = TRUE))
        }
        hits <- unique(hits)

        if(length(hits) == 0L)
            return(character(0))

        cols <- nm0[hits]
        base <- sub('([._]?[0-9]+)$', '', tolower(cols))

        ## Only average if all matched columns are numbered repeats of
        ## one field. If the match is ambiguous, keep the first column
        ## and warn instead of averaging different sources.
        if(length(unique(base)) > 1L) {
            warning(
                'Ambiguous measurement columns matched: ',
                paste(cols, collapse = ', '),
                '. Using ', cols[1L],
                call. = FALSE
            )
            return(cols[1L])
        }

        cols[order(cols)]
    }

    ## Dominant height is computed later from h, d, and n.
    var. <- var[!var %in% 'Hd']

    diam_cols <- character(0)
    ht_cols <- character(0)

    ## Resolve the raw diameter and height columns only when needed.
    if(any(var. %in% c('d', 'n', 'ba')))
        diam_cols <- resolve_measure_cols(nfi, c('Dn', 'Diamet', 'Diametro'))

    if(any(var. %in% 'h'))
        ht_cols <- resolve_measure_cols(nfi, c('altura', 'Ht'))

    ## Convert selected columns to a numeric matrix while preserving order.
    get_numeric_matrix <- function(dt, cols) {
        if(length(cols) == 0L)
            return(NULL)

        x <- dt[, cols, drop = FALSE]
        x <- lapply(x, function(z) as.numeric(as.character(z)))
        x <- as.data.frame(x, check.names = FALSE,
                           stringsAsFactors = FALSE)
        as.matrix(x)
    }

    ## Vectorised trees-per-hectare calculation for concentric designs,
    ## with fallback to trees_per_ha() for other supported designs.
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

    ## Pre-allocate derived vectors.
    diam_mm <- NULL
    diam_cm <- NULL
    trees_ha <- NULL
    ht_dm <- NULL

    ## Diameter-based metrics: treat zeros as missing values and average
    ## repeated diameter columns row-wise when several measurements exist.
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

    ## Height-based metrics: treat zeros as missing values and average
    ## repeated height columns row-wise when present.
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

    ## Dispatch each metric, using the fast precomputed vectors when the
    ## relevant raw columns were already resolved above.
    fdn <- function(dbh, var){
        if(var %in% 'd') {
            if(length(diam_cols) > 0L)
                return(diam_mm)
            cols <- resolve_measure_cols(dbh, c('Dn', 'Diamet', 'Diametro'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var)))
        }

        if(var %in% 'ba') {
            if(length(diam_cols) > 0L)
                return(pi * diam_cm^2 * (4 * 1E4)^-1)
            cols <- resolve_measure_cols(dbh, c('Dn', 'Diamet', 'Diametro'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var)))
        }

        if(var %in% 'n') {
            if(length(diam_cols) > 0L)
                return(trees_ha)
            cols <- resolve_measure_cols(dbh, c('Dn', 'Diamet', 'Diametro'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var, design = design)))
        }

        if(var %in% 'h') {
            if(length(ht_cols) > 0L)
                return(ht_dm)
            cols <- resolve_measure_cols(dbh, c('altura', 'Ht'))
            return(apply(dbh[, cols, drop = FALSE], 1,
                         function(x) dbhMetric(x, var, design = design)))
        }
    }

    ## Compute the requested metrics except dominant height.
    metric_list <- vector('list', length(var.))
    if(length(var.) > 0L) {
        for(i in seq_along(var.))
            metric_list[[i]] <- fdn(nfi, var.[i])
    }

    dmt <- data.frame(metric_list, check.names = FALSE,
                      stringsAsFactors = FALSE)
    if(length(var.) > 0L)
        names(dmt) <- var.

    ## Preserve province codes stored as attribute by readNFI().
    if(!is.null(attr(nfi, 'pr.')))
        dmt <- cbind(pr = attr(nfi, 'pr.'), dmt)

    nm_all <- names(nfi)

    ## Match grouping columns case-insensitively while preserving their
    ## original names in the input object.
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

    ## Bind the grouping columns requested through levels.
    if(length(keep_cols) == 0L) {
        dmt <- data.frame(dmt, check.names = FALSE)
    } else {
        dmt <- data.frame(nfi[, keep_cols, drop = FALSE], dmt,
                          check.names = FALSE)
    }

    ## Compute dominant height within each grouping level.
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

    ## Restore attributes, attach unit metadata, and set the output class.
    attr(dmt, 'nfi.nr') <- nfi_nr
    dmt <- conv_units(dmt)

metric_units <- c(
    d  = "mm",
    h  = "dm",
    ba = "m2",
    n  = "",
    Hd = "dm"
)

attr(dmt, "units") <- metric_units[intersect(names(dmt), names(metric_units))]

    if (any(var %in% c("n", "Hd"))) {
        design_meta <- list(
            name = if (!is.null(design$name)) design$name else NA_character_,
            class = class(design),
            min_dbh_cm = if (!is.null(design$min_dbh_cm)) design$min_dbh_cm else NA_real_,
            sample_area_m2 = if (!is.null(design$sample_area_m2)) design$sample_area_m2 else NA_real_,
            expansion_factor = if (!is.null(design$sf)) design$sf else NA_real_,
            metadata = if (!is.null(design$metadata)) design$metadata else list(),
            used_for = "n",
            returned_unit = "trees/ha"
        )
        attr(dmt, "design_meta") <- design_meta
    }

## attr(dmt, "units") <- units_map[names(dmt)]

    class(dmt) <- append('nfiMetrics', class(dmt))
    return(dmt)

### \code{data.frame} with the grouping columns selected through
### \code{levels} plus the requested metrics in \code{var}. The output
### inherits class \code{'nfiMetrics'}. Inspect
### \code{attr(x, 'units')} to see the units attached to each returned
### variable and \code{attr(x, 'design_meta')} to inspect the sampling
### design used to derive \code{'n'}.
}, ex = function(){
## Example with bundled Toledo data
ifn4p45 <- system.file("Ifn4_Toledo.zip", package = "basifoR")

## Decompress the archive and read the first 100 records
fetch_ifn4p45 <- fetchNFI(ifn4p45)
get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100, ]

## Compute default metrics and inspect the reported units
metrics_ifn4p45 <- nfiMetrics(get_ifn4p45)
attr(metrics_ifn4p45, 'units')
})
