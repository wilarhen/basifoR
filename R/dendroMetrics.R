dendroMetrics <- structure(function#Summarize dendrometrics
### This function summarizes dendrometric data from the Spanish
### National Forest Inventory (SNF). It primarily accepts a province
### name or number, a local compressed SNF file, or a URL to a
### compressed SNF file hosted by \code{www.miteco.gob.es}. It can
### also process data frames previously returned by
### \code{\link{readNFI}}, \code{\link{nfiMetrics}}, or
### \code{\link{metrics2Vol}}. Dendrometric variables in the output
### are transformed into stand units, see Details section.
                          ##details<< Dendrometric variables are
                          ## summarized according to the levels of
                          ## argument \code{summ.vr}. Summary outputs
                          ## include the categorical columns defined
                          ## by \code{summ.vr} together with the
                          ## quantitative variables available after
                          ## processing with \code{\link{nfiMetrics}}
                          ## and \code{\link{metrics2Vol}}.
                          ##
                          ## These variables may include tree basal
                          ## area \code{ba} (\code{'m2 ha-1'}), mean
                          ## diameter at breast height \code{d}
                          ## (\code{'cm'}), quadratic mean diameter
                          ## \code{dg} (\code{'cm'}), mean tree
                          ## height \code{h} (\code{'m'}), number of
                          ## trees per hectare \code{n}
                          ## (\code{'dimensionless'}), and over-bark
                          ## volume \code{v} (\code{'m3 ha-1'}).
                          ##
                          ## When \code{summ.vr = NULL}, the function
                          ## returns tree-level outputs from
                          ## \code{\link{metrics2Vol}} after applying
                          ## the filter defined in \code{cut.dt}.
                          ##
                          ## When \code{summ.vr} is not \code{NULL},
                          ## the function converts supported
                          ## variables to stand units, splits the data
                          ## by the requested grouping variable, and
                          ## computes summaries by group. Extensive
                          ## variables are multiplied by \code{n}
                          ## before summation. Variables \code{d},
                          ## \code{h}, and \code{Hd}, when present,
                          ## are returned as averages weighted by
                          ## \code{n}.
                          ##
                          ## If both \code{ba} and \code{n} are
                          ## available, the function also derives the
                          ## quadratic mean diameter \code{dg}.
                          ##
                          ## Output subsets are extracted using the
                          ## logical expression supplied in
                          ## \code{cut.dt}, see syntax in
                          ## \code{\link{Logic}}.
                          ##
                          ## The function accepts one input object or
                          ## several inputs. When several inputs are
                          ## supplied, each one is processed
                          ## independently and the results are merged
                          ## into a single output data frame.
                          ## Parallel processing is available through
                          ## argument \code{mc.cores} and can be
                           ## disabled with \code{.parallel = FALSE}.
(
nfi, ##<< \code{character}, \code{data.frame}, or \code{list}. A
         ## province name or province number used to locate SNF data;
         ## a local path or URL to a compressed SNF file
         ## (\code{.zip}), including ZIP files hosted by
         ## \code{www.miteco.gob.es}; a data frame such as that
         ## returned by \code{\link{readNFI}},
         ## \code{\link{nfiMetrics}}, or
    ## \code{\link{metrics2Vol}}; or a list of such objects.
    summ.vr = 'Estadillo', ##<< \code{character} or \code{NULL}. Name
                           ##of a Categorical variables in the SNF
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
   mc.cores = getOption("mc.cores", 1L), ##<< \code{integer}. Number
                                         ## of cores used when several
                                         ## inputs are processed.
   .parallel = TRUE, ##<< \code{logical}. If \code{TRUE}, allow
                     ##parallel processing when several inputs are
                     ##supplied.
    ... ##<< Additional arguments passed to \code{\link{readNFI}},
        ##\code{\link{nfiMetrics}}, or \code{\link{metrics2Vol}},
        ##including \code{nfi.nr} when required.

) {
    call0 <- match.call(expand.dots = TRUE)

    finalize_output <- function(out, call) {
        if (is.null(out))
            return(finalize_output(out, call0))

        attr(out, "call") <- call
        class(out) <- unique(c("dendroMetrics", class(out)))
        out
    }

dendro_one <- function(nfi, summ.vr, cut.dt, report, ...) {
    nfi. <- nfi
    if (is.null(nfi.))
        return(nfi)

    if (!inherits(nfi., "metrics2vol"))
        nfi <- metrics2Vol(nfi, ...)

    design_meta <- attr(nfi, "design_meta")
    volume_meta <- attr(nfi, "volume_meta")
    nfi_nr_attr <- attr(nfi, "nfi.nr")

    names(nfi) <- tolower(names(nfi))

    frm. <- attr(nfi, "units")

    if (!is.null(frm.)) {
        names(frm.) <- tolower(names(frm.))
        keep_units <- !is.na(names(frm.)) & nzchar(names(frm.))
        frm. <- frm.[keep_units]
        frm. <- frm.[!duplicated(names(frm.))]
        frm. <- frm.[names(frm.) %in% names(nfi)]
        attr(nfi, "units") <- frm.
    }

    if (is.null(summ.vr)) {
        nfi <- subset(nfi, eval(parse(text = cut.dt)))

        if (!is.null(frm.))
            attr(nfi, "units") <- frm.[intersect(names(nfi), names(frm.))]
        if (!is.null(design_meta))
            attr(nfi, "design_meta") <- design_meta
        if (!is.null(volume_meta))
            attr(nfi, "volume_meta") <- volume_meta
        if (!is.null(nfi_nr_attr))
            attr(nfi, "nfi.nr") <- nfi_nr_attr

        if (report)
            write.csv(nfi, file = "report.csv", row.names = FALSE)

        return(nfi)
    }

    summ.vr <- flev(nfi, summ.vr)

    weighted_mean_vars <- intersect(c("d", "h", "hd"), names(nfi))
    sum_vars <- intersect(c("ba", "n", "v", "vcc", "vsc", "iavc", "vle"),
                          names(nfi))

    mean_target_units <- c(d = "cm", h = "m", hd = "m")
    if (length(weighted_mean_vars)) {
        nfi <- conv_units(
            nfi,
            var = weighted_mean_vars,
            un = unname(mean_target_units[weighted_mean_vars])
        )
    }

    msp <- split(nfi, nfi[summ.vr])
    msp <- Filter("nrow", msp)

    fsum <- function(dt) {
        scale_vars <- unique(c(setdiff(weighted_mean_vars, "n"),
                               setdiff(sum_vars, "n")))

        if (length(scale_vars))
            dt[, scale_vars] <- dt[, scale_vars, drop = FALSE] * dt[, "n"]

        summ_names <- unique(c(intersect("n", names(dt)),
                               weighted_mean_vars,
                               sum_vars))

        ## if (length(summ_names)) {
        ##     summ <- colSums(dt[, summ_names, drop = FALSE], na.rm = TRUE)
        ## } else {
        ##     summ <- numeric(0)
        ## }
        if (length(summ_names)) {
            sum_or_na <- function(x) {
                if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
            }
            summ <- vapply(dt[, summ_names, drop = FALSE], sum_or_na, numeric(1))
        } else {
            summ <- numeric(0)
        }
        
        keep_avg <- intersect(weighted_mean_vars, names(summ))
        if (length(keep_avg) && "n" %in% names(summ) &&
            is.finite(summ["n"]) && summ["n"] > 0)
            summ[keep_avg] <- summ[keep_avg] / summ["n"]

        if (all(c("ba", "n") %in% names(summ)) &&
            is.finite(summ["n"]) && summ["n"] > 0)
            summ["dg"] <- sqrt((4E4 * summ["ba"] / summ["n"]) / pi)

        summ <- summ[order(names(summ))]
        summ <- sapply(summ, function(x) round(x, 3))
        summ <- t(as.matrix(summ))

        fcs. <- names(dt)[!names(dt) %in% unique(c(weighted_mean_vars, sum_vars))]
        fcs <- dt[1, fcs., drop = FALSE]

        cbind(fcs, summ)
    }

    bind_rows_fill_local <- function(a, b) {
        if (is.null(a))
            return(data.frame(b, check.names = FALSE))
        if (is.null(b))
            return(data.frame(a, check.names = FALSE))

        a <- data.frame(a, check.names = FALSE)
        b <- data.frame(b, check.names = FALSE)

        cols <- union(names(a), names(b))

        missing_a <- setdiff(cols, names(a))
        missing_b <- setdiff(cols, names(b))

        if (length(missing_a))
            a[missing_a] <- NA
        if (length(missing_b))
            b[missing_b] <- NA

        rbind(a[, cols, drop = FALSE], b[, cols, drop = FALSE])
    }

    resm <- lapply(msp, fsum)
    resm <- Reduce(bind_rows_fill_local, resm, init = NULL)
    resm <- data.frame(resm, check.names = FALSE)

    resm <- subset(resm, eval(parse(text = cut.dt)))
    rownames(resm) <- NULL

    if (report)
        write.csv(resm, file = "report.csv", row.names = FALSE)

## metrics2Vol() already returns volume outputs in m3
## so dendroMetrics() should only aggregate and relabel them.
vol_vars <- intersect(c("v", "vcc", "vsc", "iavc", "vle"), names(resm))
    
    units_out <- c(
        d = "cm",
        h = "m",
        hd = "m",
        dg = "cm",
        ba = "m2 ha-1",
        n = "ha-1",
        v = "m3 ha-1",
        vcc = "m3 ha-1",
        vsc = "m3 ha-1",
        iavc = "m3 ha-1",
        vle = "m3 ha-1"
    )
    attr(resm, "units") <- units_out[intersect(names(resm), names(units_out))]
    if (!is.null(design_meta))
        attr(resm, "design_meta") <- design_meta
    if (!is.null(volume_meta))
        attr(resm, "volume_meta") <- volume_meta
    if (!is.null(nfi_nr_attr))
        attr(resm, "nfi.nr") <- nfi_nr_attr

    resm
}

    dots0 <- list(...)

    nfi.nr <- dots0[["nfi.nr"]]

    n_inputs <- max(
        if (is.data.frame(nfi)) 1L else length(nfi),
        length(nfi.nr),
        1L
    )

    recycle_arg <- function(x, n, arg) {
        if (is.null(x))
            return(rep(list(NULL), n))
        if (is.data.frame(x))
            return(rep(list(x), n))
        if (n == 1L)
            return(list(x))
        if (is.list(x)) {
            if (length(x) == 1L)
                return(rep(x, n))
            if (length(x) != n)
                stop("'", arg, "' must have length 1 or length ", n, ".")
            return(x)
        }
        if (length(x) == 1L)
            return(rep(as.list(x), n))
        if (length(x) != n)
            stop("'", arg, "' must have length 1 or length ", n, ".")
        as.list(x)
    }

    nfi_list <- recycle_arg(nfi, n_inputs, "nfi")
    nfi.nr_list <- recycle_arg(nfi.nr, n_inputs, "nfi.nr")

    dot_names <- setdiff(names(dots0), "nfi.nr")
    dot_lists <- lapply(dot_names, function(arg) {
        recycle_arg(dots0[[arg]], n_inputs, arg)
    })
    names(dot_lists) <- dot_names

    jobs <- lapply(seq_len(n_inputs), function(i) {
        dots_i <- lapply(dot_lists, function(x) x[[i]])
        names(dots_i) <- dot_names
        if (!is.null(nfi.nr_list[[i]]))
            dots_i[["nfi.nr"]] <- nfi.nr_list[[i]]
        list(nfi = nfi_list[[i]], dots = dots_i)
    })


run_job <- function(job) {
    tryCatch(
        do.call(
            dendro_one,
            c(
                list(
                    nfi = job$nfi,
                    summ.vr = summ.vr,
                    cut.dt = cut.dt,
                    report = FALSE
                ),
                job$dots
            )
        ),
        error = function(e) {
            structure(
                list(
                    message = conditionMessage(e),
                    nfi = job$nfi,
                    nfi.nr = job$dots[["nfi.nr"]]
                ),
                class = "dendroMetrics_error"
            )
        }
    )
}

    if (length(jobs) == 1L) {
        out <- do.call(
            dendro_one,
            c(
                list(
                    nfi = jobs[[1]]$nfi,
                    summ.vr = summ.vr,
                    cut.dt = cut.dt,
                    report = report
                ),
                jobs[[1]]$dots
            )
        )
        return(finalize_output(out, call0))
    }

    mc.cores <- as.integer(mc.cores)
    if (is.na(mc.cores) || mc.cores < 1L)
        mc.cores <- 1L

    if (!.parallel || mc.cores == 1L) {

        res_list <- lapply(jobs, run_job)

    } else if (.Platform$OS.type == "windows") {

        cl <- parallel::makeCluster(mc.cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)

        parallel::clusterExport(
            cl = cl,
            varlist = c("jobs", "run_job", "dendro_one", "summ.vr", "cut.dt"),
            envir = environment()
        )

        res_list <- parallel::parLapply(cl = cl, X = jobs, fun = run_job)

    } else {

        res_list <- parallel::mclapply(
            X = jobs,
            FUN = run_job,
            mc.cores = mc.cores
        )
    }

errs <- vapply(res_list, inherits, logical(1), what = "dendroMetrics_error")

if (any(errs)) {
    msg <- vapply(res_list[errs], function(x) {
        paste0(
            "dendroMetrics failed for nfi = ",
            paste(x$nfi, collapse = ", "),
            ", nfi.nr = ",
            paste(x$nfi.nr, collapse = ", "),
            ": ",
            x$message
        )
    }, character(1))

    stop(paste(msg, collapse = "\n"), call. = FALSE)
}

    res_list <- Filter(Negate(is.null), res_list)

    if (!length(res_list))
        return(NULL)

    bind_rows_fill <- function(a, b) {
        if (is.null(a))
            return(b)
        if (is.null(b))
            return(a)

        a <- data.frame(a, check.names = FALSE)
        b <- data.frame(b, check.names = FALSE)

        cols <- union(names(a), names(b))

        missing_a <- setdiff(cols, names(a))
        missing_b <- setdiff(cols, names(b))

        if (length(missing_a))
            a[missing_a] <- NA
        if (length(missing_b))
            b[missing_b] <- NA

        rbind(a[, cols, drop = FALSE], b[, cols, drop = FALSE])
    }

    collect_units <- function(x) {
        units_list <- lapply(x, function(y) attr(y, "units"))
        units_list <- Filter(Negate(is.null), units_list)

        if (!length(units_list))
            return(NULL)

        out_units <- do.call(c, units_list)
        out_units[!duplicated(names(out_units))]
    }

    collect_attr <- function(x, attr_name) {
        vals <- lapply(x, function(y) attr(y, attr_name))
        vals <- Filter(Negate(is.null), vals)

        if (!length(vals))
            return(NULL)
        if (length(vals) == 1L)
            return(vals[[1L]])

        same <- vapply(vals[-1L], function(z) identical(z, vals[[1L]]), logical(1))
        if (all(same))
            return(vals[[1L]])

        vals
    }

    out <- Reduce(bind_rows_fill, res_list)
    out <- data.frame(out, check.names = FALSE)
    rownames(out) <- NULL

    out_units <- collect_units(res_list)
    if (!is.null(out_units))
        attr(out, "units") <- out_units[names(out_units) %in% names(out)]

    out_design_meta <- collect_attr(res_list, "design_meta")
    if (!is.null(out_design_meta))
        attr(out, "design_meta") <- out_design_meta

    out_volume_meta <- collect_attr(res_list, "volume_meta")
    if (!is.null(out_volume_meta))
        attr(out, "volume_meta") <- out_volume_meta

    out_nfi_nr <- collect_attr(res_list, "nfi.nr")
    if (!is.null(out_nfi_nr))
        attr(out, "nfi.nr") <- out_nfi_nr

    if (report)
        write.csv(out, file = "report.csv", row.names = FALSE)

    finalize_output(out, call0)
### \code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
### \code{\link{metrics2Vol}}, or a summary of the variables, see
### Details section.
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

# Calculate volume metrics 
vol_ifn4p45 <- metrics2Vol(metrics_ifn4p45)

# Compute all metrics (dendrometrics) for trees with height > 8
dendromet_ifn4p45 <- dendroMetrics(vol_ifn4p45, cut.dt='h > 8')

# Display structure of dendrometric data
str(dendromet_ifn4p45)

# Check units of metrics
attr(dendromet_ifn4p45,'units')

## Alternatively, download data from 'www.miteco.gob.es'
## Specify province name/number to compute dendrometrics:

## donttest{
### Compute dendrometrics for Toledo (code 45) for NFI 4, height >= 8
## dendromet_ifn4p45 <- dendroMetrics(provincia=45,nfi=4,cut.dt='h >= 8')
### Display first few rows
## head(dendromet_ifn4p45)
### Check units of metrics
## attr(dendromet_ifn4p45,'units')
## }
    
})


update.dendroMetrics <- function #Update a dendroMetrics result
##title<< Update a stored dendroMetrics call
##description<< Re-run \code{\link{dendroMetrics}} from the call stored
## in a previous \code{"dendroMetrics"} result while replacing one or
## more named arguments in \code{...}. This method supports reproducible
## re-evaluation of the original workflow and can either return the
## updated result or the reconstructed call.
### This S3 method requires an object created by the updatable
### \code{\link{dendroMetrics}} definition, which stores the original
### call in \code{attr(x, "call")}. All arguments supplied in
### \code{...} must be named.
(
    object, ##<< A \code{"dendroMetrics"} object created by the
            ## updatable \code{\link{dendroMetrics}} definition.
    ..., ##<< Named arguments used to replace entries in the stored
         ## call before evaluation.
    evaluate = TRUE ##<< \code{logical}. If \code{TRUE}, evaluate the
                    ## updated call and return the resulting object.
                    ## If \code{FALSE}, return the reconstructed call
                    ## without evaluation.
) {
    if (!inherits(object, "dendroMetrics"))
        stop(
            "'object' must inherit from 'dendroMetrics'. ",
            "Call dendroMetrics(...) first and then use update(result, ...).",
            call. = FALSE
        )

    call0 <- attr(object, "call")

    if (is.null(call0))
        stop(
            "No stored call found in 'object'. ",
            "Recreate the result with the updatable dendroMetrics() definition ",
            "and then call update(result, ...).",
            call. = FALSE
        )

    extras <- as.list(substitute(list(...)))[-1L]

    if (length(extras)) {
        extra_names <- names(extras)
        has_name <- !is.na(extra_names) & nzchar(extra_names)

        if (any(!has_name))
            stop("All arguments in '...' must be named.", call. = FALSE)

        call_list <- as.list(call0)
        for (i in seq_along(extras))
            call_list[[extra_names[[i]]]] <- extras[[i]]
        call0 <- as.call(call_list)
    }

    if (isTRUE(evaluate))
        eval(call0, envir = parent.frame())
    else
        call0
    ##value<< A new \code{"dendroMetrics"} object when
    ## \code{evaluate = TRUE}; otherwise the updated call.
}


update.list <- function #Guard against raw dendroMetrics inputs
##title<< Guard raw list inputs in \code{update}
##description<< Catch attempts to call \code{\link[utils:update]{update}}
## on raw input lists intended for \code{\link{dendroMetrics}} and
## return a workflow-specific error message. When the input does not
## look like a \code{dendroMetrics} workflow, this method falls back to
## \code{update.default}.
### This helper makes \code{update(list("toledo", "madrid"), ...)}
### fail with a message that points users to
### \code{dendroMetrics(...)} followed by \code{update(result, ...)}.
(
    object, ##<< A \code{list} passed to \code{update()}.
    ..., ##<< Additional arguments passed to \code{update()}.
    evaluate = TRUE ##<< \code{logical}. Passed through to
                    ## \code{update.default} when no
                    ## \code{dendroMetrics}-specific guard is triggered.
) {
    extras <- as.list(substitute(list(...)))[-1L]
    extra_names <- names(extras)
    likely_dendro_args <- c(
        "nfi", "nfi.nr", "summ.vr", "cut.dt", "report", "var",
        "parametro", "keep.var", "keep.legacy", "method_registry",
        "track_provenance", "cub.met"
    )

    looks_like_dendro_input <-
        length(object) > 0L &&
        all(vapply(object, function(x) is.character(x) && length(x) == 1L, logical(1)))

    asks_for_dendro_update <-
        length(extras) > 0L &&
        any(!is.na(extra_names) & nzchar(extra_names) & extra_names %in% likely_dendro_args)

    if (looks_like_dendro_input || asks_for_dendro_update) {
        stop(
            "update() cannot start from raw inputs such as list('toledo', 'madrid'). ",
            "First create a result with dendroMetrics(...), then call update(result, ...).\n",
            "Example:\n",
            "  x <- dendroMetrics(list('toledo', 'madrid'), nfi.nr = c(4, 4))\n",
            "  y <- update(x, cut.dt = 'h > 12')",
            call. = FALSE
        )
    }

    getS3method("update", "default")(object, ..., evaluate = evaluate)
    ##value<< Either an error with a \code{dendroMetrics}-specific
    ## message or the result of \code{update.default()}.
}
