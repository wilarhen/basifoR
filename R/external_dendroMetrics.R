
external_dendroMetrics <- structure(function #Summarize external dendrometric and volume data
### Summarizes external tree data after computing required metrics and optional volume outputs. The function can return filtered tree-level outputs when \code{summ.vr = NULL} or grouped stand-level summaries when \code{summ.vr} is supplied, and it supports processing several inputs with optional parallel execution.
##title<< Summarize external dendrometric and volume data
##description<< Summarize external inventory data by computing required metrics, optional volume outputs, and grouped stand-level aggregates for external basifoR workflows.
(
    x,
    summ.vr = NULL,
    cut.dt = "d == d",
    report = FALSE,
    mc.cores = getOption("mc.cores", 1L),
    var = c("d", "h", "ba", "n", "Hd"),
    parametro = NULL,
    design = NULL,
    parameter_table = NULL,
    method_registry = external_volume_method_registry(),
    levels = NULL,
    metric_levels = NULL,
    keep_cols = NULL,
    metric_keep_cols = NULL,
    colmap = NULL,
    metric_colmap = list(
        d = c("d", "dbh", "diameter", "diameter_mm"),
        h = c("h", "height", "height_m")
    ),
    d_unit = NULL,
    metric_d_unit = c("mm", "cm")[1],
    h_unit = NULL,
    metric_h_unit = c("m", "dm", "cm")[1],
    volume_colmap = list(
        d = c("d"),
        h = c("h"),
        dnm = c("dnm", "d_nm", "D.n.m."),
        v = c("v"),
        species = c("species", "spec", "especie"),
        region = c("region", "pr"),
        equation_set = c("equation_set", "eqset", "tariff", "model_set")
    ),
    selector = c("first", "priority")[1],
    track_provenance = FALSE,
    compute_metrics_if_needed = TRUE,
    ...
) {
    call0 <- match.call(expand.dots = TRUE)

    `%||%` <- function(a, b) if (is.null(a)) b else a
    dots <- list(...)
    metric_extra <- dots[intersect(names(dots), c("domheight_fun"))]

    if (!is.null(d_unit))
        metric_d_unit <- d_unit
    if (!is.null(h_unit))
        metric_h_unit <- h_unit
    metric_d_unit <- match.arg(metric_d_unit, c("mm", "cm"))
    metric_h_unit <- match.arg(metric_h_unit, c("m", "dm", "cm"))

    metric_levels <- metric_levels %||% levels
    metric_keep_cols <- metric_keep_cols %||% keep_cols

    if (!is.null(colmap)) {
        volume_colmap <- utils::modifyList(volume_colmap, colmap)
        dh_keys <- intersect(names(colmap), c("d", "h"))
        if (length(dh_keys)) {
            metric_colmap <- utils::modifyList(metric_colmap, colmap[dh_keys])
        }
    }

    if (is.null(parametro)) {
        output_to_param <- setNames(
            toupper(names(method_registry)),
            tolower(vapply(
                method_registry,
                function(z) z$output %||% NA_character_,
                character(1)
            ))
        )
        var_low <- tolower(var %||% character(0))
        p1 <- toupper(intersect(var_low, tolower(names(method_registry))))
        p2_keys <- intersect(var_low, names(output_to_param))
        p2 <- unname(output_to_param[p2_keys])
        parametro <- unique(c(p1, p2))
        if (!length(parametro))
            parametro <- NULL
    }

    finalize_output <- function(out, call) {
        if (is.null(out))
            return(NULL)

        attr(out, "call") <- call
        class(out) <- unique(c("external_dendroMetrics", "dendroMetrics", class(out)))
        out
    }

    get_units_map <- function(x) {
        un <- attr(x, "units")
        if (is.null(un))
            return(setNames(character(0), character(0)))

        if (is.null(names(un)) || anyNA(names(un)) || any(names(un) == ""))
            stop("'attr(x, \"units\")' must be a named vector.", call. = FALSE)

        un[!duplicated(names(un))]
    }

    resolve_cols <- function(dt, cols, required = TRUE) {
        if (is.null(cols))
            return(character(0))

        cols <- as.character(cols)
        cols <- cols[!is.na(cols) & nzchar(cols)]
        if (!length(cols))
            return(character(0))

        nm0 <- names(dt)
        nml <- tolower(nm0)

        out <- character(0)

        for (cl in cols) {
            i <- match(tolower(cl), nml)

            if (!is.na(i)) {
                out <- c(out, nm0[i])
                next
            }

            j <- grep(tolower(cl), nml, fixed = TRUE)
            if (length(j) == 1L) {
                out <- c(out, nm0[j])
            } else if (length(j) > 1L) {
                warning(
                    "Ambiguous grouping column '", cl,
                    "'. Using ", nm0[j[1L]],
                    call. = FALSE
                )
                out <- c(out, nm0[j[1L]])
            } else if (isTRUE(required)) {
                stop("Could not resolve grouping column '", cl, "'.",
                     call. = FALSE)
            }
        }

        unique(out)
    }

    has_standardized_metric <- function(dt, key) {
        aliases <- switch(
            tolower(key),
            d  = c("d"),
            h  = c("h"),
            ba = c("ba"),
            n  = c("n"),
            hd = c("hd", "Hd"),
            character(0)
        )

        if (!length(aliases))
            return(FALSE)

        nml <- tolower(names(dt))
        ali <- tolower(aliases)
        ii <- match(ali, nml)
        ii <- ii[!is.na(ii)]
        if (!length(ii))
            return(FALSE)

        nm <- names(dt)[ii[1L]]
        nm %in% names(get_units_map(dt))
    }

    convert_value_units <- function(z, from, to) {
        if (is.null(to) || is.na(to))
            return(z)

        from_chr <- if (is.null(from) || is.na(from)) "" else as.character(from)
        to_chr <- as.character(to)

        if (!nzchar(to_chr))
            return(z)

        if (!nzchar(from_chr))
            stop("Cannot convert from an unknown unit.", call. = FALSE)

        from <- tolower(from_chr)
        to <- tolower(to_chr)

        if (identical(from, to))
            return(z)

        to_base <- function(x, un) {
            if (un == "mm") return(x / 1000)
            if (un == "cm") return(x / 100)
            if (un == "dm") return(x / 10)
            if (un == "m")  return(x)
            if (un == "cm3") return(x / 1e6)
            if (un == "dm3") return(x / 1000)
            if (un == "m3")  return(x)
            stop("Unsupported unit: ", un, call. = FALSE)
        }

        from_base <- function(x, un) {
            if (un == "mm") return(x * 1000)
            if (un == "cm") return(x * 100)
            if (un == "dm") return(x * 10)
            if (un == "m")  return(x)
            if (un == "cm3") return(x * 1e6)
            if (un == "dm3") return(x * 1000)
            if (un == "m3")  return(x)
            stop("Unsupported unit: ", un, call. = FALSE)
        }

        from_base(to_base(z, from), to)
    }

    convert_cols_to_units <- function(dt, mapping) {
        un <- get_units_map(dt)
        if (!length(un))
            return(dt)

        for (nm in names(mapping)) {
            if (!nm %in% names(dt) || !nm %in% names(un))
                next

            target_unit <- mapping[[nm]]
            if (is.null(target_unit) || is.na(target_unit) || !nzchar(as.character(target_unit)))
                next

            current_unit <- un[[nm]]
            if (is.null(current_unit) || is.na(current_unit) || !nzchar(as.character(current_unit)))
                next

            dt[[nm]] <- convert_value_units(
                suppressWarnings(as.numeric(as.character(dt[[nm]]))),
                from = current_unit,
                to = target_unit
            )
            un[[nm]] <- target_unit
        }

        attr(dt, "units") <- un
        dt
    }

    build_metric_var <- function(var, summ.vr, parametro, method_registry) {
        var <- unique(var %||% character(0))
        var <- var[!is.na(var) & nzchar(var)]

        volume_requested <- length(parametro %||% character(0)) > 0L
        needs_n_for_summary <- !is.null(summ.vr) && (
            length(intersect(tolower(var), c("d", "h", "hd", "ba"))) > 0L ||
            volume_requested
        )

        out <- unique(c(var, if (needs_n_for_summary) "n"))

        if ("Hd" %in% out || "hd" %in% tolower(out))
            out <- unique(c(out, "d", "h", "n"))

        out
    }

    materialize_metrics_only <- function(
        x,
        tree_var_needed,
        design,
        metric_levels,
        metric_keep_cols,
        metric_colmap,
        metric_d_unit,
        metric_h_unit,
        compute_metrics_if_needed,
        ...
    ) {
        if (is.null(x))
            return(x)

        missing_metrics <- tree_var_needed[
            !vapply(tolower(tree_var_needed), function(k) {
                has_standardized_metric(x, k)
            }, logical(1))
        ]

        if (!length(missing_metrics) &&
            inherits(x, c("externalMetrics", "metrics2vol")))
            return(x)

        if (!isTRUE(compute_metrics_if_needed)) {
            stop(
                "Requested metric columns are missing and 'compute_metrics_if_needed = FALSE'.",
                call. = FALSE
            )
        }

        metric_fun <- get0("externalMetrics", mode = "function", inherits = TRUE)
        if (is.null(metric_fun))
            stop("Could not find 'externalMetrics()'.", call. = FALSE)

        if (is.null(design))
            stop(
                "To compute metrics from raw external data, supply 'design'.",
                call. = FALSE
            )

        keep_needed <- unique(c(metric_keep_cols, metric_levels))
        keep_needed <- keep_needed[!is.na(keep_needed) & nzchar(keep_needed)]

        do.call(
            metric_fun,
            c(
                list(
                    x = x,
                    var = tree_var_needed,
                    levels = metric_levels,
                    design = design,
                    colmap = metric_colmap,
                    d_unit = metric_d_unit,
                    h_unit = metric_h_unit,
                    keep_cols = keep_needed
                ),
                metric_extra
            )
        )
    }

    external_one <- function(
        x,
        summ.vr,
        cut.dt,
        report,
        var,
        parametro,
        design,
        parameter_table,
        method_registry,
        metric_levels,
        metric_keep_cols,
        metric_colmap,
        metric_d_unit,
        metric_h_unit,
        volume_colmap,
        selector,
        track_provenance,
        compute_metrics_if_needed,
        ...
    ) {
        x0 <- x
        if (is.null(x0))
            return(x)

        if (!is.data.frame(x))
            stop("'x' must be a data.frame, processed data.frame, or list thereof.",
                 call. = FALSE)

        tree_var_needed <- build_metric_var(
            var = var,
            summ.vr = summ.vr,
            parametro = parametro,
            method_registry = method_registry
        )

        effective_metric_levels <- unique(c(metric_levels, summ.vr))
        effective_metric_levels <- effective_metric_levels[
            !is.na(effective_metric_levels) & nzchar(effective_metric_levels)
        ]

        effective_metric_keep <- unique(c(
            metric_keep_cols,
            effective_metric_levels,
            unlist(
                volume_colmap[
                    intersect(names(volume_colmap), c("species", "region", "equation_set"))
                ],
                use.names = FALSE
            )
        ))
        effective_metric_keep <- effective_metric_keep[
            !is.na(effective_metric_keep) & nzchar(effective_metric_keep)
        ]

        has_param <- length(parametro %||% character(0)) > 0L

        if (has_param) {
            dt <- externalMetrics2Vol(
                x = x,
                parametro = parametro,
                parameter_table = parameter_table,
                method_registry = method_registry,
                colmap = volume_colmap,
                selector = selector,
                track_provenance = track_provenance,
                compute_metrics_if_needed = compute_metrics_if_needed,
                design = design,
                metric_var = tree_var_needed,
                metric_levels = effective_metric_levels,
                metric_keep_cols = effective_metric_keep,
                metric_colmap = metric_colmap,
                metric_d_unit = metric_d_unit,
                metric_h_unit = metric_h_unit,
                ...
            )
        } else {
            dt <- materialize_metrics_only(
                x = x,
                tree_var_needed = tree_var_needed,
                design = design,
                metric_levels = effective_metric_levels,
                metric_keep_cols = effective_metric_keep,
                metric_colmap = metric_colmap,
                metric_d_unit = metric_d_unit,
                metric_h_unit = metric_h_unit,
                compute_metrics_if_needed = compute_metrics_if_needed,
                ...
            )
        }

        design_meta <- attr(dt, "design_meta")
        volume_meta <- attr(dt, "volume_meta")

        names(dt) <- tolower(names(dt))

        frm <- attr(dt, "units")
        if (!is.null(frm)) {
            names(frm) <- tolower(names(frm))
            keep_units <- !is.na(names(frm)) & nzchar(names(frm))
            frm <- frm[keep_units]
            frm <- frm[!duplicated(names(frm))]
            frm <- frm[names(frm) %in% names(dt)]
            attr(dt, "units") <- frm
        }

        if (is.null(summ.vr)) {
            dt <- subset(dt, eval(parse(text = cut.dt)))

            if (!is.null(frm))
                attr(dt, "units") <- frm[intersect(names(dt), names(frm))]
            if (!is.null(design_meta))
                attr(dt, "design_meta") <- design_meta
            if (!is.null(volume_meta))
                attr(dt, "volume_meta") <- volume_meta

            if (report)
                write.csv(dt, file = "report.csv", row.names = FALSE)

            return(dt)
        }

        provenance_cols <- grepl(
            "_(source|status|raw_unit|scale|model)$",
            names(dt),
            ignore.case = TRUE
        )
        if (any(provenance_cols))
            dt <- dt[, !provenance_cols, drop = FALSE]

        summ_cols <- resolve_cols(dt, summ.vr, required = TRUE)

        target_units <- c(
            d = "cm",
            h = "m",
            hd = "m",
            ba = "m2",
            n = "",
            v = "m3",
            vcc = "m3",
            vsc = "m3",
            iavc = "m3",
            vle = "m3"
        )
        dt <- convert_cols_to_units(dt, target_units)

        weighted_mean_vars <- intersect(c("d", "h", "hd"), names(dt))
        sum_vars <- intersect(c("ba", "n", "v", "vcc", "vsc", "iavc", "vle"),
                              names(dt))

        if (length(unique(c(weighted_mean_vars, sum_vars))) && !"n" %in% names(dt)) {
            stop(
                "Summarization requires column 'n' (trees/ha). ",
                "Provide raw data plus 'design', or pass a processed input that already contains 'n'.",
                call. = FALSE
            )
        }

        msp <- split(dt, dt[summ_cols], drop = TRUE)
        msp <- Filter("nrow", msp)

        fsum <- function(z) {
            scale_vars <- unique(c(setdiff(weighted_mean_vars, "n"),
                                   setdiff(sum_vars, "n")))

            if (length(scale_vars))
                z[, scale_vars] <- z[, scale_vars, drop = FALSE] * z[, "n"]

            summ_names <- unique(c(intersect("n", names(z)),
                                   weighted_mean_vars,
                                   sum_vars))

            if (length(summ_names)) {
                sum_or_na <- function(v) {
                    if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
                }
                summ <- vapply(z[, summ_names, drop = FALSE], sum_or_na, numeric(1))
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
            summ <- sapply(summ, function(v) round(v, 3))
            summ <- t(as.matrix(summ))

            non_metric <- names(z)[!names(z) %in% unique(c(weighted_mean_vars, sum_vars))]
            non_metric <- intersect(non_metric, summ_cols)
            fcs <- z[1, non_metric, drop = FALSE]

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

        if (report)
            write.csv(resm, file = "report.csv", row.names = FALSE)

        resm
    }

    dots0 <- list(...)

    recycle_arg <- function(x, n, arg) {
        if (is.null(x))
            return(rep(list(NULL), n))

        if (is.data.frame(x))
            return(rep(list(x), n))

        if (inherits(x, "inventory_design"))
            return(rep(list(x), n))

        if (is.list(x)) {
            is_named <- !is.null(names(x)) && any(nzchar(names(x)))
            is_scalar_list <- is_named || inherits(x, c("inventory_design", "data.frame"))
            if (is_scalar_list)
                return(rep(list(x), n))

            if (n == 1L)
                return(list(x))

            if (length(x) == 1L)
                return(rep(x, n))

            if (length(x) != n)
                stop("'", arg, "' must have length 1 or length ", n, ".",
                     call. = FALSE)

            return(x)
        }

        rep(list(x), n)
    }

    n_inputs <- if (is.data.frame(x)) 1L else max(length(x), 1L)

    arg_names <- c(
        "design", "parameter_table", "method_registry", "metric_levels",
        "metric_keep_cols", "metric_colmap", "metric_d_unit", "metric_h_unit",
        "volume_colmap", "selector", "track_provenance",
        "compute_metrics_if_needed", "var", "parametro"
    )

    arg_values <- list(
        design = design,
        parameter_table = parameter_table,
        method_registry = method_registry,
        metric_levels = metric_levels,
        metric_keep_cols = metric_keep_cols,
        metric_colmap = metric_colmap,
        metric_d_unit = metric_d_unit,
        metric_h_unit = metric_h_unit,
        volume_colmap = volume_colmap,
        selector = selector,
        track_provenance = track_provenance,
        compute_metrics_if_needed = compute_metrics_if_needed,
        var = var,
        parametro = parametro
    )

    x_list <- recycle_arg(x, n_inputs, "x")
    arg_lists <- lapply(arg_names, function(nm) recycle_arg(arg_values[[nm]], n_inputs, nm))
    names(arg_lists) <- arg_names

    jobs <- lapply(seq_len(n_inputs), function(i) {
        args_i <- lapply(arg_lists, function(y) y[[i]])
        list(x = x_list[[i]], args = args_i)
    })

    run_job <- function(job) {
        tryCatch(
            do.call(
                external_one,
                c(
                    list(
                        x = job$x,
                        summ.vr = summ.vr,
                        cut.dt = cut.dt,
                        report = FALSE
                    ),
                    job$args,
                    dots0
                )
            ),
            error = function(e) {
                structure(
                    list(
                        message = conditionMessage(e),
                        x = job$x
                    ),
                    class = "external_dendroMetrics_error"
                )
            }
        )
    }

    if (length(jobs) == 1L) {
        out <- do.call(
            external_one,
            c(
                list(
                    x = jobs[[1]]$x,
                    summ.vr = summ.vr,
                    cut.dt = cut.dt,
                    report = report
                ),
                jobs[[1]]$args,
                dots0
            )
        )
        return(finalize_output(out, call0))
    }

    mc.cores <- as.integer(mc.cores)
    if (is.na(mc.cores) || mc.cores < 1L)
        mc.cores <- 1L

    use_parallel <- length(jobs) > 1L && mc.cores > 1L

    if (!use_parallel) {

        res_list <- lapply(jobs, run_job)

    } else if (.Platform$OS.type == "windows") {

        cl <- parallel::makeCluster(mc.cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)

        parallel::clusterExport(
            cl = cl,
            varlist = c("jobs", "run_job", "external_one", "summ.vr", "cut.dt", "dots0"),
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

    errs <- vapply(res_list, inherits, logical(1), what = "external_dendroMetrics_error")

    if (any(errs)) {
        msg <- vapply(res_list[errs], function(z) {
            paste0(
                "external_dendroMetrics failed: ",
                z$message
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

    if (report)
        write.csv(out, file = "report.csv", row.names = FALSE)

    finalize_output(out, call0)
    ##value<< A new \code{"external_dendroMetrics"} object. When \code{summ.vr = NULL} the function returns filtered tree-level outputs; otherwise it returns grouped summaries with unit metadata and any preserved \code{design_meta} or \code{volume_meta}.

}, ex = function() {
    sq_0.1ha <- new_inventory_design(
        sample_area_m2 = 1000,
        min_dbh_cm = 7.5,
        name = "Square 0.1-ha plot"
    )

    x <- data.frame(
        plot = c("P1", "P1", "P2"),
        species = c("sp1", "sp1", "sp2"),
        Diameter = c(120, 185, 260),
        Height = c(7.1, 9.4, 13.2)
    )

    pars <- data.frame(
        parameter = "V",
        species = c("sp1", "sp2"),
        a = c(0.00002, 0.00003),
        b = c(2.30, 2.10),
        model = c("demo_sp1", "demo_sp2")
    )

    ext_methods <- external_volume_method_registry(list(
        V = new_volume_method(
            output = "v",
            fun = function(dbh_mm, h_m, pars) {
                dbh_cm <- dbh_mm / 10
                pars$a + pars$b * (dbh_cm^2) * h_m
            },
            raw_unit = "cm3",
            unit = "m3",
            scale_to_m3 = 1 / 1e6,
            build_args = function(ctx, pars, resolved) {
                list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_,
            match_by = "species",
            required_inputs = c("d", "h")
        )
    ))

    external_dendroMetrics(
        x = x,
        summ.vr = "plot",
        design = sq_0.1ha,
        parameter_table = pars,
        method_registry = ext_methods,
        metric_colmap = list(d = "Diameter", h = "Height"),
        metric_d_unit = "mm",
        metric_h_unit = "m"
    )
})


update.external_dendroMetrics <- function(
    object,
    ...,
    evaluate = TRUE
) {
    if (!inherits(object, "external_dendroMetrics"))
        stop(
            "'object' must inherit from 'external_dendroMetrics'.",
            call. = FALSE
        )

    call0 <- attr(object, "call")

    if (is.null(call0))
        stop(
            "No stored call found in 'object'. ",
            "Recreate the result with external_dendroMetrics() and then call update(result, ...).",
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
}
