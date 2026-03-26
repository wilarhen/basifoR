`%||%` <- function(a, b) if (is.null(a)) b else a

new_volume_method <- function(
    output,
    fun = NULL,
    fun_name = NULL,
    unit = "m3",
    raw_unit = unit,
    scale_to_m3 = 1,
    build_args = function(ctx, pars, resolved) list(),
    fallback = function(ctx, pars, resolved) resolved$preexisting_v_m3 %||% NA_real_,
    match_by = character(0),
    get_pars = NULL,
    pars = NULL,
    filter_pars = NULL,
    required_inputs = NULL
) {
    list(
        output = output,
        fun = fun,
        fun_name = fun_name,
        unit = unit,
        raw_unit = raw_unit,
        scale_to_m3 = scale_to_m3,
        build_args = build_args,
        fallback = fallback,
        match_by = match_by,
        get_pars = get_pars,
        pars = pars,
        filter_pars = filter_pars,
        required_inputs = required_inputs
    )
}


default_external_volume_methods <- function() {
    list(
        V = new_volume_method(
            output = "v",
            unit = "m3",
            raw_unit = "m3",
            scale_to_m3 = 1,
            build_args = function(ctx, pars, resolved) list(),
            fallback = function(ctx, pars, resolved) resolved$preexisting_v_m3 %||% NA_real_,
            match_by = character(0),
            required_inputs = "v"
        ),
        VCC = new_volume_method(
            output = "vcc",
            unit = "m3",
            raw_unit = "m3",
            scale_to_m3 = 1,
            build_args = function(ctx, pars, resolved) {
                list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_,
            match_by = c("species", "region", "equation_set"),
            required_inputs = c("d", "h")
        ),
        VSC = new_volume_method(
            output = "vsc",
            unit = "m3",
            raw_unit = "m3",
            scale_to_m3 = 1,
            build_args = function(ctx, pars, resolved) {
                list(vcc_m3 = resolved$vcc_m3, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_,
            match_by = c("species", "region", "equation_set"),
            required_inputs = "vcc"
        )
    )
}


external_volume_method_registry <- function(
    methods = get0("external_volume_methods", inherits = TRUE, ifnotfound = NULL)
) {
    defaults <- default_external_volume_methods()

    if (is.null(methods))
        methods <- defaults

    if (!is.list(methods) || is.null(names(methods)))
        stop("'external_volume_methods' must be a named list.", call. = FALSE)

    extra <- getOption("basifoR.external_volume_methods")
    if (!is.null(extra)) {
        if (!is.list(extra) || is.null(names(extra)))
            stop("Option 'basifoR.external_volume_methods' must be a named list.",
                 call. = FALSE)
        methods <- utils::modifyList(methods, extra)
    }

    utils::modifyList(defaults, methods)
}


externalMetrics2Vol <- structure(function(
    x,
    parametro = c("V"),
    parameter_table = NULL,
    method_registry = external_volume_method_registry(),
    colmap = NULL,
    selector = c("first", "priority")[1],
    track_provenance = FALSE,
    compute_metrics_if_needed = TRUE,
    design = NULL,
    var = NULL,
    metric_var = NULL,
    levels = NULL,
    metric_levels = NULL,
    keep_cols = NULL,
    metric_keep_cols = NULL,
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
    ...
) {
    x0 <- x
    if (is.null(x0))
        return(x)

    if (!is.data.frame(x))
        stop("'x' must be a data.frame or an 'externalMetrics' object.",
             call. = FALSE)

    selector <- match.arg(selector, c("first", "priority"))
    if (!is.null(d_unit))
        metric_d_unit <- d_unit
    if (!is.null(h_unit))
        metric_h_unit <- h_unit
    metric_d_unit <- match.arg(metric_d_unit, c("mm", "cm"))
    metric_h_unit <- match.arg(metric_h_unit, c("m", "dm", "cm"))

    metric_var <- metric_var %||% var
    metric_levels <- metric_levels %||% levels
    metric_keep_cols <- metric_keep_cols %||% keep_cols

    if (is.null(colmap)) {
        colmap <- volume_colmap
    } else {
        colmap <- utils::modifyList(volume_colmap, colmap)
        dh_keys <- intersect(names(colmap), c("d", "h"))
        if (length(dh_keys)) {
            metric_colmap <- utils::modifyList(
                metric_colmap,
                colmap[dh_keys]
            )
        }
    }

    dots <- list(...)
    metric_extra <- dots[intersect(names(dots), c("domheight_fun"))]

    get_units_map <- function(x) {
        un <- attr(x, "units")

        if (is.null(un))
            return(setNames(character(0), character(0)))

        if (is.null(names(un)) || anyNA(names(un)) || any(names(un) == ""))
            stop("'attr(x, \"units\")' must be a named vector.",
                 call. = FALSE)

        un[!duplicated(names(un))]
    }

    resolve_col <- function(dt, aliases, required = FALSE) {
        if (is.null(aliases) || !length(aliases)) {
            if (required)
                stop("Missing required aliases.", call. = FALSE)
            return(NULL)
        }

        nm0 <- names(dt)
        nml <- tolower(nm0)
        ali <- tolower(aliases)

        ii <- match(ali, nml)
        ii <- ii[!is.na(ii)]
        if (length(ii))
            return(nm0[ii[1L]])

        hits <- integer(0)
        for (a in ali)
            hits <- c(hits, grep(a, nml, fixed = TRUE))
        hits <- unique(hits)

        if (!length(hits)) {
            if (required)
                stop("Missing required column: ",
                     paste(aliases, collapse = " / "),
                     call. = FALSE)
            return(NULL)
        }

        if (length(hits) > 1L) {
            warning(
                "Ambiguous column match for ",
                paste(aliases, collapse = ", "),
                ". Using ", nm0[hits[1L]],
                call. = FALSE
            )
        }

        nm0[hits[1L]]
    }

    require_units_for <- function(x, cols, where = "x") {
        cols <- cols[!is.null(cols)]
        cols <- cols[!is.na(cols)]
        cols <- cols[nzchar(cols)]

        if (!length(cols))
            return(invisible(NULL))

        un <- get_units_map(x)
        miss <- setdiff(cols, names(un))

        if (length(miss))
            stop("Missing unit metadata in attr(", where, ', "units") for: ',
                 paste(miss, collapse = ", "),
                 call. = FALSE)

        invisible(un)
    }

    convert_value_units <- function(z, from, to) {
        if (is.null(from) || is.na(from) || !nzchar(from))
            stop("Cannot convert from an unknown unit.", call. = FALSE)

        from <- tolower(from)
        to <- tolower(to)

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

    convert_col <- function(dt, col, target_unit) {
        if (is.null(col))
            return(dt)

        un <- get_units_map(dt)
        from_unit <- un[[col]]
        dt[[col]] <- convert_value_units(
            suppressWarnings(as.numeric(as.character(dt[[col]]))),
            from = from_unit,
            to = target_unit
        )
        un[col] <- target_unit
        attr(dt, "units") <- un
        dt
    }

    method_has_equation <- function(param) {
        def <- method_registry[[param]]
        is.function(def$fun) ||
            !is.null(def$fun_name) ||
            is.function(def$get_pars) ||
            !is.null(def$pars)
    }

    method_required_inputs <- function(param) {
        def <- method_registry[[param]]
        req <- def$required_inputs %||% NULL

        if (!is.null(req))
            return(unique(tolower(as.character(req))))

        if (!method_has_equation(param))
            return(tolower(def$output %||% param))

        if (identical(toupper(param), "VSC"))
            return("vcc")

        c("d", "h")
    }

    parametro <- unique(toupper(parametro))
    valid_param <- unique(toupper(names(method_registry)))
    bad_param <- setdiff(parametro, valid_param)
    if (length(bad_param))
        stop("Unknown parametro value(s): ",
             paste(bad_param, collapse = ", "),
             call. = FALSE)

    output_to_param <- setNames(
        names(method_registry),
        tolower(vapply(
            method_registry,
            function(z) z$output %||% NA_character_,
            character(1)
        ))
    )

    expand_required_inputs <- function(req, seen = character(0)) {
        req <- unique(tolower(as.character(req)))
        req <- req[!is.na(req) & nzchar(req)]

        out <- character(0)

        for (inp in req) {
            if (inp %in% c("d", "h", "dnm", "v", "n", "ba", "hd")) {
                out <- c(out, inp)
                next
            }

            parent_param <- output_to_param[[inp]]
            if (is.null(parent_param) || inp %in% seen) {
                out <- c(out, inp)
                next
            }

            out <- c(
                out,
                expand_required_inputs(
                    method_required_inputs(parent_param),
                    seen = c(seen, inp)
                )
            )
        }

        unique(out)
    }

    required_inputs <- unique(unlist(lapply(parametro, method_required_inputs)))
    required_inputs <- expand_required_inputs(required_inputs)

    std_aliases <- list(
        d = unique(c(colmap$d, metric_colmap$d, "d")),
        h = unique(c(colmap$h, metric_colmap$h, "h")),
        dnm = unique(c(colmap$dnm, "dnm")),
        v = unique(c(colmap$v, "v")),
        n = "n",
        ba = "ba",
        hd = c("Hd", "hd")
    )

    has_standardized_input <- function(dt, key) {
        aliases <- std_aliases[[tolower(key)]]
        if (is.null(aliases))
            return(FALSE)

        col <- resolve_col(dt, aliases, required = FALSE)
        if (is.null(col))
            return(FALSE)

        col %in% names(get_units_map(dt))
    }

    maybe_compute_external_metrics <- function(x) {
        if (!isTRUE(compute_metrics_if_needed))
            return(x)

        metrics_supported <- unique(intersect(
            c(required_inputs, tolower(metric_var %||% character(0))),
            c("d", "h", "n", "ba", "hd")
        ))
        if (!length(metrics_supported))
            return(x)

        missing_supported <- metrics_supported[
            !vapply(metrics_supported,
                    function(k) has_standardized_input(x, k),
                    logical(1))
        ]

        if (!length(missing_supported))
            return(x)

        metric_fun <- get0("externalMetrics", mode = "function", inherits = TRUE)
        if (is.null(metric_fun))
            stop(
                "Required metric columns are missing and 'externalMetrics()' is not available.",
                call. = FALSE
            )

        if (is.null(design))
            stop(
                "To compute metrics automatically from raw or partially standardized data, supply 'design'.",
                call. = FALSE
            )

        auto_var_needed <- c(
            if ("d" %in% missing_supported) "d",
            if ("h" %in% missing_supported) "h",
            if ("n" %in% missing_supported) "n",
            if ("ba" %in% missing_supported) "ba",
            if ("hd" %in% missing_supported) "Hd"
        )

        var_needed <- unique(c(metric_var %||% character(0), auto_var_needed))

        if ("Hd" %in% var_needed)
            var_needed <- unique(c(var_needed, "d", "h", "n"))

        drop_cols <- tolower(c(
            "d", "h", "ba", "n", "hd", "v", "vcc", "vsc", "iavc", "vle"
        ))

        existing_keep <- if (inherits(x, c("externalMetrics", "metrics2vol"))) {
            nm <- names(x)
            nm[
                !tolower(nm) %in% drop_cols &
                !grepl("_(source|status|raw_unit|scale|model)$", tolower(nm))
            ]
        } else {
            character(0)
        }

        keep_needed <- unique(c(
            existing_keep,
            metric_keep_cols,
            metric_levels,
            unlist(
                colmap[intersect(names(colmap), c("species", "region", "equation_set"))],
                use.names = FALSE
            )
        ))
        keep_needed <- keep_needed[!is.na(keep_needed) & nzchar(keep_needed)]

        do.call(
            metric_fun,
            c(
                list(
                    x = x,
                    var = var_needed,
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

    x <- maybe_compute_external_metrics(x)

    if (!is.data.frame(x))
        stop("The standardized input must be a data.frame.", call. = FALSE)

    x_orig <- x

    col_d <- resolve_col(x, std_aliases$d, required = "d" %in% required_inputs)
    col_h <- resolve_col(x, std_aliases$h, required = "h" %in% required_inputs)
    col_dnm <- resolve_col(x, std_aliases$dnm, required = "dnm" %in% required_inputs)
    col_v <- resolve_col(x, std_aliases$v, required = "v" %in% required_inputs)

    ctx_names <- setdiff(names(colmap), c("d", "h", "dnm", "v"))
    ctx_cols <- setNames(vector("list", length(ctx_names)), ctx_names)
    for (nm in ctx_names)
        ctx_cols[[nm]] <- resolve_col(x, colmap[[nm]], required = FALSE)

    required_unit_cols <- c(
        if ("d" %in% required_inputs) col_d,
        if ("h" %in% required_inputs) col_h,
        if ("dnm" %in% required_inputs) col_dnm,
        if ("v" %in% required_inputs) col_v
    )
    require_units_for(x, unique(required_unit_cols))

    units_now <- get_units_map(x)
    has_unit_col <- function(col) !is.null(col) && col %in% names(units_now)

    if (has_unit_col(col_d))
        x <- convert_col(x, col_d, "mm")
    if (has_unit_col(col_h))
        x <- convert_col(x, col_h, "m")
    if (has_unit_col(col_dnm))
        x <- convert_col(x, col_dnm, "mm")
    if (has_unit_col(col_v))
        x <- convert_col(x, col_v, "m3")

    out <- x_orig

    keep_out <- unique(vapply(
        parametro,
        function(z) method_registry[[z]]$output %||% tolower(z),
        character(1)
    ))

    for (nm_out in keep_out)
        out[[nm_out]] <- NA_real_

    if (track_provenance) {
        for (nm_out in keep_out) {
            out[[paste0(nm_out, "_source")]] <- NA_character_
            out[[paste0(nm_out, "_status")]] <- NA_character_
            out[[paste0(nm_out, "_raw_unit")]] <- NA_character_
            out[[paste0(nm_out, "_scale")]] <- NA_real_
            out[[paste0(nm_out, "_model")]] <- NA_character_
        }
    }

    get_method_fun <- function(def) {
        if (is.function(def$fun))
            return(def$fun)

        fn <- def$fun_name %||% NULL
        if (is.null(fn))
            return(NULL)

        get0(fn, mode = "function", inherits = TRUE)
    }

    choose_par_row <- function(p) {
        if (is.null(p) || !nrow(p))
            return(NULL)

        if (selector == "priority" && "priority" %in% tolower(names(p))) {
            pr_col <- names(p)[match("priority", tolower(names(p)))]
            pr_val <- suppressWarnings(as.numeric(as.character(p[[pr_col]])))
            i <- order(pr_val, decreasing = TRUE, na.last = TRUE)[1L]
            return(p[i, , drop = FALSE])
        }

        p[1L, , drop = FALSE]
    }

    get_method_pars <- function(param, ctx, resolved) {
        def <- method_registry[[param]]

        if (is.function(def$get_pars)) {
            p <- def$get_pars(
                ctx = ctx,
                resolved = resolved,
                x = x_orig,
                parameter_table = parameter_table
            )
            if (!is.null(p) && nrow(as.data.frame(p)) > 0)
                return(as.data.frame(p, stringsAsFactors = FALSE))
        }

        p <- def$pars %||% parameter_table
        if (is.null(p))
            return(NULL)

        if (!is.data.frame(p))
            p <- as.data.frame(p, stringsAsFactors = FALSE)

        if (!nrow(p))
            return(NULL)

        nms <- tolower(names(p))

        param_col <- names(p)[match(c("parameter", "parametro", "method"), nms)]
        param_col <- param_col[!is.na(param_col)]
        if (length(param_col)) {
            ii <- toupper(as.character(p[[param_col[1L]]])) == toupper(param)
            if (any(ii))
                p <- p[ii, , drop = FALSE]
        }

        match_by <- def$match_by %||% character(0)
        match_by <- intersect(match_by, names(ctx))

        if (length(match_by)) {
            for (key in match_by) {
                if (!key %in% tolower(names(p)))
                    next
                pcol <- names(p)[match(key, tolower(names(p)))]
                val <- ctx[[key]]
                if (is.null(val) || length(val) != 1L || is.na(val) || !nzchar(as.character(val)))
                    next
                ii <- as.character(p[[pcol]]) == as.character(val)
                if (any(ii))
                    p <- p[ii, , drop = FALSE]
            }
        }

        if (is.function(def$filter_pars))
            p <- def$filter_pars(p = p, ctx = ctx, resolved = resolved)

        choose_par_row(p)
    }

    eval_method <- function(param, ctx, resolved) {
        def <- method_registry[[param]]

        mk <- function(value = NA_real_,
                       source = NA_character_,
                       status = NA_character_,
                       raw_unit = def$raw_unit %||% NA_character_,
                       scale = def$scale_to_m3 %||% 1,
                       model = NA_character_) {
            list(
                value = value,
                source = source,
                status = status,
                raw_unit = raw_unit,
                scale = scale,
                model = model
            )
        }

        fun <- get_method_fun(def)
        if (is.null(fun)) {
            fb <- def$fallback(ctx, NULL, resolved)
            return(mk(
                value = fb,
                source = if (!is.na(fb)) "fallback" else "missing",
                status = "no_function"
            ))
        }

        pars <- get_method_pars(param, ctx, resolved)
        if (is.null(pars)) {
            fb <- def$fallback(ctx, NULL, resolved)
            return(mk(
                value = fb,
                source = if (!is.na(fb)) "fallback" else "missing",
                status = "no_parameters"
            ))
        }

        model_val <- if ("model" %in% tolower(names(pars))) {
            pars[[names(pars)[match("model", tolower(names(pars)))]]][1L]
        } else {
            NA_character_
        }

        args <- def$build_args(ctx, pars, resolved)
        if (is.null(args)) {
            fb <- def$fallback(ctx, pars, resolved)
            return(mk(
                value = fb,
                source = if (!is.na(fb)) "fallback" else "missing",
                status = "no_arguments",
                model = as.character(model_val)
            ))
        }

        err_msg <- NULL
        val_raw <- tryCatch(
            do.call(fun, args),
            error = function(e) {
                err_msg <<- conditionMessage(e)
                NA_real_
            }
        )

        val_raw <- suppressWarnings(as.numeric(val_raw)[1L])
        if (!length(val_raw) || is.na(val_raw)) {
            fb <- def$fallback(ctx, pars, resolved)
            return(mk(
                value = fb,
                source = if (!is.na(fb)) "fallback" else "missing",
                status = if (is.null(err_msg)) "returned_na" else paste0("error: ", err_msg),
                model = as.character(model_val)
            ))
        }

        mk(
            value = val_raw * (def$scale_to_m3 %||% 1),
            source = "equation",
            status = "ok",
            model = as.character(model_val)
        )
    }

    d_mm <- if (!is.null(col_d) && col_d %in% names(x)) {
        suppressWarnings(as.numeric(as.character(x[[col_d]])))
    } else {
        rep(NA_real_, nrow(x))
    }

    h_m <- if (!is.null(col_h) && col_h %in% names(x)) {
        suppressWarnings(as.numeric(as.character(x[[col_h]])))
    } else {
        rep(NA_real_, nrow(x))
    }

    dnm_mm <- if (!is.null(col_dnm) && col_dnm %in% names(x)) {
        suppressWarnings(as.numeric(as.character(x[[col_dnm]])))
    } else {
        rep(NA_real_, nrow(x))
    }

    v_m3 <- if (!is.null(col_v) && col_v %in% names(x)) {
        suppressWarnings(as.numeric(as.character(x[[col_v]])))
    } else {
        rep(NA_real_, nrow(x))
    }

    eval_v <- "V" %in% parametro && method_has_equation("V")

    param_order <- unique(c(
        if (eval_v) "V",
        "VCC",
        setdiff(parametro, c("V", "VCC"))
    ))
    param_order <- intersect(param_order, names(method_registry))

    if ("V" %in% parametro && !eval_v) {
        out_nm <- method_registry[["V"]]$output %||% "v"
        out[[out_nm]] <- v_m3

        if (track_provenance) {
            out[[paste0(out_nm, "_source")]] <- ifelse(is.na(v_m3), "missing", "input")
            out[[paste0(out_nm, "_status")]] <- ifelse(is.na(v_m3), "unavailable", "ok")
            out[[paste0(out_nm, "_raw_unit")]] <- "m3"
            out[[paste0(out_nm, "_scale")]] <- 1
            out[[paste0(out_nm, "_model")]] <- NA_character_
        }
    }

    for (i in seq_len(nrow(out))) {
        ctx <- list(
            d_mm = d_mm[i],
            h_m = h_m[i],
            dnm_mm = dnm_mm[i],
            preexisting_v_m3 = v_m3[i]
        )

        for (nm in names(ctx_cols)) {
            col_i <- ctx_cols[[nm]]
            ctx[[nm]] <- if (is.null(col_i)) NA else x_orig[[col_i]][i]
        }

        resolved <- list(
            preexisting_v_m3 = v_m3[i],
            vcc_m3 = NA_real_
        )

        if ("V" %in% param_order) {
            res_v <- eval_method("V", ctx, resolved)
            resolved$preexisting_v_m3 <- res_v$value
            if ("V" %in% parametro) {
                out_nm <- method_registry[["V"]]$output
                out[[out_nm]][i] <- res_v$value

                if (track_provenance) {
                    out[[paste0(out_nm, "_source")]][i] <- res_v$source
                    out[[paste0(out_nm, "_status")]][i] <- res_v$status
                    out[[paste0(out_nm, "_raw_unit")]][i] <- res_v$raw_unit
                    out[[paste0(out_nm, "_scale")]][i] <- res_v$scale
                    out[[paste0(out_nm, "_model")]][i] <- res_v$model
                }
            }
        }

        if ("VCC" %in% param_order) {
            res_vcc <- eval_method("VCC", ctx, resolved)
            resolved$vcc_m3 <- res_vcc$value

            if ("VCC" %in% parametro) {
                out_nm <- method_registry[["VCC"]]$output
                out[[out_nm]][i] <- res_vcc$value

                if (track_provenance) {
                    out[[paste0(out_nm, "_source")]][i] <- res_vcc$source
                    out[[paste0(out_nm, "_status")]][i] <- res_vcc$status
                    out[[paste0(out_nm, "_raw_unit")]][i] <- res_vcc$raw_unit
                    out[[paste0(out_nm, "_scale")]][i] <- res_vcc$scale
                    out[[paste0(out_nm, "_model")]][i] <- res_vcc$model
                }
            }
        }

        rest <- setdiff(param_order, c("V", "VCC"))
        for (param in rest) {
            res <- eval_method(param, ctx, resolved)
            out_nm <- method_registry[[param]]$output
            out[[out_nm]][i] <- res$value

            if (track_provenance) {
                out[[paste0(out_nm, "_source")]][i] <- res$source
                out[[paste0(out_nm, "_status")]][i] <- res$status
                out[[paste0(out_nm, "_raw_unit")]][i] <- res$raw_unit
                out[[paste0(out_nm, "_scale")]][i] <- res$scale
                out[[paste0(out_nm, "_model")]][i] <- res$model
            }
        }
    }

    units_orig <- get_units_map(x_orig)
    units_keep <- units_orig[names(units_orig) %in% names(out)]

    vol_units <- vapply(
        keep_out,
        function(out_nm) {
            hit <- which(vapply(
                method_registry,
                function(z) identical(z$output %||% NA_character_, out_nm),
                logical(1)
            ))[1L]

            if (is.na(hit))
                return("m3")

            method_registry[[hit]]$unit %||% "m3"
        },
        character(1)
    )
    names(vol_units) <- keep_out

    units_out <- c(units_keep, vol_units)
    units_out <- units_out[!duplicated(names(units_out), fromLast = TRUE)]
    units_out <- units_out[intersect(names(out), names(units_out))]
    attr(out, "units") <- units_out

    design_meta <- attr(x_orig, "design_meta")
    if (!is.null(design_meta))
        attr(out, "design_meta") <- design_meta

    if (track_provenance) {
        attr(out, "volume_meta") <- list(
            input_units = get_units_map(x_orig),
            normalized_input_units = c(d = "mm", h = "m", dnm = "mm", v = "m3"),
            returned_units = vol_units,
            required_inputs = required_inputs,
            methods = lapply(
                parametro,
                function(param) {
                    def <- method_registry[[param]]
                    list(
                        param = param,
                        output = def$output %||% tolower(param),
                        raw_unit = def$raw_unit %||% NA_character_,
                        returned_unit = def$unit %||% "m3",
                        scale_to_m3 = def$scale_to_m3 %||% 1,
                        match_by = def$match_by %||% character(0),
                        required_inputs = def$required_inputs %||% NULL
                    )
                }
            )
        )
        names(attr(out, "volume_meta")$methods) <- parametro
    }

    class(out) <- unique(c("externalMetrics2Vol", "metrics2vol", class(out)))
    out
}, ex = function() {
    sq_0.1ha <- new_inventory_design(
        sample_area_m2 = 1000,
        min_dbh_cm = 7.5,
        name = "Square 0.1-ha plot"
    )

    x <- data.frame(
        plot = c("P1", "P1", "P1"),
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

    externalMetrics2Vol(
        x,
        parametro = "V",
        parameter_table = pars,
        method_registry = ext_methods,
        design = sq_0.1ha,
        metric_levels = c("plot", "species"),
        metric_colmap = list(d = "Diameter", h = "Height"),
        metric_d_unit = "mm",
        metric_h_unit = "m",
        track_provenance = TRUE
    )
})
