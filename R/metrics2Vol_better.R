`%||%` <- function(x, y) if (is.null(x)) y else x

# Default metadata used to dispatch SNFI cubication functions.
default_snfi_volume_equations <- function() {
    list(
        V = list(
            output = "v",
            fun_name = NULL,
            unit = "m3",
            build_args = function(ctx, pars, resolved) list(),
            fallback = function(ctx, pars, resolved) resolved$legacy_v_m3 %||% NA_real_
        ),
        VCC = list(
            output = "vcc",
            fun_name = "get_snfi_vcc",
            unit = "m3",
            build_args = function(ctx, pars, resolved) {
                list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
            },
            fallback = function(ctx, pars, resolved) resolved$legacy_v_m3 %||% NA_real_
        ),
        VSC = list(
            output = "vsc",
            fun_name = "get_snfi_vsc",
            unit = "m3",
            build_args = function(ctx, pars, resolved) {
                vcc_m3 <- resolved$vcc_m3
                if (is.null(vcc_m3) || is.na(vcc_m3))
                    vcc_m3 <- resolved$legacy_v_m3
                if (is.null(vcc_m3) || is.na(vcc_m3))
                    return(NULL)
                list(vcc = vcc_m3 * 1000, pars = pars)
            },
            fallback = function(ctx, pars, resolved) NA_real_
        ),
        IAVC = list(
            output = "iavc",
            fun_name = "get_snfi_iavc",
            unit = "m3",
            build_args = function(ctx, pars, resolved) {
                vcc_m3 <- resolved$vcc_m3
                if (is.null(vcc_m3) || is.na(vcc_m3))
                    vcc_m3 <- resolved$legacy_v_m3
                list(
                    dbh_mm = ctx$d_mm,
                    dnm_mm = ctx$dnm_mm,
                    h_t = ctx$h_m,
                    vcc = if (is.null(vcc_m3) || is.na(vcc_m3)) NULL else vcc_m3 * 1000,
                    pars = pars
                )
            },
            fallback = function(ctx, pars, resolved) NA_real_
        ),
        VLE = list(
            output = "vle",
            fun_name = "get_snfi_vle",
            unit = "m3",
            build_args = function(ctx, pars, resolved) {
                vcc_m3 <- resolved$vcc_m3
                if (is.null(vcc_m3) || is.na(vcc_m3))
                    vcc_m3 <- resolved$legacy_v_m3
                list(
                    dbh_mm = ctx$d_mm,
                    vcc = if (is.null(vcc_m3) || is.na(vcc_m3)) NULL else vcc_m3 * 1000,
                    pars = pars
                )
            },
            fallback = function(ctx, pars, resolved) NA_real_
        )
    )
}

# Build the registry from an object named `snfi_volume_equations` when it exists.
# This lets you keep the dispatch map next to the equation functions.
snfi_volume_method_registry <- function(equations = get0("snfi_volume_equations",
                                                          inherits = TRUE,
                                                          ifnotfound = NULL)) {
    defaults <- default_snfi_volume_equations()

    if (is.null(equations))
        equations <- defaults

    if (!is.list(equations) || is.null(names(equations)))
        stop("'snfi_volume_equations' must be a named list.", call. = FALSE)

    extra <- getOption("basifoR.snfi_volume_methods")
    if (!is.null(extra)) {
        if (!is.list(extra) || is.null(names(extra)))
            stop("Option 'basifoR.snfi_volume_methods' must be a named list.",
                 call. = FALSE)
        equations <- utils::modifyList(equations, extra)
    }

    utils::modifyList(defaults, equations)
}

metrics2Vol_better <- structure(function(
    nfi,
    cub.met = "freq",
    parametro = c("VCC"),
    keep.var = FALSE,
    keep.legacy = TRUE,
    method_registry = snfi_volume_method_registry(),
    ...
) {
    nfi. <- nfi
    if (is.null(nfi.))
        return(nfi)

    if (!inherits(nfi., "nfiMetrics"))
        nfi <- nfiMetrics(nfi, ...)

    nfi_nr <- attr(nfi, "nfi.nr")

if (is.null(nfi_nr) || length(nfi_nr) != 1L || is.na(nfi_nr)) {
    nm0 <- names(nfi)
    nm  <- tolower(nm0)
    i_nfi <- match("nfi.nr", nm)

    if (!is.na(i_nfi)) {
        vals <- unique(stats::na.omit(nfi[[nm0[i_nfi]]]))
        if (length(vals) == 1L)
            nfi_nr <- vals[1L]
    }
}

if (is.null(nfi_nr) || length(nfi_nr) != 1L || is.na(nfi_nr))
    stop("Could not determine 'nfi.nr' from attribute or column.", call. = FALSE)
    
    nfi_orig <- nfi

    nm0 <- names(nfi)
    nm <- tolower(nm0)

    pick_col <- function(candidates, required = TRUE) {
        ii <- match(tolower(candidates), nm)
        ii <- ii[!is.na(ii)]
        if (!length(ii)) {
            if (required)
                stop("Missing required column: ",
                     paste(candidates, collapse = " / "),
                     call. = FALSE)
            return(NULL)
        }
        nm0[ii[1L]]
    }

    fc_match <- function(dt, cl.) {
        nt <- paste(cl., collapse = "|")
        ii <- grep(nt, names(dt), ignore.case = TRUE)
        sort(names(dt)[ii], decreasing = TRUE)
    }

    col_spec <- pick_col(c("Especie", "especie", "codigo_especie", "spec"))
    col_pr   <- pick_col(c("pr", "codigo_provincia"))
    col_d    <- pick_col(c("d"))
    col_h    <- pick_col(c("h"), required = FALSE)
    col_dnm  <- pick_col(c("D.n.m.", "dnm", "d_nm"), required = FALSE)

    if (!is.null(col_d))
        nfi <- conv_units(nfi, var = col_d, un = "mm")
    if (!is.null(col_h))
        nfi <- conv_units(nfi, var = col_h, un = "m")
    if (!is.null(col_dnm))
        nfi <- conv_units(nfi, var = col_dnm, un = "mm")

    parametro <- unique(toupper(parametro))
    valid_param <- unique(toupper(names(method_registry)))
    bad_param <- setdiff(parametro, valid_param)
    if (length(bad_param))
        stop("Unknown parametro value(s): ",
             paste(bad_param, collapse = ", "),
             call. = FALSE)

    out <- nfi_orig
    ## out <- nfi
    keep_param <- unique(c(if (keep.legacy) "V", parametro))
    requested_outputs <- unique(vapply(
        keep_param,
        function(x) method_registry[[x]]$output %||% tolower(x),
        character(1)
    ))
    for (nm_out in requested_outputs)
        out[[nm_out]] <- NA_real_

    warn_msg <- character(0)

compute_legacy_v <- function(nfi_input, cub.met = "freq", nfi.nr = NULL) {
    if (!exists("metrics2Vol", mode = "function", inherits = TRUE))
        return(rep(NA_real_, nrow(nfi_input)))

    leg <- nfi_input
    leg$.rowid_legacy <- seq_len(nrow(leg))
    if (!is.null(nfi.nr))
        attr(leg, "nfi.nr") <- nfi.nr

    old <- tryCatch(
        metrics2Vol(leg, cub.met = cub.met, keep.var = FALSE),
        error = function(e) {
            stop("Legacy metrics2Vol failed: ", conditionMessage(e), call. = FALSE)
        }
    )

    if (is.null(old) || !"v" %in% names(old))
        return(rep(NA_real_, nrow(leg)))

    id_col <- names(old)[tolower(names(old)) == ".rowid_legacy"]
    if (!length(id_col))
        stop("Legacy metrics2Vol did not preserve '.rowid_legacy'.", call. = FALSE)

    out_v <- rep(NA_real_, nrow(leg))
    idx <- match(seq_len(nrow(leg)), old[[id_col[1L]]])
    ok <- !is.na(idx)
    out_v[ok] <- suppressWarnings(as.numeric(old[["v"]][idx[ok]]))
    out_v
}
    
    ## compute_legacy_v <- function(nfi_input, cub.met = "freq") {
    ##     nm_leg0 <- names(nfi_input)
    ##     nm_leg <- tolower(nm_leg0)
    ##     get_leg_col <- function(candidates) {
    ##         ii <- match(tolower(candidates), nm_leg)
    ##         ii <- ii[!is.na(ii)]
    ##         if (!length(ii))
    ##             return(NULL)
    ##         nm_leg0[ii[1L]]
    ##     }

    ##     spec_legacy <- nm_leg0[
    ##         grepl("spec", nm_leg0, ignore.case = TRUE) |
    ##         grepl("espec", nm_leg0, ignore.case = TRUE)
    ##     ]
    ##     col_pr_leg <- get_leg_col(c("pr", "codigo_provincia"))
    ##     col_d_leg  <- get_leg_col(c("d"))
    ##     col_h_leg  <- get_leg_col(c("h"))

    ##     if (is.null(col_pr_leg) || is.null(col_d_leg) || is.null(col_h_leg) || !length(spec_legacy))
    ##         return(rep(NA_real_, nrow(nfi_input)))
    ##     if (!exists("parEqVcc", inherits = TRUE))
    ##         return(rep(NA_real_, nrow(nfi_input)))

    ##     nfi_leg <- nfi_input
    ##     nfi_leg$.rowid_metrics2vol <- seq_len(nrow(nfi_leg))
    ##     if (col_pr_leg != "pr") nfi_leg$pr <- nfi_leg[[col_pr_leg]]
    ##     if (col_d_leg  != "d")  nfi_leg$d  <- nfi_leg[[col_d_leg]]
    ##     if (col_h_leg  != "h")  nfi_leg$h  <- nfi_leg[[col_h_leg]]
    ##     nfi_leg <- conv_units(nfi_leg, var = c("d", "h"), un = c("mm", "dm"))

    ##     mds <- c(
    ##         "1"  = "v ~ par1 + par2 * (d^2) * h",
    ##         "11" = "v ~ par1 * (d^par2) * (h^par3)"
    ##     )

    ##     fmdV <- function(mdb2, ntm = c("pr", "spec")) {
    ##         merge(mdb2, parEqVcc,
    ##               by.x = fc_match(mdb2, ntm),
    ##               by.y = fc_match(parEqVcc, ntm),
    ##               all.x = TRUE)
    ##     }

    ##     fvarin <- function(fun, ind = TRUE) {
    ##         fun <- formula(fun)
    ##         allv <- all.vars(fun, functions = FALSE)
    ##         yvar <- all.vars(update(fun, . ~ 1), functions = FALSE)
    ##         inds <- allv[!allv %in% yvar]
    ##         if (!ind)
    ##             inds <- yvar
    ##         inds
    ##     }

    ##     fev <- function(fun, md) {
    ##         e <- list2env(as.list(md))
    ##         eval(parse(text = fun), e)
    ##     }

    ##     feV <- function(vt, md) {
    ##         ind <- fvarin(md)
    ##         dep <- fvarin(md, FALSE)
    ##         sbs <- paste(dep, "~", sep = "|")
    ##         md2 <- gsub(sbs, "", md)
    ##         md2 <- gsub(" ", "", md2)
    ##         vt2 <- vt[, ind, drop = FALSE]
    ##         vl <- apply(vt2, 1, function(x) fev(md2, x))
    ##         vl <- cbind(vt, vl)
    ##         names(vl) <- c(names(vt), dep)
    ##         vl
    ##     }

    ##     vt <- fmdV(nfi_leg)
    ##     vt <- vt[!is.na(vt$Modelo), , drop = FALSE]
    ##     if (!nrow(vt))
    ##         return(rep(NA_real_, nrow(nfi_input)))

    ##     spm <- split(vt, vt[, "Modelo"])
    ##     nms_mod <- names(spm)
    ##     mds_use <- mds[nms_mod]
    ##     ok_mod <- !is.na(mds_use)
    ##     spm <- spm[ok_mod]
    ##     mds_use <- mds_use[ok_mod]
    ##     if (!length(spm))
    ##         return(rep(NA_real_, nrow(nfi_input)))

    ##     mmod <- Map(function(x, y) feV(x, y), x = spm, y = mds_use)
    ##     mmd <- do.call("rbind", mmod)
    ##     if (!nrow(mmd))
    ##         return(rep(NA_real_, nrow(nfi_input)))

    ##     ffreq <- function(df) {
    ##         tm <- data.frame(table(df$fc))
    ##         tm <- subset(tm, Freq %in% max(Freq))
    ##         as.character(tm$Var1)[1L]
    ##     }

    ##     cub.met.use <- cub.met
    ##     if (identical(cub.met.use, "freq"))
    ##         cub.met.use <- ffreq(mmd)

    ##     mmd <- subset(mmd, fc %in% as.factor(cub.met.use))
    ##     if (!nrow(mmd))
    ##         return(rep(NA_real_, nrow(nfi_input)))

    ##     vv <- mmd[, c(".rowid_metrics2vol", "v"), drop = FALSE]
    ##     vv <- conv_units(vv, var = "v", un = "m3")

    ##     out_v <- rep(NA_real_, nrow(nfi_input))
    ##     idx <- match(seq_len(nrow(nfi_input)), vv$.rowid_metrics2vol)
    ##     ok <- !is.na(idx)
    ##     out_v[ok] <- vv$v[idx[ok]]
    ##     out_v
    ## }

    can_compute_legacy <- all(c("pr", "d", "h") %in% tolower(names(nfi_orig))) &&
        any(grepl("spec|espec", names(nfi_orig), ignore.case = TRUE))

    need_legacy <- keep.legacy || any(parametro %in% c("V", "VCC", "VSC", "IAVC", "VLE"))
    ## legacy_v_m3 <- rep(NA_real_, nrow(out))
    ## if (need_legacy && can_compute_legacy) {
    ##     legacy_v_m3 <- compute_legacy_v(nfi_orig, cub.met = cub.met)
    ## } else if (need_legacy) {
    ##     warn_msg <- c(warn_msg, "Legacy method not available: missing species, pr, d and/or h.")
    ## }

legacy_v_m3 <- rep(NA_real_, nrow(out))
if (need_legacy && can_compute_legacy) {
    legacy_v_m3 <- compute_legacy_v(
        nfi_input = nfi_orig,
        cub.met = cub.met,
        nfi.nr = nfi_nr
    )
} else if (need_legacy) {
    warn_msg <- c(warn_msg, "Legacy method not available: missing species, pr, d and/or h.")
}
    
    if (keep.legacy || "V" %in% parametro)
        out[[method_registry[["V"]]$output]] <- legacy_v_m3

    can_use_new <- !is.null(col_pr) &&
        !is.null(col_spec) &&
        exists("SNFI43_all_volume_coefficients", inherits = TRUE)

    if (can_use_new) {
        coef_tab <- SNFI43_all_volume_coefficients
        coef_names <- names(coef_tab)
        coef_names_lc <- tolower(coef_names)

        coef_col <- function(candidates) {
            ii <- match(tolower(candidates), coef_names_lc)
            ii <- ii[!is.na(ii)]
            if (!length(ii))
                return(NULL)
            coef_names[ii[1L]]
        }

        coef_col_nfi   <- coef_col(c("nfi.nr"))
        coef_col_pr    <- coef_col(c("pr", "codigo_provincia"))
        coef_col_param <- coef_col(c("Parametro", "parametro"))
        coef_col_specn <- coef_col(c("Especie", "especie", "codigo_especie"))
        coef_col_fc    <- coef_col(c("F.c.", "fc"))
        coef_col_model <- coef_col(c("Modelo", "modelo"))

        if (!is.null(coef_col_param))
            coef_tab[[coef_col_param]] <- toupper(as.character(coef_tab[[coef_col_param]]))

        match_coef_rows <- function(pr, especie, param = NULL, cub.met = "freq") {
            if (is.null(coef_col_nfi) || is.null(coef_col_pr))
                return(coef_tab[0, , drop = FALSE])

            x <- coef_tab[
                coef_tab[[coef_col_nfi]] == nfi_nr &
                coef_tab[[coef_col_pr]] == suppressWarnings(as.numeric(as.character(pr))),
                , drop = FALSE
            ]
            if (!nrow(x))
                return(x)

            sp_num <- suppressWarnings(as.numeric(as.character(especie)))
            if (!is.na(sp_num) && !is.null(coef_col_specn)) {
                y <- x[x[[coef_col_specn]] == sp_num, , drop = FALSE]
                if (nrow(y))
                    x <- y
            }

            if (!is.null(param) && !is.null(coef_col_param)) {
                y <- x[x[[coef_col_param]] == toupper(param), , drop = FALSE]
                if (nrow(y))
                    x <- y
            }

            # Never use Estadillo from nfiMetrics as if it were coefficient F.c.
            # Only use cub.met when the user explicitly requests a specific form.
            if (nrow(x) > 1L && !is.null(coef_col_fc) && !identical(cub.met, "freq")) {
                y <- x[as.character(x[[coef_col_fc]]) == as.character(cub.met), , drop = FALSE]
                if (nrow(y))
                    x <- y
            }

            x
        }

        get_method_fun <- function(def) {
            fn <- def$fun_name
            if (is.null(fn))
                return(NULL)
            get0(fn, mode = "function", inherits = TRUE)
        }

        eval_method <- function(param, ctx, resolved) {
            def <- method_registry[[param]]
            fun <- get_method_fun(def)
            if (is.null(fun))
                return(def$fallback(ctx, NULL, resolved))

            pars <- match_coef_rows(
                pr = ctx$pr,
                especie = ctx$especie,
                param = param,
                cub.met = cub.met
            )
            if (!nrow(pars))
                return(def$fallback(ctx, NULL, resolved))

            pars <- pars[1L, , drop = FALSE]
            if (!is.null(coef_col_model) && !"Modelo" %in% names(pars))
                pars$Modelo <- pars[[coef_col_model]]

            args <- def$build_args(ctx, pars, resolved)
            if (is.null(args))
                return(def$fallback(ctx, pars, resolved))

            val <- tryCatch(
                do.call(fun, args),
                error = function(e) NA_real_
            )
            val <- suppressWarnings(as.numeric(val)[1L])
            if (!length(val) || is.na(val))
                return(def$fallback(ctx, pars, resolved))

            val / 1000
        }

        param_order <- unique(c("VCC", setdiff(parametro, c("V", "VCC"))))
        param_order <- intersect(param_order, setdiff(names(method_registry), "V"))

        for (i in seq_len(nrow(out))) {
            ## ctx <- list(
            ##     pr = nfi_orig[[col_pr]][i],
            ##     especie = nfi_orig[[col_spec]][i],
            ##     d_mm = if (!is.null(col_d)) suppressWarnings(as.numeric(out[[col_d]][i])) else NULL,
            ##     h_m = if (!is.null(col_h)) suppressWarnings(as.numeric(out[[col_h]][i])) else NULL,
            ##     dnm_mm = if (!is.null(col_dnm)) suppressWarnings(as.numeric(out[[col_dnm]][i])) else NULL
            ## )
ctx <- list(
    pr = nfi_orig[[col_pr]][i],
    especie = nfi_orig[[col_spec]][i],
    d_mm = if (!is.null(col_d)) suppressWarnings(as.numeric(nfi[[col_d]][i])) else NULL,
    h_m = if (!is.null(col_h)) suppressWarnings(as.numeric(nfi[[col_h]][i])) else NULL,
    dnm_mm = if (!is.null(col_dnm)) suppressWarnings(as.numeric(nfi[[col_dnm]][i])) else NULL
)
            resolved <- list(
                legacy_v_m3 = legacy_v_m3[i],
                vcc_m3 = NA_real_
            )

            if ("VCC" %in% param_order) {
                vcc_val <- eval_method("VCC", ctx, resolved)
                resolved$vcc_m3 <- vcc_val
                if ("VCC" %in% parametro)
                    out[[method_registry[["VCC"]]$output]][i] <- vcc_val
            }

            rest <- setdiff(param_order, "VCC")
            for (param in rest) {
                val <- eval_method(param, ctx, resolved)
                out[[method_registry[[param]]$output]][i] <- val
            }
        }
    } else {
        warn_msg <- c(warn_msg,
                      "SNFI43 coefficients not used: missing matching variables or coefficient table not available.")
    }

    if (!keep.var) {
        drop_cols <- intersect(
            c("par1", "par2", "par3", "fc", "parametro",
              "codigo_provincia", "nombre_provincia", "codigo_especie",
              "f.c.", "a", "b", "c", "d", "p", "q", "r", "r2", "par_esp"),
            names(out)
        )
        if (length(drop_cols))
            out <- out[, !names(out) %in% drop_cols, drop = FALSE]
    }

##     keep_out <- unique(vapply(keep_param, function(x) method_registry[[x]]$output, character(1)))
##     vol_cols_all <- unique(vapply(method_registry, function(x) x$output, character(1)))
##     drop_vol <- intersect(tolower(names(out)), tolower(vol_cols_all))
##     drop_vol <- setdiff(drop_vol, tolower(keep_out))
##     if (length(drop_vol))
##         out <- out[, !tolower(names(out)) %in% drop_vol, drop = FALSE]

##     units_orig <- attr(nfi_orig, "units")
##     out_cols <- intersect(keep_out, names(out))
##     new_units <- setNames(rep("m3", length(out_cols)), out_cols)
##     attr(out, "units") <- c(units_orig, new_units)

##     rownames(out) <- NULL
##     n <- names(out)
##     first <- c("nfi.nr", "pr", "estadillo", "especie")
##     i <- match(first, tolower(n))
##     i <- i[!is.na(i)]
##     out <- out[, c(n[i], n[-i]), drop = FALSE]

    
##     attr(out, "nfi.nr") <- nfi_nr
##     class(out) <- append("metrics2vol", class(out))

##     if (length(warn_msg))
##         warning(paste(unique(warn_msg), collapse = "\n"), call. = FALSE)

##     out
## })

        keep_out <- unique(vapply(
        keep_param,
        function(x) method_registry[[x]]$output %||% tolower(x),
        character(1)
    ))

    vol_cols_all <- unique(vapply(
        method_registry,
        function(x) x$output %||% NA_character_,
        character(1)
    ))
    vol_cols_all <- vol_cols_all[!is.na(vol_cols_all)]

    drop_vol <- intersect(tolower(names(out)), tolower(vol_cols_all))
    drop_vol <- setdiff(drop_vol, tolower(keep_out))
    if (length(drop_vol))
        out <- out[, !tolower(names(out)) %in% drop_vol, drop = FALSE]

    rownames(out) <- NULL
    n <- names(out)
    first <- c("nfi.nr", "pr", "estadillo", "especie")
    i <- match(first, tolower(n))
    i <- i[!is.na(i)]
    out <- out[, c(n[i], n[-i]), drop = FALSE]

    ## rebuild units from surviving nfiMetrics columns + returned volume outputs
    units_orig <- attr(nfi_orig, "units")
    if (is.null(units_orig))
        units_orig <- setNames(character(0), character(0))

    ## nfiMetrics stores units as: values = variable names, names = units
    ## convert to the more convenient form: names = variable names, values = units
    if (length(units_orig)) {
        units_keep <- setNames(names(units_orig), as.character(units_orig))
        units_keep <- units_keep[names(units_keep) %in% names(out)]
    } else {
        units_keep <- setNames(character(0), character(0))
    }

    ## add units for returned computed outputs
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

    ## computed outputs override any same-named original entry
    units_out <- c(units_keep, vol_units)
    units_out <- units_out[!duplicated(names(units_out), fromLast = TRUE)]

    ## keep only units for columns that are present, in output order
    units_out <- units_out[intersect(names(out), names(units_out))]

    attr(out, "units") <- units_out
    
    ## ## rebuild units from original nfiMetrics units + returned volume outputs
    ## units_orig <- attr(nfi_orig, "units")
    ## if (is.null(units_orig))
    ##     units_orig <- setNames(character(0), character(0))

    ## vol_units <- vapply(
    ##     keep_out,
    ##     function(out_nm) {
    ##         hit <- which(vapply(
    ##             method_registry,
    ##             function(z) identical(z$output %||% NA_character_, out_nm),
    ##             logical(1)
    ##         ))[1L]

    ##         if (is.na(hit))
    ##             return("m3")

    ##         method_registry[[hit]]$unit %||% "m3"
    ##     },
    ##     character(1)
    ## )
    ## names(vol_units) <- keep_out

    ## units_out <- c(units_orig, vol_units)
    ## units_out <- units_out[match(names(out), names(units_out))]
    ## names(units_out) <- names(out)

    ## attr(out, "units") <- units_out
    attr(out, "nfi.nr") <- nfi_nr
    class(out) <- append("metrics2vol", class(out))

    if (length(warn_msg))
        warning(paste(unique(warn_msg), collapse = "\n"), call. = FALSE)

    out
})
