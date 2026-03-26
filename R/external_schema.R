# Schema support patch for external basifoR workflow

`%||%` <- function(a, b) if (is.null(a)) b else a

new_external_schema <- function(
    colmap,
    units = list(),
    levels = NULL,
    keep_cols = NULL,
    defaults = list()
) {
    if (!is.list(colmap) || !length(colmap)) {
        stop("'colmap' must be a non-empty named list.", call. = FALSE)
    }
    if (is.null(names(colmap)) || anyNA(names(colmap)) || any(names(colmap) == "")) {
        stop("'colmap' must be a named list.", call. = FALSE)
    }
    if (!is.list(units)) {
        stop("'units' must be a named list.", call. = FALSE)
    }
    if (length(units) && (is.null(names(units)) || anyNA(names(units)) || any(names(units) == ""))) {
        stop("'units' must be a named list.", call. = FALSE)
    }

    normalize_chr <- function(x) {
        x <- unlist(x, use.names = FALSE)
        x <- as.character(x)
        x[!is.na(x) & nzchar(x)]
    }

    colmap <- lapply(colmap, normalize_chr)
    units <- lapply(units, function(x) as.character(x)[1L])

    schema <- list(
        colmap = colmap,
        units = units,
        levels = normalize_chr(levels),
        keep_cols = normalize_chr(keep_cols),
        defaults = defaults %||% list()
    )
    class(schema) <- c("external_schema", "list")
    schema
}

print.external_schema <- function(x, ...) {
    cat("<external_schema>\n")
    cat("Columns:\n")
    for (nm in names(x$colmap)) {
        cat("  - ", nm, ": ", paste(x$colmap[[nm]], collapse = ", "), "\n", sep = "")
    }
    if (length(x$units)) {
        cat("Units:\n")
        for (nm in names(x$units)) {
            cat("  - ", nm, ": ", x$units[[nm]], "\n", sep = "")
        }
    }
    if (length(x$levels))
        cat("Levels: ", paste(x$levels, collapse = ", "), "\n", sep = "")
    invisible(x)
}

.resolve_external_schema <- function(
    schema = NULL,
    levels = NULL,
    keep_cols = NULL,
    colmap = NULL,
    d_unit = NULL,
    h_unit = NULL,
    metric_colmap = NULL,
    volume_colmap = NULL,
    metric_levels = NULL,
    metric_keep_cols = NULL,
    metric_d_unit = NULL,
    metric_h_unit = NULL
) {
    if (is.null(schema)) {
        return(list(
            levels = levels,
            keep_cols = keep_cols,
            colmap = colmap,
            d_unit = d_unit,
            h_unit = h_unit,
            metric_colmap = metric_colmap,
            volume_colmap = volume_colmap,
            metric_levels = metric_levels,
            metric_keep_cols = metric_keep_cols,
            metric_d_unit = metric_d_unit,
            metric_h_unit = metric_h_unit
        ))
    }

    if (!inherits(schema, "external_schema")) {
        stop("'schema' must inherit from 'external_schema'.", call. = FALSE)
    }

    schema_colmap <- schema$colmap %||% list()
    schema_units <- schema$units %||% list()

    out_colmap <- if (is.null(colmap)) schema_colmap else utils::modifyList(schema_colmap, colmap)
    out_metric_colmap <- if (is.null(metric_colmap)) {
        schema_colmap[names(schema_colmap) %in% c("d", "h")]
    } else {
        utils::modifyList(schema_colmap[names(schema_colmap) %in% c("d", "h")], metric_colmap)
    }
    out_volume_colmap <- if (is.null(volume_colmap)) schema_colmap else utils::modifyList(schema_colmap, volume_colmap)

    list(
        levels = levels %||% schema$levels,
        keep_cols = keep_cols %||% schema$keep_cols,
        colmap = out_colmap,
        d_unit = d_unit %||% schema_units$d,
        h_unit = h_unit %||% schema_units$h,
        metric_colmap = out_metric_colmap,
        volume_colmap = out_volume_colmap,
        metric_levels = metric_levels %||% levels %||% schema$levels,
        metric_keep_cols = metric_keep_cols %||% keep_cols %||% schema$keep_cols,
        metric_d_unit = metric_d_unit %||% d_unit %||% schema_units$d,
        metric_h_unit = metric_h_unit %||% h_unit %||% schema_units$h
    )
}

.externalMetrics_base <- if (exists("externalMetrics", inherits = TRUE)) get("externalMetrics", inherits = TRUE) else NULL
.externalMetrics2Vol_base <- if (exists("externalMetrics2Vol", inherits = TRUE)) get("externalMetrics2Vol", inherits = TRUE) else NULL
.external_dendroMetrics_base <- if (exists("external_dendroMetrics", inherits = TRUE)) get("external_dendroMetrics", inherits = TRUE) else NULL

if (is.null(.externalMetrics_base) || is.null(.externalMetrics2Vol_base) || is.null(.external_dendroMetrics_base)) {
    stop(
        "Load the external workflow first (for example source('external_workflow_bundle_v2.R')) before sourcing this schema patch.",
        call. = FALSE
    )
}

externalMetrics <- local({
    base_fun <- .externalMetrics_base
    function(
        x,
        var = c("d", "h", "ba", "n", "Hd"),
        levels = NULL,
        design,
        colmap = list(
            d = c("d", "dbh", "diameter", "diameter_mm"),
            h = c("h", "height", "height_m")
        ),
        d_unit = c("mm", "cm")[1],
        h_unit = c("m", "dm", "cm")[1],
        keep_cols = NULL,
        domheight_fun = get0("domheight", mode = "function", inherits = TRUE),
        schema = NULL
    ) {
        sch <- .resolve_external_schema(
            schema = schema,
            levels = levels,
            keep_cols = keep_cols,
            colmap = colmap,
            d_unit = d_unit,
            h_unit = h_unit
        )

        base_fun(
            x = x,
            var = var,
            levels = sch$levels,
            design = design,
            colmap = sch$colmap,
            d_unit = sch$d_unit %||% d_unit,
            h_unit = sch$h_unit %||% h_unit,
            keep_cols = sch$keep_cols,
            domheight_fun = domheight_fun
        )
    }
})

externalMetrics2Vol <- local({
    base_fun <- .externalMetrics2Vol_base
    function(
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
        schema = NULL,
        ...
    ) {
        sch <- .resolve_external_schema(
            schema = schema,
            levels = levels,
            keep_cols = keep_cols,
            colmap = colmap,
            d_unit = d_unit,
            h_unit = h_unit,
            metric_colmap = metric_colmap,
            volume_colmap = volume_colmap,
            metric_levels = metric_levels,
            metric_keep_cols = metric_keep_cols,
            metric_d_unit = metric_d_unit,
            metric_h_unit = metric_h_unit
        )

        base_fun(
            x = x,
            parametro = parametro,
            parameter_table = parameter_table,
            method_registry = method_registry,
            colmap = sch$colmap,
            selector = selector,
            track_provenance = track_provenance,
            compute_metrics_if_needed = compute_metrics_if_needed,
            design = design,
            var = var,
            metric_var = metric_var,
            levels = sch$levels,
            metric_levels = sch$metric_levels,
            keep_cols = sch$keep_cols,
            metric_keep_cols = sch$metric_keep_cols,
            metric_colmap = sch$metric_colmap,
            d_unit = sch$d_unit,
            metric_d_unit = sch$metric_d_unit,
            h_unit = sch$h_unit,
            metric_h_unit = sch$metric_h_unit,
            volume_colmap = sch$volume_colmap,
            ...
        )
    }
})

external_dendroMetrics <- local({
    base_fun <- .external_dendroMetrics_base
    function(
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
        schema = NULL,
        ...
    ) {
        sch <- .resolve_external_schema(
            schema = schema,
            levels = levels,
            keep_cols = keep_cols,
            colmap = colmap,
            d_unit = d_unit,
            h_unit = h_unit,
            metric_colmap = metric_colmap,
            volume_colmap = volume_colmap,
            metric_levels = metric_levels,
            metric_keep_cols = metric_keep_cols,
            metric_d_unit = metric_d_unit,
            metric_h_unit = metric_h_unit
        )

        base_fun(
            x = x,
            summ.vr = summ.vr,
            cut.dt = cut.dt,
            report = report,
            mc.cores = mc.cores,
            var = var,
            parametro = parametro,
            design = design,
            parameter_table = parameter_table,
            method_registry = method_registry,
            levels = sch$levels,
            metric_levels = sch$metric_levels,
            keep_cols = sch$keep_cols,
            metric_keep_cols = sch$metric_keep_cols,
            colmap = sch$colmap,
            metric_colmap = sch$metric_colmap,
            d_unit = sch$d_unit,
            metric_d_unit = sch$metric_d_unit,
            h_unit = sch$h_unit,
            metric_h_unit = sch$metric_h_unit,
            volume_colmap = sch$volume_colmap,
            selector = selector,
            track_provenance = track_provenance,
            compute_metrics_if_needed = compute_metrics_if_needed,
            ...
        )
    }
})
