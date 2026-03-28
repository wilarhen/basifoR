external_dendroMetrics <- structure(function # Summarize external inventory tree data and optional volume outputs
### Compute standardized tree metrics from external inventory data and, when requested,
### derive stand-level summaries or volume variables within the external basifoR workflow.
##description<< Process external tree data through a unified workflow that standardizes measurements, computes missing dendrometric variables, optionally derives volume outputs, and returns either tree-level records or grouped stand-level summaries.
##details<< This wrapper mirrors the arguments and behaviour of \code{dendroMetrics()}.
##details<< When \code{summ.vr = NULL}, the function returns tree-level records after applying \code{cut.dt}. When \code{summ.vr} contains one or more grouping variables, the function aggregates by those groups, reporting weighted means for \code{d}, \code{h}, and \code{Hd}, arithmetic sums for \code{ba}, \code{n}, and volume variables, and quadratic mean diameter \code{dg} when diameter and trees-per-hectare are available.
##details<< The function can work from already standardized inputs or from raw external tables. If requested metrics are missing and \code{compute_metrics_if_needed = TRUE}, it calls \code{externalMetrics()} internally, so raw inputs usually need a valid \code{design} plus diameter and, when relevant, height mappings. When \code{schema} is supplied, it provides reusable defaults for column aliases, units, grouping variables, and retained columns, while explicit arguments supplied in the call override those defaults.
##details<< Volume outputs are optional. They are computed only when \code{parametro} is supplied explicitly or can be inferred from \code{var} and \code{method_registry}. This lets the same entry point handle metric-only workflows, mixed metric-plus-volume workflows, and repeated processing of several input tables with optional parallel execution through \code{mc.cores}.
(
    x, ##<< A \code{data.frame}, a processed external basifoR table, or a \code{list} of such objects. Raw inputs must contain enough columns to resolve the requested metrics and, if needed, the volume selectors.
    summ.vr = NULL, ##<< Optional grouping variable or character vector of grouping variables. Leave \code{NULL} to keep tree-level output; supply one or more column names to obtain grouped summaries.
    cut.dt = "d == d", ##<< Character expression evaluated with \code{eval(parse(...))} on the final table. Use it to filter rows after metrics or summaries have been computed.
    report = FALSE, ##<< Logical. If \code{TRUE}, write the returned table to \file{report.csv} in the working directory.
    mc.cores = getOption("mc.cores", 1L), ##<< Integer number of worker processes used when \code{x} is a list. Values smaller than 1 are reset to 1.
    var = c("d", "h", "ba", "n", "Hd"), ##<< Character vector of requested variables. Typical metric requests are \code{"d"}, \code{"h"}, \code{"ba"}, \code{"n"}, and \code{"Hd"}; volume-like names can also trigger volume processing.
    parametro = NULL, ##<< Optional character vector of volume-method codes, for example \code{"V"}. If \code{NULL}, the function tries to infer required methods from \code{var} and \code{method_registry}.
    design = NULL, ##<< An object inheriting from \code{"inventory_design"}. Required when the function must compute missing metrics such as \code{n} from raw external data.
    parameter_table = NULL, ##<< Optional parameter table passed to \code{externalMetrics2Vol()} for method-specific volume coefficients or selection metadata.
    method_registry = external_volume_method_registry(), ##<< Named list of volume methods, usually created by \code{external_volume_method_registry()} or a custom registry following the same structure.
    levels = NULL, ##<< Optional grouping variables forwarded to schema resolution. When \code{schema} is not used, these serve as defaults for workflow grouping metadata.
    metric_levels = NULL, ##<< Optional grouping variables used specifically while computing missing metrics internally with \code{externalMetrics()}.
    keep_cols = NULL, ##<< Optional character vector of source columns to preserve in the output or schema defaults.
    metric_keep_cols = NULL, ##<< Optional character vector of columns to preserve during the internal metric-computation stage.
    colmap = NULL, ##<< Optional named list of column aliases that update the default mappings, especially when source names differ from the expected volume or metric names.
    metric_colmap = list(
        d = c("d", "dbh", "diameter", "diameter_mm"),
        h = c("h", "height", "height_m")
    ), ##<< Named list of aliases used to resolve raw diameter and height columns during internal metric computation.
    d_unit = NULL, ##<< Optional diameter unit override shared with schema resolution. Accepted values are \code{"mm"} and \code{"cm"}.
    metric_d_unit = c("mm", "cm")[1], ##<< Diameter unit expected by \code{metric_colmap} when metrics must be computed internally.
    h_unit = NULL, ##<< Optional height unit override shared with schema resolution. Accepted values are \code{"m"}, \code{"dm"}, and \code{"cm"}.
    metric_h_unit = c("m", "dm", "cm")[1], ##<< Height unit expected by \code{metric_colmap} when metrics must be computed internally.
    tree_d_unit_out = NULL, ##<< Optional output unit for tree-level diameter values when \code{summ.vr = NULL}. Accepted values are \code{"mm"} and \code{"cm"}.
    tree_h_unit_out = NULL, ##<< Optional output unit for tree-level height and dominant-height values when \code{summ.vr = NULL}. Accepted values are \code{"m"}, \code{"dm"}, and \code{"cm"}.
    volume_colmap = list(
        d = c("d"),
        h = c("h"),
        dnm = c("dnm", "d_nm", "D.n.m."),
        v = c("v"),
        species = c("species", "spec", "especie"),
        region = c("region", "pr"),
        equation_set = c("equation_set", "eqset", "tariff", "model_set")
    ), ##<< Named list of aliases used by the optional volume stage to locate metrics, species, region, and equation-set selectors.
    selector = c("first", "priority")[1], ##<< Character string controlling how \code{externalMetrics2Vol()} resolves multiple candidate methods or rows.
    track_provenance = FALSE, ##<< Logical. If \code{TRUE}, keep provenance columns generated by the volume workflow before the final summary stage removes them from grouped output.
    compute_metrics_if_needed = TRUE, ##<< Logical. If \code{TRUE}, compute missing metrics from raw inputs when possible; if \code{FALSE}, stop when required standardized columns are absent.
    schema = NULL, ##<< Optional \code{"external_schema"} object created by \code{new_external_schema()}. It centralizes column aliases, units, grouping defaults, and kept columns for repeated workflows.
    domheight_fun = get0("domheight_strict", mode = "function", inherits = TRUE) %||%
        get0("domheight", mode = "function", inherits = TRUE), ##<< Function used to compute dominant height when \code{"Hd"} is requested during internal metric computation.
    ... ##<< Additional arguments passed to downstream helpers, mainly custom options for internal metric or volume-processing steps.
) {
    ##value<< A \code{data.frame} with class \code{c("external_dendroMetrics", "dendroMetrics", ...)}. With \code{summ.vr = NULL}, the returned rows represent tree-level records, optionally converted to \code{tree_d_unit_out} and \code{tree_h_unit_out}. With \code{summ.vr} supplied, the returned rows represent grouped summaries and include standardized summary units such as \code{"cm"}, \code{"m"}, \code{"m2 ha-1"}, \code{"ha-1"}, and \code{"m3 ha-1"} when those variables are present.
    ##value<< The returned object stores the matched call in \code{attr(x, "call")}. When available, it also preserves \code{"units"}, \code{"design_meta"}, and \code{"volume_meta"} attributes from upstream processing, which makes the result suitable for downstream inspection and update methods.
    external_dendroMetrics(
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
        levels = levels,
        metric_levels = metric_levels,
        keep_cols = keep_cols,
        metric_keep_cols = metric_keep_cols,
        colmap = colmap,
        metric_colmap = metric_colmap,
        d_unit = d_unit,
        metric_d_unit = metric_d_unit,
        h_unit = h_unit,
        metric_h_unit = metric_h_unit,
        tree_d_unit_out = tree_d_unit_out,
        tree_h_unit_out = tree_h_unit_out,
        volume_colmap = volume_colmap,
        selector = selector,
        track_provenance = track_provenance,
        compute_metrics_if_needed = compute_metrics_if_needed,
        schema = schema,
        domheight_fun = domheight_fun,
        ...
    )
},
ex = function() {
    x <- data.frame(
        plot = c("P1", "P1", "P2", "P2"),
        species = c("sp1", "sp2", "sp1", "sp2"),
        dbh_mm = c(120, 185, 260, 140),
        height_m = c(7.1, 9.4, 13.2, 8.0),
        stringsAsFactors = FALSE
    )

    design <- new_inventory_design(
        sample_area_m2 = 1000,
        min_dbh_cm = 7.5,
        name = "Square 0.1-ha plot"
    )

    schema <- new_external_schema(
        colmap = list(
            plot = "plot",
            species = "species",
            d = "dbh_mm",
            h = "height_m"
        ),
        units = list(d = "mm", h = "m"),
        levels = "plot",
        keep_cols = c("plot", "species")
    )

    ## Tree-level standardized output
    external_dendroMetrics(
        x = x,
        var = c("d", "h", "ba", "n"),
        design = design,
        schema = schema,
        tree_d_unit_out = "cm",
        tree_h_unit_out = "m"
    )

    ## Plot-level summary output
    external_dendroMetrics(
        x = x,
        var = c("d", "h", "ba", "n"),
        design = design,
        schema = schema,
        summ.vr = "plot"
    )
},
class = c("function", "basifoR"))
