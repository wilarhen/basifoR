# external_dendroMetrics wrapper patch
# Keeps schema support and adds optional tree-level output unit conversion
# without touching externalMetrics() or nfiMetrics().
#
# Source this file after basifoR and after the schema wrappers are loaded.

external_dendroMetrics <- local({
  base_fun <- external_dendroMetrics

  convert_value_units_local <- function(z, from, to) {
    if (is.null(to) || is.na(to) || !nzchar(as.character(to))) return(z)
    if (is.null(from) || is.na(from) || !nzchar(as.character(from))) {
      stop("Cannot convert from an unknown unit.", call. = FALSE)
    }

    from <- tolower(as.character(from))
    to   <- tolower(as.character(to))
    if (identical(from, to)) return(z)

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

  convert_tree_units_local <- function(dt, mapping) {
    un <- attr(dt, "units")
    if (is.null(un) || !length(un)) return(dt)
    if (is.null(names(un))) return(dt)

    for (nm in names(mapping)) {
      if (!nm %in% names(dt) || !nm %in% names(un)) next
      target_unit <- mapping[[nm]]
      current_unit <- un[[nm]]
      if (is.null(target_unit) || is.na(target_unit) || !nzchar(as.character(target_unit))) next
      if (is.null(current_unit) || is.na(current_unit) || !nzchar(as.character(current_unit))) next

      dt[[nm]] <- convert_value_units_local(
        suppressWarnings(as.numeric(as.character(dt[[nm]]))),
        from = current_unit,
        to = target_unit
      )
      un[[nm]] <- target_unit
    }

    attr(dt, "units") <- un
    dt
  }

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
    tree_d_unit_out = NULL,
    tree_h_unit_out = NULL,
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
    if (!is.null(d_unit)) metric_d_unit <- d_unit
    if (!is.null(h_unit)) metric_h_unit <- h_unit

    metric_d_unit <- match.arg(metric_d_unit, c("mm", "cm"))
    metric_h_unit <- match.arg(metric_h_unit, c("m", "dm", "cm"))

    if (!is.null(tree_d_unit_out)) tree_d_unit_out <- match.arg(tree_d_unit_out, c("mm", "cm"))
    if (!is.null(tree_h_unit_out)) tree_h_unit_out <- match.arg(tree_h_unit_out, c("m", "dm", "cm"))

    if (!exists(".resolve_external_schema", mode = "function", inherits = TRUE)) {
      stop("Could not find .resolve_external_schema(). Load basifoR schema support first.", call. = FALSE)
    }

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

    out <- base_fun(
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

    if (!is.null(out) && is.null(summ.vr)) {
      target_units <- c(d = tree_d_unit_out, h = tree_h_unit_out, hd = tree_h_unit_out)
      target_units <- target_units[!is.na(target_units) & nzchar(target_units)]
      if (length(target_units)) {
        out <- convert_tree_units_local(out, target_units)
      }

      frm <- attr(out, "units")
      if (!is.null(frm)) attr(out, "units") <- frm[intersect(names(out), names(frm))]
    }

    out
  }
})
