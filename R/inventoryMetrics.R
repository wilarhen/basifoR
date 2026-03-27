inventoryMetrics <- structure(function #Unified dispatcher for inventory workflows
### Run a complete inventory workflow by dispatching to either
### \code{\link{dendroMetrics}} for Spanish National Forest Inventory
### inputs or to \code{external_dendroMetrics()} for external
### inventories. The function is intentionally thin: it decides which
### backend to use, forwards the relevant arguments, and stores the
### original call so the result can be updated later with
### \code{\link[utils:update]{update}}.
(
    nfi, ##<< \code{character}, \code{data.frame}, or \code{list}. One
         ##<< inventory input or several inputs accepted by the selected
         ##<< backend.
    backend = c("auto", "snfi", "external"), ##<< \code{character(1)}.
                                                ##<< Backend selector.
                                                ##<< Use \code{"auto"}
                                                ##<< to infer the backend
                                                ##<< from the supplied
                                                ##<< arguments.
    summ.vr = "Estadillo", ##<< \code{character} or \code{NULL}. Grouping
                            ##<< variable passed to the selected backend.
                            ##<< When this argument is not supplied,
                            ##<< \code{inventoryMetrics()} uses a backend-
                            ##<< aware default: \code{"Estadillo"} for
                            ##<< \code{backend = "snfi"} and the backend
                            ##<< default for \code{backend = "external"}.
    cut.dt = "d == d", ##<< \code{character}. Logical expression used
                        ##<< to subset the backend output.
    report = FALSE, ##<< \code{logical}. If \code{TRUE}, request a
                    ##<< CSV report from the selected backend.
    mc.cores = getOption("mc.cores", 1L), ##<< \code{integer}. Number
                                           ##<< of cores used when the
                                           ##<< backend supports several
                                           ##<< inputs.
    design = NULL, ##<< Optional sampling \code{design}. Passed to the
                   ##<< selected backend when relevant.
    schema = NULL, ##<< Optional external schema object. Used only by
                   ##<< \code{backend = "external"}.
    method_registry = NULL, ##<< Optional method registry passed to the
                            ##<< selected backend. This may define
                            ##<< custom volume or summarization methods.
    parameter_table = NULL, ##<< Optional table of parameters required
                            ##<< by the external backend.
    ... ##<< Additional named arguments forwarded to the selected
        ##<< backend.
) {
    ##title<< Dispatch complete workflows for SNFI and external inventories
    ##details<<
    ##details<< \code{backend = "auto"} selects \code{"external"} when
    ##details<< any external-only object is supplied in \code{schema} or
    ##details<< \code{parameter_table}. Otherwise it selects
    ##details<< \code{"snfi"}. This conservative rule avoids routing
    ##details<< standard SNFI calls away from
    ##details<< \code{\link{dendroMetrics}}.
    ##details<<
    ##details<< When \code{summ.vr} is not supplied, the dispatcher does
    ##details<< not force the SNFI default onto the external backend.
    ##details<< That prevents errors such as attempts to group external
    ##details<< data by \code{"Estadillo"}.
    ##details<<
    ##details<< The dispatcher keeps computation inside the backend
    ##details<< functions. It only validates the backend choice,
    ##details<< forwards arguments, stores the reconstructed call in
    ##details<< \code{attr(x, "call")}, and records the chosen backend in
    ##details<< \code{attr(x, "backend")}. That makes the wrapper easier
    ##details<< to maintain while preserving a single public entry point.
    ##value<< A backend result augmented with class
    ##value<< \code{"inventoryMetrics"}, a stored call in
    ##value<< \code{attr(x, "call")}, and the selected backend in
    ##value<< \code{attr(x, "backend")}. The returned object otherwise
    ##value<< preserves the class and attributes produced by the backend.
    ##seealso<< dendroMetrics, nfiMetrics, metrics2Vol, update.inventoryMetrics
    ##note<< Keep backend-specific computation in the backend functions,
    ##note<< not in this dispatcher.

    call0 <- match.call(expand.dots = TRUE)
    dots0 <- list(...)
    summ_missing <- missing(summ.vr)

    compact_args <- function(x) {
        keep <- !vapply(x, is.null, logical(1))
        x[keep]
    }

    detect_backend <- function(nfi, design, schema, method_registry,
                               parameter_table, dots) {
        if (!is.null(schema) || !is.null(parameter_table))
            return("external")

        if (inherits(nfi, c("external_nfi", "external_nfiMetrics",
                            "external_metrics2vol")))
            return("external")

        if (!is.null(dots[["backend_hint"]]) &&
            identical(dots[["backend_hint"]], "external"))
            return("external")

        "snfi"
    }

    finalize_output <- function(out, call, backend) {
        if (is.null(out))
            return(NULL)

        attr(out, "call") <- call
        attr(out, "backend") <- backend
        class(out) <- unique(c("inventoryMetrics", class(out)))
        out
    }

    backend <- match.arg(backend)

    if (backend == "auto")
        backend <- detect_backend(
            nfi = nfi,
            design = design,
            schema = schema,
            method_registry = method_registry,
            parameter_table = parameter_table,
            dots = dots0
        )

    if (backend == "snfi" && (!is.null(schema) || !is.null(parameter_table)))
        stop(
            "'schema' and 'parameter_table' are only valid for ",
            "backend = 'external'.",
            call. = FALSE
        )

    if (backend == "external" &&
        !exists("external_dendroMetrics", mode = "function", inherits = TRUE))
        stop(
            "Backend 'external' requires a function named ",
            "'external_dendroMetrics()'.",
            call. = FALSE
        )

    if (backend == "snfi") {
        args <- compact_args(c(
            list(
                nfi,
                summ.vr = if (summ_missing) "Estadillo" else summ.vr,
                cut.dt = cut.dt,
                report = report,
                mc.cores = mc.cores,
                design = design,
                method_registry = method_registry
            ),
            dots0
        ))

        out <- do.call(dendroMetrics, args)
    } else {
        base_args <- list(
            nfi,
            cut.dt = cut.dt,
            report = report,
            mc.cores = mc.cores,
            design = design,
            schema = schema,
            method_registry = method_registry,
            parameter_table = parameter_table
        )

        if (!summ_missing)
            base_args$summ.vr <- summ.vr

        args <- compact_args(c(base_args, dots0))
        out <- do.call(external_dendroMetrics, args)
    }

    finalize_output(out, call0, backend)

}, ex = function() {
    ## Example with bundled Toledo data routed to the SNFI backend
    ifn4p45 <- system.file("Ifn4_Toledo.zip", package = "basifoR")
    x <- inventoryMetrics(ifn4p45, backend = "snfi")
    attr(x, "backend")

    ## External workflows require an external backend function and the
    ## corresponding schema, design, and parameter objects.
})


update.inventoryMetrics <- function #Update a stored inventoryMetrics call
##title<< Update a stored inventoryMetrics call
##description<< Re-run \code{\link{inventoryMetrics}} from the call
## stored in a previous \code{"inventoryMetrics"} result while
## replacing one or more named arguments in \code{...}. This method
## supports reproducible re-evaluation of the selected backend and can
## either return the updated result or the reconstructed call.
### This S3 method requires an object created by the updatable
### \code{\link{inventoryMetrics}} definition, which stores the
### original call in \code{attr(x, "call")}. All arguments supplied in
### \code{...} must be named.
(
    object, ##<< A \code{"inventoryMetrics"} object created by the
            ##<< updatable \code{\link{inventoryMetrics}} definition.
    ..., ##<< Named arguments used to replace entries in the stored
         ##<< call before evaluation.
    evaluate = TRUE ##<< \code{logical}. If \code{TRUE}, evaluate the
                    ##<< updated call and return the resulting object.
                    ##<< If \code{FALSE}, return the reconstructed call
                    ##<< without evaluation.
) {
    if (!inherits(object, "inventoryMetrics"))
        stop(
            "'object' must inherit from 'inventoryMetrics'. ",
            "Call inventoryMetrics(...) first and then use update(result, ...).",
            call. = FALSE
        )

    call0 <- attr(object, "call")

    if (is.null(call0))
        stop(
            "No stored call found in 'object'. ",
            "Recreate the result with the updatable inventoryMetrics() definition ",
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
    ##value<< A new \code{"inventoryMetrics"} object when
    ##value<< \code{evaluate = TRUE}; otherwise the updated call.
}
