## =========================================================
## Sampling-design classes and methods
## =========================================================

# Constructor for concentric plot designs
new_concentric_design <- function(radii_m, min_dbh_cm, name = "custom") {

    if (!is.numeric(radii_m) || !is.numeric(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must be numeric.")

    if (length(radii_m) != length(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must have the same length.")

    if (length(radii_m) == 0)
        stop("Design vectors cannot be empty.")

    o <- order(min_dbh_cm)

    structure(
        list(
            name = name,
            radii_m = radii_m[o],
            min_dbh_cm = min_dbh_cm[o],
            sf = 1e4 / (pi * radii_m[o]^2)
        ),
        class = c("concentric_design", "inventory_design")
    )
}

# Spanish National Forest Inventory design
snfi_design <- function() {
    new_concentric_design(
        radii_m = c(5, 10, 15, 25),
        min_dbh_cm = c(7.5, 12.5, 22.5, 42.5),
        name = "SNFI"
    )
}

# Example of another concentric design
concentric_design_3 <- function() {
    new_concentric_design(
        radii_m = c(4, 8, 12),
        min_dbh_cm = c(5, 15, 30),
        name = "3-ring design"
    )
}

print.concentric_design <- function(x, ...) {
    cat("Concentric plot design:", x$name, "\n")
    cat("Radii (m):", paste(x$radii_m, collapse = ", "), "\n")
    cat("Minimum DBH (cm):", paste(x$min_dbh_cm, collapse = ", "), "\n")
    cat("Expansion factors:", paste(round(x$sf, 2), collapse = ", "), "\n")
    invisible(x)
}

## Generic
trees_per_ha <- function(design, dbh_cm) {
    UseMethod("trees_per_ha")
}

## Method for concentric designs
trees_per_ha.concentric_design <- function(design, dbh_cm) {

    dbh_cm <- as.numeric(dbh_cm)

    if (length(dbh_cm) > 1)
        dbh_cm <- mean(dbh_cm, na.rm = TRUE)

    if (all(is.na(dbh_cm)))
        return(NA_real_)

    if (is.na(dbh_cm) || dbh_cm < design$min_dbh_cm[1])
        return(NA_real_)

    idx <- findInterval(dbh_cm, design$min_dbh_cm)
    design$sf[idx]
}

## =========================================================
## Rewritten dbhMetric()
## =========================================================

dbhMetric <- structure(function(
    dbh,          ##<< \code{numeric}. Either diameters at breast height
                  ##<< (\code{mm}) or tree heights (\code{m}). Vectors are
                  ##<< averaged. Zero values are formatted to \code{NA}.
    met = "d",    ##<< \code{character}. Any of four metrics: mean diameter
                  ##<< at breast height (\code{'d'}), basal area
                  ##<< (\code{'ba'}), number of trees (\code{'n'}), or tree
                  ##<< height (\code{'h'}). Default \code{'d'}.
    design = snfi_design()
                  ##<< \code{inventory_design}. Sampling design used to
                  ##<< compute \code{'n'}. Default is the Spanish National
                  ##<< Forest Inventory concentric plot design.
) {

    if (!is.numeric(dbh))
        dbh <- as.numeric(as.character(dbh))

    dbh[dbh == 0] <- NA_real_

    if (length(dbh) > 1)
        dbh <- mean(dbh, na.rm = TRUE)

    if (all(is.na(dbh)))
        return(NA_real_)

    if (!met %in% c("d", "ba", "n", "h"))
        stop("'met' must be one of 'd', 'ba', 'n', or 'h'.")

    if (met %in% "d")
        return(dbh)

    if (met %in% c("ba", "n"))
        dbh <- conv_unit(dbh, from = "mm", to = "cm")

    if (met %in% "ba")
        return(pi * dbh^2 * (4 * 1E4)^-1)

    if (met %in% "n")
        return(trees_per_ha(design = design, dbh_cm = dbh))

    if (met %in% "h")
        return(conv_unit(dbh, from = "m", to = "dm"))

}, ex = function() {

    ## default SNFI design
    dbhMetric(300, "ba")   # basal area for 300 mm DBH
    dbhMetric(130, "n")    # trees per ha under SNFI design

    ## custom concentric design
    dsg <- concentric_design_3()
    dbhMetric(130, "n", design = dsg)

})


