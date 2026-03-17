dendroMetrics_ <- structure(function
(
    nfi,
    summ.vr = "Estadillo",
    cut.dt = "d == d",
    report = FALSE,
    mc.cores = getOption("mc.cores", 1L),
    .parallel = TRUE,
    ...
) {

    dendro_one <- function(nfi, summ.vr, cut.dt, report, ...) {

        nfi. <- nfi

        if (is.null(nfi.))
            return(nfi)

        if (!inherits(nfi., "metrics2vol"))
            nfi <- metrics2Vol(nfi, ...)

        frm. <- attr(nfi, "units")

        if (is.null(summ.vr)) {
            nfi <- subset(nfi, eval(parse(text = cut.dt)))
            attributes(nfi) <- c(attributes(nfi), list(units = frm.))

            if (report)
                write.csv(nfi, file = "report.csv", row.names = FALSE)

            return(nfi)
        }

        summ.vr <- flev(nfi, summ.vr)
        var <- getOption("units1")[getOption("units1") %in% names(nfi)]
        to. <- names(var)
        var. <- var[var != "n"]

        nfi <- conv_units(nfi, var = var, un = to.)
        msp <- split(nfi, nfi[summ.vr])
        msp <- Filter("nrow", msp)

        fsum <- function(dt) {
            dt[, var.] <- dt[, var.] * dt[, "n"]

            summ <- apply(dt[, var, drop = FALSE], 2, sum, na.rm = TRUE)

            keep_avg <- intersect(c("d", "h", "Hd"), names(summ))
            if (length(keep_avg))
                summ[keep_avg] <- summ[keep_avg] / summ["n"]

            if (all(c("ba", "n") %in% names(summ)))
                summ["dg"] <- sqrt((4E4 * summ["ba"] / summ["n"]) / pi)

            summ <- summ[order(names(summ))]
            summ <- sapply(summ, function(x) round(x, 3))
            summ <- t(as.matrix(summ))

            fcs. <- names(dt)[!names(dt) %in% var]
            fcs <- dt[1, fcs., drop = FALSE]

            cbind(fcs, summ)
        }

        resm <- lapply(msp, fsum)
        resm <- Reduce("rbind", resm)
        resm <- data.frame(resm)

        resm <- subset(resm, eval(parse(text = cut.dt)))
        rownames(resm) <- NULL

        if (report)
            write.csv(resm, file = "report.csv", row.names = FALSE)

        dgcm <- "dg"
        names(dgcm) <- "cm"
        attr. <- c(attr(nfi, "units"), dgcm)
        attributes(resm) <- c(attributes(resm), list(units = attr.))

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
            return(vector("list", n))
        if (is.data.frame(x))
            return(rep(list(x), n))
        if (length(x) == 1L)
            return(rep(as.list(x), n))
        if (length(x) != n)
            stop("'", arg, "' must have length 1 or length ", n, ".")
        as.list(x)
    }

    nfi_list <- recycle_arg(nfi, n_inputs, "nfi")
    nfi.nr_list <- recycle_arg(nfi.nr, n_inputs, "nfi.nr")

    jobs <- Map(function(nfi_i, nfi.nr_i) {
        dots_i <- dots0
        if (!is.null(nfi.nr_i))
            dots_i[["nfi.nr"]] <- nfi.nr_i
        list(nfi = nfi_i, dots = dots_i)
    }, nfi_list, nfi.nr_list)

    run_job <- function(job) {
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
        return(out)
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

    res_list <- Filter(Negate(is.null), res_list)

    if (!length(res_list))
        return(NULL)

    out <- Reduce(function(a, b) {
        if (is.null(a)) return(b)
        if (is.null(b)) return(a)
        rbind(a, b)
    }, res_list)

    out <- data.frame(out)
    rownames(out) <- NULL

    if (length(res_list) && !is.null(attr(res_list[[1]], "units")))
        attr(out, "units") <- attr(res_list[[1]], "units")

    if (report)
        write.csv(out, file = "report.csv", row.names = FALSE)

    out
})

## dendroMetrics_ <- structure(function
## ### Summarize dendrometrics
## ### This function can summarize dendrometric data of the Spanish
## ### National Forest Inventory (SNF). It can also control most other
## ### functions of the package. Dendrometric variables in the outputs are
## ### transformed into stand units, see the Details section.
##                            ##details<< Dendrometric variables are
##                            ## summarized according to the levels of
##                            ## the argument \code{summ.vr}. The summary
##                            ## outputs include the categorical columns
##                            ## formulated in \code{summ.vr} and the
##                            ## variables defined using
##                            ## arguments/defaults in
##                            ## \code{\link{nfiMetrics}}. These
##                            ## variables include the tree basal area
##                            ## \code{ba} (\code{'m2 ha-1'}), the
##                            ## average diameter at breast height
##                            ## \code{d} (\code{'cm'}), the quadratic
##                            ## mean diameter \code{dg} (\code{'cm'}),
##                            ## the average tree height \code{h}
##                            ## (\code{'m'}), the number of trees by
##                            ## hectare \code{n} ('dimensionless'), and
##                            ## the over bark volume \code{v} (\code{'m3
##                            ## ha-1'}). Subsets of the output summary
##                            ## are extracted using logical expressions
##                            ## in argument \code{'cut.dt'}, see syntax
##                            ## in \code{\link{Logic}}.
## (
##     nfi, ##<< \code{character}, \code{list}, or \code{data.frame}.
##           ## URL/path to a compressed SNF file (.zip) having data of
##           ## either .dbf or .mdb file extensions; or data frame such
##           ## as that produced by \code{\link{nfiMetrics}}; or data
##           ## frame such as that produced by \code{\link{readNFI}}.
##           ## Several inputs can be supplied as a list or vector and
##           ## processed in parallel.
##     summ.vr = "Estadillo", ##<< \code{character} or \code{NULL}. Name
##                            ## of a categorical variable in the SNF
##                            ## data used to summarize the outputs. If
##                            ## \code{NULL} then output from
##                            ## \code{\link{metrics2Vol}} is returned.
##                            ## Default \code{"Estadillo"} processes
##                            ## sample plots.
##     cut.dt = "d == d", ##<< \code{character}. Logical condition used
##                        ## to subset the output. Default \code{"d == d"}
##                        ## avoids subsetting.
##     report = FALSE, ##<< \code{logical}. Write report files in
##                     ## \code{report.dir}. When several inputs are
##                     ## supplied, one file per input is written.
##     report.dir = getwd(), ##<< \code{character}. Directory where
##                           ## report files are written.
##     report.prefix = "report", ##<< \code{character}. Prefix used in
##                               ## report filenames.
##     mc.cores = getOption("mc.cores", 1L), ##<< \code{integer}. Number
##                     ## of worker processes used when several inputs
##                     ## are supplied in \code{nfi}.
##     .parallel = TRUE, ##<< \code{logical}. If \code{TRUE} and several
##                       ## inputs are supplied in \code{nfi}, process
##                       ## them in parallel.
##     ...
## ) {

##     make_report_file <- function(id, dir, prefix) {
##         if (!dir.exists(dir))
##             dir.create(dir, recursive = TRUE, showWarnings = FALSE)
##         file.path(dir, paste0(prefix, "_", id, ".csv"))
##     }

##     dendro_one <- function(nfi, summ.vr, cut.dt, report, report.file, ...) {

##         nfi. <- nfi

##         if (is.null(nfi.))
##             return(nfi)

##         if (!inherits(nfi., "metrics2vol"))
##             nfi <- metrics2Vol(nfi, ...)

##         frm. <- attr(nfi, "units")

##         if (is.null(summ.vr)) {
##             nfi <- subset(nfi, eval(parse(text = cut.dt)))
##             attributes(nfi) <- c(attributes(nfi), list(units = frm.))

##             if (report)
##                 write.csv(nfi, file = report.file, row.names = FALSE)

##             return(nfi)
##         }

##         summ.vr <- flev(nfi, summ.vr)
##         var <- getOption("units1")[getOption("units1") %in% names(nfi)]
##         frm. <- names(attr(nfi, "units"))
##         to. <- names(var)
##         var. <- var[var != "n"]

##         nfi <- conv_units(nfi, var = var, un = to.)
##         msp <- split(nfi, nfi[summ.vr])
##         msp <- Filter("nrow", msp)

##         fsum <- function(dt) {
##             dt[, var.] <- dt[, var.] * dt[, "n"]

##             summ <- apply(dt[, var, drop = FALSE], 2, sum, na.rm = TRUE)

##             keep_avg <- intersect(c("d", "h", "Hd"), names(summ))
##             if (length(keep_avg))
##                 summ[keep_avg] <- summ[keep_avg] / summ["n"]

##             if (all(c("ba", "n") %in% names(summ)))
##                 summ["dg"] <- sqrt((4E4 * summ["ba"] / summ["n"]) / pi)

##             summ <- summ[order(names(summ))]
##             summ <- sapply(summ, function(x) round(x, 3))
##             summ <- t(as.matrix(summ))

##             fcs. <- names(dt)[!names(dt) %in% var]
##             fcs <- dt[1, fcs., drop = FALSE]

##             cbind(fcs, summ)
##         }

##         resm <- lapply(msp, fsum)
##         resm <- Reduce("rbind", resm)
##         resm <- data.frame(resm)

##         resm <- subset(resm, eval(parse(text = cut.dt)))
##         rownames(resm) <- NULL

##         if (report)
##             write.csv(resm, file = report.file, row.names = FALSE)

##         dgcm <- "dg"
##         names(dgcm) <- "cm"
##         attr. <- c(attr(nfi, "units"), dgcm)
##         attributes(resm) <- c(attributes(resm), list(units = attr.))

##         resm
##     }

##     is_many <- is.list(nfi) || (length(nfi) > 1L && !is.data.frame(nfi))

##     if (!is_many) {
##         report.file <- make_report_file(1L, report.dir, report.prefix)
##         return(dendro_one(
##             nfi = nfi,
##             summ.vr = summ.vr,
##             cut.dt = cut.dt,
##             report = report,
##             report.file = report.file,
##             ...
##         ))
##     }

##     nfi_list <- if (is.list(nfi)) nfi else as.list(nfi)
##     ids <- seq_along(nfi_list)
##     report.files <- vapply(
##         ids,
##         function(i) make_report_file(i, report.dir, report.prefix),
##         character(1)
##     )

##     mc.cores <- as.integer(mc.cores)
##     if (is.na(mc.cores) || mc.cores < 1L)
##         mc.cores <- 1L

##     if (!.parallel || mc.cores == 1L) {

##         res_list <- Map(
##             function(x, rf) {
##                 dendro_one(
##                     nfi = x,
##                     summ.vr = summ.vr,
##                     cut.dt = cut.dt,
##                     report = report,
##                     report.file = rf,
##                     ...
##                 )
##             },
##             x = nfi_list,
##             rf = report.files
##         )

##     } else if (.Platform$OS.type == "windows") {

##         cl <- parallel::makeCluster(mc.cores)
##         on.exit(parallel::stopCluster(cl), add = TRUE)

##         parallel::clusterExport(
##             cl = cl,
##             varlist = c(
##                 "dendro_one",
##                 "summ.vr",
##                 "cut.dt",
##                 "report",
##                 "nfi_list",
##                 "report.files"
##             ),
##             envir = environment()
##         )

##         parallel::clusterEvalQ(cl, {
##             if ("basifoR" %in% loadedNamespaces())
##                 NULL
##             else
##                 library(basifoR)
##         })

##         res_list <- parallel::parLapply(
##             cl = cl,
##             X = ids,
##             fun = function(i, ...) {
##                 dendro_one(
##                     nfi = nfi_list[[i]],
##                     summ.vr = summ.vr,
##                     cut.dt = cut.dt,
##                     report = report,
##                     report.file = report.files[[i]],
##                     ...
##                 )
##             },
##             ...
##         )

##     } else {

##         res_list <- parallel::mclapply(
##             X = ids,
##             FUN = function(i, ...) {
##                 dendro_one(
##                     nfi = nfi_list[[i]],
##                     summ.vr = summ.vr,
##                     cut.dt = cut.dt,
##                     report = report,
##                     report.file = report.files[[i]],
##                     ...
##                 )
##             },
##             ...,
##             mc.cores = mc.cores
##         )
##     }

##     res_list <- Filter(Negate(is.null), res_list)

##     if (!length(res_list))
##         return(NULL)

##     res_list <- Map(function(x, id) {
##         if (!is.null(x))
##             x$source_nfi <- id
##         x
##     }, res_list, ids)

##     out <- Reduce(function(a, b) {
##         if (is.null(a)) return(b)
##         if (is.null(b)) return(a)
##         rbind(a, b)
##     }, res_list)

##     out <- data.frame(out)
##     rownames(out) <- NULL

##     if (!is.null(attr(res_list[[1]], "units")))
##         attr(out, "units") <- attr(res_list[[1]], "units")

##     out

## ### \code{data.frame}. Depending on \code{summ.vr = NULL}, an output
## ### from \code{\link{metrics2Vol}}, or a summary of the variables, see
## ### Details section.
## }, ex = function() {

## ## Single input, one report file:
## ifn4p45 <- system.file("Ifn4_Toledo.zip", package = "basifoR")

## res1 <- dendroMetrics(
##     nfi = ifn4p45,
##     report = TRUE,
##     report.dir = tempdir(),
##     report.prefix = "report"
## )

## ## Several inputs, one report per input:
## z1 <- system.file("Ifn4_Toledo.zip", package = "basifoR")
## z2 <- system.file("Ifn4_Toledo.zip", package = "basifoR")

## res2 <- dendroMetrics(
##     nfi = list(z1, z2),
##     cut.dt = "h > 8",
##     report = TRUE,
##     report.dir = tempdir(),
##     report.prefix = "report",
##     mc.cores = 2
## )

## list.files(tempdir(), pattern = "^report_.*\\.csv$")

## })


## Testing functions in basifoR
nfi4 <- function(prov, complain = TRUE){
## Function to download ifn4 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
    ## dt <- read.csv('procods_Cristobal.csv')
    dt <- procods
prov. <- prov
    ## prov <- find_code(dt, prov)
        prov <- find_code_(prov, is.ifn4 = TRUE, df = dt)
    if(is.null(prov))
        return(invisible(NULL))
u <- miteco_urls_from_paths('path41')
    all_links. <- unlist(Map(function(x)
        inspect_links(x, prov, ignore.case = TRUE), u))
    pattern <- "[iI]fn4[_\\-p]?"
  exclude <- "[tT]ablas|[sS]ig"
  matches <- grep(pattern, all_links., value = TRUE) # Find strings matching 'ifn4'
  all_links <- matches[!grepl(exclude, matches)]    # Exclude unwanted patterns
    parsed <- mapply(function(x)
        httr::modify_url(getOption('server'), path = x),
        all_links, USE.NAMES = FALSE)
if(length(parsed) == 0){
        if(complain)
            warning(paste0("URL for spanish province '", prov., "' not found!\n"),
                    call. = FALSE)
    return(invisible(NULL))
}
return(parsed)}

## # Define the function with wildcard support
## find_code <- function(df, input_value) {
##     if (is.numeric(input_value)) {  # Check if input is numeric
##     result <- df$provincia_1[grepl(paste0('^',input_value,'$'), df$codigo,ignore.case = TRUE)]
##   }else{
##   # Use grepl for partial matching (case-insensitive search)
##   result <- df$codigo[
##     grepl(input_value, df$provincia, ignore.case = FALSE) | 
##     grepl(input_value, df$codigo2,
##           ## fixed = TRUE,ignore.case = FALSE) | 
##           ignore.case = FALSE) |
##     grepl(input_value, df$provincia_0, ignore.case = FALSE) |
##     grepl(input_value, df$provincia_1, ignore.case = FALSE)
##     ][1L]
##       result <- df$provincia_1[grepl(paste0('^',result,'$'), df$codigo,ignore.case = TRUE)]
##   }
##   # Return the result
##   return(result)
## }

## find_code_ <- function(input_value, is.ifn4 = TRUE, df) {
##   result <- df$codigo[
##     grepl(input_value, df$codigo) | 
##     grepl(input_value, df$provincia) | 
##     grepl(input_value, df$codigo2) |
##     grepl(input_value, df$provincia_0) |
##     grepl(input_value, df$provincia_1)
##     ][1L]
##   if(is.ifn4){
##       result <- df$provincia_1[
##                        grepl(paste0('^',result,'$'), df$codigo,
##                              ignore.case = TRUE)]}
##   if(length(result) == 0)
##       result <- NA
##       if(is.na(result)){
##           warning(paste0("Spanish province '", input_value, "' not found!\n"),
##                   call. = FALSE)
##         return(invisible(NULL))}
##   ## }
##   # Return the result
##   return(result)
## }


find_code_ <- function(input_value, is.ifn4 = TRUE, df, complain = TRUE) {
  result <- df$codigo[
    grepl(input_value, df$codigo) | 
    grepl(input_value, df$provincia) | 
    grepl(input_value, df$codigo2) |
    grepl(input_value, df$provincia_0) |
    grepl(input_value, df$provincia_1)
    ][1L]
  if(is.ifn4){
      result <- df$provincia_1[
                       grepl(paste0('^',result,'$'), df$codigo,
                             ignore.case = TRUE)]}
  if(length(result) == 0)
      result <- NA
      if(is.na(result) & complain){
          warning(paste0("Spanish province '", input_value, "' not found!\n"),
                  call. = FALSE)
        return(invisible(NULL))}
  ## }
  # Return the result
  return(result)
}

## # Define the function
## find_ifn4 <- function(strings) {
##   # Regular expression
##  # Matches 'ifn4' optionally followed by '_', '-', or 'p'
##     pattern <- "[iI]fn4[_\\-p]?"
##  # Exclude strings containing 'tables' or 'Sig'
##   exclude <- "[tT]ablas|[sS]ig"
##   # Filter strings
##   matches <- grep(pattern, strings, value = TRUE) # Find strings matching 'ifn4'
##   result <- matches[!grepl(exclude, matches)]    # Exclude unwanted patterns

##   return(result)
## }


## parsedURL <- function(x, path.='path41', dt = procods){
##     parsedURL <- Map(function(x)
##         fparsed(x, path.= path., dt = dt),x)
##     names(parsedURL) <- x
## return(parsedURL)}


## fparsed <- function(code., path. = 'path41', dt){
## dt <- read.csv('procods_Cristobal.csv')
##     u <- miteco_urls_from_paths(path.)
##     prov <- find_code(dt, code.)
## ## all_links. <- inspect_links(u, prov, ignore.case = TRUE) #%>% print()
##     all_links. <- unlist(Map(function(x)
##         inspect_links(x, prov, ignore.case = TRUE), u))
## parsed <- mapply(function(x)
##     httr::modify_url(getOption('server'), path = x),
##     all_links., USE.NAMES = FALSE)
##     if(length(parsed) == 0)
##         parsed = NULL
## return(parsed)
## }

## accentless <- function( s ) {
##   chartr(
##     "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
##     "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
##     s );
## }

##-----------------------------------------------------------------

check_extension_in_zip <- function(url, extension){
  temp_file <- tempfile(fileext=".zip")
  suppressWarnings(
    tryCatch({
      download.file(url, temp_file, mode="wb", quiet=TRUE)
      zip_contents <- unzip(temp_file, list=TRUE)$Name
      ## has_extension <- any(grepl(extension, zip_contents, ignore.case=TRUE))
      has_extension <- grepl(extension, zip_contents, ignore.case=TRUE)
      has_extension <- url[has_extension]
      unlink(temp_file)
      return(has_extension)
    }, error=function(e){
      ## message("An error occurred:", e$message)
      return(FALSE)
    })
  )
}

## check_extension_in_zip <- function(url, extension){
##   temp_file <- tempfile(fileext=".zip")
##   tryCatch({
##     curl_download(url, temp_file)
##     zip_contents <- unzip(temp_file, list=TRUE)$Name
##     has_extension <- any(grepl(paste0("\\", extension, "$"), zip_contents, ignore.case=TRUE))
##     unlink(temp_file)
##     return(has_extension)
##   }, error=function(e){
##     message("An error occurred: ", e$message)
##     return(FALSE)
##   })
## }

#----------------------------------------------------------------
## Internal utility functions used by basifoR

# Function to replace a row based on two indices
replace_provincia <- function(df, row1, row2) {
    if(is.character(row1))
        row1 <- find_provincia_or_codigo(row1)
    if(is.character(row2))
        row1 <- find_provincia_or_codigo(row2)
  # Check if row indices are within the data frame bounds
  if (any(row1 > nrow(df) | row2 > nrow(df))) {
    stop("Row indices are out of bounds")
  }
  
  # Replace the row corresponding to row1 with the row corresponding to row2
  df[row1, ] <- df[row2, ]
  
  return(df)
}

# Function to test response from a URL
test_url_response <- function(url) {
  # Send GET request
  response <- GET(url)
  
  # Check the status code
  status_code <- status_code(response)
  ## print(paste("Status Code:", status_code))
return(status_code)  
  ## # Check the content type
  ## content_type <- headers(response)$`content-type`
  ## print(paste("Content Type:", content_type))
  
  ## # Check the content of the response
  ## content <- content(response, as = "text", encoding = "UTF-8")
  ## print(paste("Content:", substr(content, 1, 500)))  # Print the first 500 characters
}

.onAttach <- function(lib, pkg)
{
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  if(interactive())
  { # > figlet basifoR
      msg <- basifoR_figlet()
      packageStartupMessage(msg)
    }
    else
    { packageStartupMessage(
          "Package 'basifoR' version ", version) }
    packageStartupMessage("Type 'citation(\"basifoR\")' for citing this R package in publications\n")
    invisible()
}

.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(
        server = "http://www.miteco.gob.es",
        path21 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_1_25.html",
        path22 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_26_50.html",
        path31 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_1_25.html",
        path32 = "es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_26_50.html",
        path41 = "es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html", 
        utm = "+proj=utm +zone=utm.z +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
        utm1 = "+proj=utm +zone=utm.z +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
        longlat = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
        fapp = 'mcmapply',
        dt.ext = c('mdb','DBF', 'accdb'),
        units = units.,
        units1 = units..)
toset <- !(names(op.FC) %in% names(op))
  if(any(toset)) options(op.FC[toset])
invisible()
}

basifoR_figlet <- function(){
msg <- cat(
"
 _           _ ___     _____ 
| |_ ___ ___|_|  _|___| __  |
| . | .'|_ -| |  _| . |    -|
|___|__,|___|_|_| |___|__|__|\n
"
)
vrs <- paste0('basifoR version ',packageVersion("basifoR"),'\n')
cat(vrs)
}

conv_units <- function(nfi, var = c('d','h'), un = c('cm','m')){
    units. <- getOption('units')
    if(!is.null(attr(nfi,'units')))
        units.  <- attr(nfi,'units')
    cols <- units.[units.%in%names(nfi)]
    units_ini <- units_out <- names(cols)
    matches <- sapply(var,function(m) paste0("^",m,"$"))
    pos. <- sapply(matches,function(m) grep(m, cols))
    units_out[pos.]  <- un
    f_conv_unit <- function(x,y,z){
        if(y == "" | z == ""){
            return(x)
        }else{
            conv_unit(x,y,z)}}
    nfi[,cols] <- data.frame(
        mapply(function(x,y,z)
            f_conv_unit(x,y,z),
            nfi[,cols],
            units_ini,
            units_out))
    un_attr <- cols 
    names(un_attr) <- units_out
    attributes(nfi) <- c(attributes(nfi), list(units = un_attr))
    return(nfi)}

convert_factors_to_numeric <- function(df) {
# Function to convert factor columns to numeric while preserving
# character columns
  df[] <- lapply(df, function(col) {
    if (is.factor(col) && all(grepl("^-?\\d*\\.?\\d+$", as.character(col)))) {
      return(as.numeric(as.character(col)))
    } else {
      return(col)
    }
  })
  return(df)
}

domheight<-function(h, d, n) {
## /IFNdyn-master/ github proyect with dominantHeight function for NFI
## https://github.com/miquelcaceres/IFNdyn
  o <-order(d, decreasing=TRUE)
  h = h[o]
  n = n[o]
  ncum = 0 
  for(i in 1:length(h)) {
    ncum = ncum + n[i]
    if(!is.na(ncum)&&ncum>100){
        return(sum(h[1:i]*n[1:i], na.rm=TRUE)/sum(h[1:i]*n[1:i]/h[1:i], na.rm=TRUE))}
    ## if(ncum>100) return(sum(h[1:i]*n[1:i], na.rm=TRUE)/sum(h[1:i]*n[1:i]/h[1:i], na.rm=TRUE)) ## this produces an error message if the condition is NA
  }
  return(sum(h*n)/sum(n))
}

file_exten <- function(texts)
    sapply(texts, function(x) sub(".*\\.(.*)", "\\1", x),
           USE.NAMES = FALSE)




find_provincia_or_codigo <- function(input) { #
# Function to find provincia if input is numeric, or codigo/codigo2 if
# input is character (case insensitive)
    ## to comment:
    ## load('/home/wihe/Documents/tuh32536/bfRdevel/basifoR/R/sysdata.rda')
    data <- procods
    if (is.numeric(input)) {  # Check if input is numeric
    result <- data$provincia[grepl(paste0("^", input, "$"), data$codigo, ignore.case = TRUE)]
  } else if (is.character(input)) {  # Assume input is character
      result <- data$codigo[grepl(input, data$provincia,
                                  ignore.case = TRUE)]
    if (length(result) == 0) {
        result <- data$codigo2[grepl(paste0("^", input, "$"),
                                     data$provincia, ignore.case = TRUE)]
    }
  } else {
    result <- NA
  }
  if (length(result) == 0) {
    result <- NA
  }
    ## if(is.na(result))
    ## cat(paste0("Warning: provincia or codigo '", input, "' was not found!\n"))
  return(result)
}

flev <- function(vmad, levels){
nma <- names(vmad)
app <- paste(levels, collapse = '|')
gap <- grepl(app,nma, ignore.case = TRUE)
nms <- nma[gap]
return(nms)}

gracefully_fail <- function(remote_file, timeOut = timeout(50)) {
## source:
## https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/11
  try_GET <- function(x, ...) {
    tryCatch(
      GET(url = x, timeOut, ...),
      ## GET(url = x, timeout(50), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(remote_file)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) { 
    message_for_status(resp)
    return(invisible(NULL))
  }
return(TRUE)
}

insert_ifn_ifn4 <- function(input_string) {
  # Define the pattern to match "ifn4" followed by "_" or "-"
  pattern <- "nacionales/ifn4(?=[_-])"
  # Use gregexpr to find all matches
  match_positions <- gregexpr(pattern, input_string, perl = TRUE)
  # Check if there is exactly one match
  if (length(match_positions[[1]]) == 1 && match_positions[[1]][1] != -1) {
    # Find the position of the match
    start_pos <- match_positions[[1]][1]
    # Insert "ifn/ifn4" before the match
    result_string <- paste0(substr(input_string, 1, start_pos - 1), 
                            "ifn/ifn4/", 
                            substr(input_string, start_pos, nchar(input_string)))
  } else {
    # If the pattern is not found exactly once, return the original string
    result_string <- input_string
  }
      return(result_string)
}

inspect_links <- function(url, pattern = NULL, ...) {
# Function to inspect links
    webpage <- rvest::read_html(url)
    links <- rvest::html_nodes(webpage, 'a')
    links <- rvest::html_attr(links, "href")
  if (!is.null(pattern)) {
    links <- links[grepl(pattern, links, perl = TRUE, ...)]
  }
  return(links)
}

is_decompressed <- function(x)
    grepl(paste0(getOption('dt.ext'), collapse = '|'), x)

miteco_urls_from_paths <- function(paths = c('path21', 'path22')) 
    mapply(function(x)
        httr::modify_url(getOption('server'), path = getOption(x)),
        paths,
        USE.NAMES = FALSE)

msg <- basifoR_figlet()

nfi2 <- function(prov, complain = TRUE){
## Function to download ifn2 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
u <- miteco_urls_from_paths(c('path21', 'path22'))
all_links <- unlist(Map(function(x)inspect_links(x,'zip'), u), use.names = FALSE)
parsed <- mapply(function(x)
    httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
    prov. <- prov
    prov <- find_code_(prov, is.ifn4 = FALSE, df = procods)
    ## if(is.na(prov) | length(prov) == 0){
if(is.null(prov)){
        ## if(complain)
    ## warning(paste0("Spanish province '", prov., "' not found!\n"))
        return(invisible(NULL))}
    ptt <- prov + 5
if(ptt < 10)
    ptt <- paste0('0', ptt)
ptt <- paste0(ptt, '.zip')
## return(ptt)
parsed. <- parsed[grepl(ptt, parsed)]
if(length(parsed.) == 0){
    warning(paste0("URL for spanish province '", prov., "' not found!\n"),
            call. = FALSE)
    return(invisible(NULL))
}
return(parsed.)}

nfi3 <- function(prov){
## Function to download ifn3 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
u <- miteco_urls_from_paths(c('path31', 'path32'))
all_links <- unlist(Map(function(x)inspect_links(x,"fn3.*\\.zip"), u), use.names = FALSE)
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
msg <- paste0("Warning: Data for codigo '", prov., "' was not found!\n")
## if(is.character(prov))
##  prov <- find_provincia_or_codigo(prov)
prov <- find_code_(prov, is.ifn4 = FALSE, df = procods)
## if(is.na(prov)){
##     cat(msg)
##     return(invisible(NULL))}
if(is.null(prov)){
        ## if(complain)
    ## warning(paste0("Spanish province '", prov., "' not found!\n"))
        return(invisible(NULL))}
if(prov < 10)
    prov <- paste0('0', prov)
prov <- paste0(prov, '.zip')
parsed. <- parsed[grepl(prov, parsed)]
if(length(parsed.) == 0){
    warning(paste0("URL for spanish province '", prov., "' not found!\n"),
            call. = FALSE)
    return(invisible(NULL))
}
return(parsed.)}

## nfi4 <- function(prov, complain = TRUE){
## ## Function to download ifn4 data using a province code
##     if(is.null(prov))
##         return(invisible(NULL))
##     ## u <- 'https://www.mitueco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html'
## u <- miteco_urls_from_paths('path41')
## all_links. <- inspect_links(u,'tablas|sig', ignore.case = TRUE) #%>% print()
## all_links <- inspect_links(u, "fn4.*\\.zip") #%>% print()
## all_links <- all_links[!all_links%in%all_links.]
## parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
## prov. <- prov
## if(!is.character(prov))
## prov <- find_provincia_or_codigo(prov)
## parsed. <- parsed[grepl(prov, parsed, ignore.case = TRUE)]
##     if(length(parsed.) == 0){
##         if(complain)
##     cat(paste0("Warning: Data for codigo '", prov., "' was not found!\n"))
##     return(invisible(NULL))
## }
## ## to solve some wrong urls addind ifn/ifn4    
## parsed. <- insert_ifn_ifn4(parsed.)
## return(parsed.)}

units. <- c('d','h','ba','n','Hd','v')
names(units.) <- c('mm','m','m2','','m','dm3')

units.. <- units.
names(units..) <- c('cm','m','m2','','m','m3')
