readNFI <- structure(function#Read SNF data from path
### This function can read compressed data (\code{.zip}) from the 
### Spanish National Forest Inventory (SNF). It can process either 
### \code{URLs} to data stored on the SNF web page 
### (\code{"http://www.miteco.gob.es"}) or paths to locally stored files.
                     ## details<< Compressed data files with
                     ## extensions other than \code{.dbf}, \code{.mdb},
                     ## \code{.accdb}, or \code{.csv} are not supported.
                     ## Most databases in the 2nd and 3rd stages of the
                     ## SNF can be imported directly from
                     ## \code{http://www.miteco.gob.es} using appropriate
                     ## URLs. Data sets from the 2nd stage of SNF are
                     ## imported using \code{\link{read.dbf}}. Data from
                     ## later stages are imported using either
                     ## \code{\link{RODBC}} (Windows) or
                     ## \code{\link{mdb.get}} (Unix-like systems). On
                     ## Windows, install a Microsoft Access driver such as
                     ## Microsoft 365 Access Runtime. On Unix-like systems,
                     ## install the \code{mdbtools} dependency. When
                     ## \code{.csv} files are requested, the function
                     ## returns either one data frame or a named list of
                     ## data frames.
(
    nfi,  ##<< \code{character}. URL or local path to a compressed
          ##file (\code{.zip}) containing SNF data, or to a
          ##decompressed file with these supported extensions.
    nfi.nr = 4,
    dt.nm = 'PCMayores', ##<< \code{character}. Name of a dataset
                         ##stored in the imported NFI data. Defaults
                         ##to \code{'PCMayores'} (3rd NFI) or
                         ##\code{'PIESMA'} (2nd NFI).
    file_ext = NULL, ##<< \code{character}. Optional file extension(s)
                     ##passed to \code{\link{fetchNFI}}. Use
                     ##\code{"csv"} to read zipped csv files.
    file_name = NULL, ##<< \code{character}. Optional file names passed
                      ##to \code{\link{fetchNFI}}.
    ... ##<< Additional arguments for \code{\link{fetchNFI}}.
) {
    imp <- nfi

    is.ifn4 <- nfi.nr == 4

    is_zip_path <- function(x) {
        is.character(x) &&
            length(x) == 1L &&
            !is.na(x) &&
            tolower(tools::file_ext(x)) == "zip"
    }

    is_csv <- function(x) {
        is.character(x) &&
            length(x) > 0L &&
            all(grepl("\\.csv$", x, ignore.case = TRUE))
    }

    detect_sep <- function(fi, n = 5L) {
        hdr <- tryCatch(readLines(fi, n = n, warn = FALSE),
                        error = function(e) character(0))
        hdr <- hdr[nzchar(trimws(hdr))]
        if (length(hdr) == 0L)
            return(",")
        hdr <- hdr[1L]
        cand <- c(";", ",", "\t", "|")
        cnt <- vapply(cand, function(sep)
            length(strsplit(hdr, sep, fixed = TRUE)[[1L]]) - 1L,
            integer(1))
        if (all(cnt <= 0L))
            return(",")
        cand[which.max(cnt)]
    }

    read_one_csv <- function(fi) {
        sep <- detect_sep(fi)
        tryCatch(
            utils::read.table(fi,
                              header = TRUE,
                              sep = sep,
                              quote = '"',
                              dec = '.',
                              fill = TRUE,
                              comment.char = '',
                              stringsAsFactors = FALSE,
                              check.names = FALSE),
            error = function(e) NULL
        )
    }

    read_csv_files <- function(x) {
        out <- lapply(x, read_one_csv)
        ok <- !vapply(out, is.null, logical(1))
        out <- out[ok]
        x <- x[ok]
        if (length(out) == 0L)
            return(NULL)
        names(out) <- tools::file_path_sans_ext(basename(x))
        if (length(out) == 1L)
            return(out[[1L]])
        out
    }

    has_mdbtools_backend <- function() {
        all(nzchar(Sys.which(c("mdb-tables", "mdb-export"))))
    }

    has_windows_access_driver <- function() {
        if (!identical(unname(Sys.info()[["sysname"]]), "Windows")) {
            return(FALSE)
        }

        if (!requireNamespace("odbc", quietly = TRUE)) {
            return(NA)
        }

        drv <- tryCatch(odbc::odbcListDrivers(), error = function(e) NULL)
        if (is.null(drv) || !"name" %in% names(drv)) {
            return(NA)
        }

        any(grepl("access", drv$name, ignore.case = TRUE))
    }

    assert_access_backend <- function(backend = c("odbc", "mdbtools")) {
        backend <- match.arg(backend)

        if (backend == "odbc") {
            if (!requireNamespace("RODBC", quietly = TRUE)) {
                stop(
                    "Missing package 'RODBC'. Install it before reading Access files on Windows.",
                    call. = FALSE
                )
            }

            drv <- has_windows_access_driver()
            if (identical(drv, FALSE)) {
                stop(
                    paste(
                        "Windows Access driver not found.",
                        "Install Microsoft 365 Access Runtime or another Microsoft Access driver,",
                        "restart R, and try readNFI() again."
                    ),
                    call. = FALSE
                )
            }

            if (is.na(drv)) {
                warning(
                    paste(
                        "Could not verify the Windows Access driver because package 'odbc' is not installed.",
                        "basifoR will still try the RODBC connection."
                    ),
                    call. = FALSE
                )
            }

            return(invisible(TRUE))
        }

        if (!requireNamespace("Hmisc", quietly = TRUE)) {
            stop(
                "Missing package 'Hmisc'. Install it before reading Access files on Unix-like systems.",
                call. = FALSE
            )
        }

        if (!has_mdbtools_backend()) {
            sys <- unname(Sys.info()[["sysname"]])
            install_hint <- if (identical(sys, "Darwin")) {
                "Install it with Homebrew: brew install mdbtools"
            } else if (file.exists("/etc/arch-release")) {
                "Install it with pacman: sudo pacman -S mdbtools"
            } else {
                "Install it with your system package manager, for example: sudo apt install mdbtools"
            }

            stop(
                paste(
                    "External tool 'mdbtools' not found.",
                    install_hint
                ),
                call. = FALSE
            )
        }

        invisible(TRUE)
    }

    fetch_args <- c(list(url. = NULL), list(...))
    if (!is.null(file_ext))
        fetch_args$file_ext <- file_ext
    if (!is.null(file_name))
        fetch_args$file_name <- file_name

    code_match <- find_code__(imp, is.ifn4 = is.ifn4, df = procods)
    pr. <- find_code__(imp, FALSE, df = procods)

    if (!is.na(code_match) && !is_zip_path(imp)) {
        nfi. <- paste0("nfi", nfi.nr)
        imp <- do.call(nfi., list(prov = imp))
        fetch_args$url. <- imp
        imp <- do.call(fetchNFI, fetch_args)
    } else if (is_zip_path(imp)) {
        fetch_args$url. <- imp
        imp <- do.call(fetchNFI, fetch_args)
        if (!is.null(imp) && !is_csv(imp))
            nfi.nr  <- get_ifn_nr(imp)
    }

    if (is.null(imp))
        return(imp)

    if (is_csv(imp))
        return(read_csv_files(imp))

    fwin <- function(x, dt.nm) {
        ife <- RODBC::odbcConnectAccess2007(x, rows_at_time = 1)
        on.exit(RODBC::odbcClose(ife))
        ifc <- Map(function(x)
            RODBC::sqlFetch(ife, sqtable = x), dt.nm)
        return(ifc)
    }
    fmdb <- function(x, dt.nm) {
        tryCatch(Hmisc::mdb.get(x, tables = dt.nm),
                 error = function(e) NULL)
    }
    fdbf <- function(x, dt.nm) {
        x <- x[grepl(dt.nm, x)]
        foreign::read.dbf(x)
    }
    is_dbf <- all(grepl('\\.dbf$', imp, ignore.case = TRUE))
    is_mdb <- all(grepl('\\.mdb$|\\.accdb$', imp, ignore.case = TRUE))
    is_win <- identical(unname(Sys.info()['sysname']), 'Windows')

    if (is_mdb) {
        if (is_win) {
            assert_access_backend("odbc")
            fnim <- 'fwin'
        } else {
            assert_access_backend("mdbtools")
            fnim <- 'fmdb'
        }
    }
    if (is_dbf) {
        fnim <- 'fdbf'
    }
    dt.nm. <- dt.nm
    may. <- grepl('mayores', dt.nm, ignore.case = TRUE)
    may2. <- grepl('dbf', fnim)
    if (may. & !may2.) {
        dt.nm. <- unique(c(dt.nm, 'PCDatosMap'))
    }
    if (may2. & dt.nm %in% 'PCMayores')
        dt.nm. <- 'PIESMA'
    dset <- tryCatch(do.call(fnim, list(imp, dt.nm.)),
                     error = function(e) NULL)
    if (is.null(dset))
        return(dset)
    if (!may. & !dt.nm.[1] %in% 'PIESMA')
        return(dset)
    if (may. & !may2.) {
        pr. <- unique(dset$'PCDatosMap'$'Provincia')
        dset <- dset[[dt.nm]]
    }
    if (dt.nm.[1] %in% 'PIESMA')
        pr. <- unique(dset$'PROVINCIA')
    dset <- convert_factors_to_numeric(dset)
    attributes(dset) <- c(attributes(dset), list(pr. = pr.))
    attr(dset, "nfi.nr") <- nfi.nr
    if ('provincia' %in% tolower(names(dset))) {
        dset <- data.frame(nfi.nr = nfi.nr, dset)
        names(dset)[tolower(names(dset)) == "provincia"] <- "pr"
    } else {
        dset <- data.frame(nfi.nr = nfi.nr, pr = pr., dset)
    }
    class(dset) <- append('readNFI', class(dset))
    return(dset)
### \code{data.frame} with numeric columns converted from factors back
### to numeric, while preserving the format of character columns.
}, ex = function(){
    ## donttest{
    ### Retrieval of a database from the second stage of the SNF using a URL resource

    ## ifn2_path <-
    ## '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
    ## ifn2_url <- httr::modify_url("https://www.miteco.gob.es", path
    ## = ifn2_path)

    ## read_ifn2 <- readNFI(ifn2_url)

    ## str(read_ifn2) }

    ## French NFI tree table read from the official web resource
    ## f <- "https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2024_en.zip"
    ## arbre <- readNFI(f, file_ext = "csv", file_name = "ARBRE")
    ## str(arbre)

})
