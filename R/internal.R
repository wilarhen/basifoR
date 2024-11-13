## Internal utility functions used by basifoR

accentless <- function( s ) {
  chartr(
    "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
    "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
    s );
}

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


# Define the function with wildcard support
find_code <- function(df, input_value) {
    if (is.numeric(input_value)) {  # Check if input is numeric
    result <- df$provincia_1[grepl(paste0('^',input_value,'$'), df$codigo,ignore.case = TRUE)]
  }else{
  # Use grepl for partial matching (case-insensitive search)
  result <- df$codigo[
    grepl(input_value, df$provincia, ignore.case = FALSE) | 
    grepl(input_value, df$codigo2,
          fixed = TRUE,ignore.case = FALSE) | 
    grepl(input_value, df$provincia_1, ignore.case = FALSE)
  ]
  }
  # Return the result
  return(result)
}


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

nfi2 <- function(prov){
## Function to download ifn2 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
## u <- c( 'https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_1_25.html',
##        'https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_26_50.html')
## all_links <- mapply(function(x)inspect_links(x,'#\\d|zip'), u)
u <- miteco_urls_from_paths(c('path21', 'path22'))
all_links <- unlist(Map(function(x)inspect_links(x,'zip'), u), use.names = FALSE)
## parsed <- mapply(function(x){paste("https://www.miteco.gob.es",x, sep ='')}, all_links, USE.NAMES = FALSE)
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
msg <- paste0("Warning: Data for codigo '", prov., "' was not found!\n")
if(is.character(prov))
 prov <- find_provincia_or_codigo(prov)
if(is.na(prov)){
    cat(msg)
    return(invisible(NULL))}
ptt <- prov + 5
if(ptt < 10)
    ptt <- paste0('0', ptt)
ptt <- paste0(ptt, '.zip')
## return(ptt)
parsed. <- parsed[grepl(ptt, parsed)]
if(length(parsed.) == 0){
    cat(msg)
    return(invisible(NULL))
}
return(parsed.)}

nfi3 <- function(prov){
## Function to download ifn3 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
## u <- c('https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_1_25.html',
## 'https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_26_50.html')
u <- miteco_urls_from_paths(c('path31', 'path32'))
all_links <- unlist(Map(function(x)inspect_links(x,"fn3.*\\.zip"), u), use.names = FALSE)
## parsed <- mapply(function(x){paste("https://www.miteco.gob.es",x, sep ='')}, all_links, USE.NAMES = FALSE)
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
msg <- paste0("Warning: Data for codigo '", prov., "' was not found!\n")
if(is.character(prov))
 prov <- find_provincia_or_codigo(prov)
if(is.na(prov)){
    cat(msg)
    return(invisible(NULL))}
if(prov < 10)
    prov <- paste0('0', prov)
prov <- paste0(prov, '.zip')
parsed. <- parsed[grepl(prov, parsed)]
if(length(parsed.) == 0){
    cat(msg)
    return(invisible(NULL))
}
return(parsed.)}

nfi4 <- function(prov, complain = TRUE){
## Function to download ifn4 data using a province code
    if(is.null(prov))
        return(invisible(NULL))
    ## u <- 'https://www.mitueco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html'
u <- miteco_urls_from_paths('path41')
all_links. <- inspect_links(u,'tablas|sig', ignore.case = TRUE) #%>% print()
all_links <- inspect_links(u, "fn4.*\\.zip") #%>% print()
all_links <- all_links[!all_links%in%all_links.]
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
if(!is.character(prov))
prov <- find_provincia_or_codigo(prov)
parsed. <- parsed[grepl(prov, parsed, ignore.case = TRUE)]
    if(length(parsed.) == 0){
        if(complain)
    cat(paste0("Warning: Data for codigo '", prov., "' was not found!\n"))
    return(invisible(NULL))
}
## to solve some wrong urls addind ifn/ifn4    
parsed. <- insert_ifn_ifn4(parsed.)
return(parsed.)}

units. <- c('d','h','ba','n','Hd','v')
names(units.) <- c('mm','m','m2','','m','dm3')

units.. <- units.
names(units..) <- c('cm','m','m2','','m','m3')
