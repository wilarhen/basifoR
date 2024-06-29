## Internal utility functions used by basifoR

# Function to convert factor columns to numeric while preserving character columns
convert_factors_to_numeric <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.factor(col) && all(grepl("^-?\\d*\\.?\\d+$", as.character(col)))) {
      return(as.numeric(as.character(col)))
    } else {
      return(col)
    }
  })
  return(df)
}

# Function to find provincia if input is numeric, or codigo/codigo2 if input is character (case insensitive)
find_provincia_or_codigo <- function(input) {
    ## to comment:
    ## load('/home/wihe/Documents/tuh32536/bfRdevel/basifoR/R/sysdata.rda')
    data <- procods
    if (is.numeric(input)) {  # Check if input is numeric
    result <- data$Provincia[grepl(paste0("^", input, "$"), data$Código, ignore.case = TRUE)]
  } else if (is.character(input)) {  # Assume input is character
      result <- data$Código[grepl(input, data$Provincia,
                                  ignore.case = TRUE)]
    if (length(result) == 0) {
        result <- data$Código2[grepl(paste0("^", input, "$"),
                                     data$Provincia, ignore.case = TRUE)]
    }
  } else {
    result <- NA
  }
  if (length(result) == 0) {
    result <- NA
  }
    if(is.na(result))
    cat(paste0("Warning: Provincia or codigo '", input, "' was not found!\n"))
        
  return(result)
}

# Function to inspect links
inspect_links <- function(url, pattern = NULL, ...) {
    webpage <- rvest::read_html(url)
    links <- rvest::html_nodes(webpage, 'a')
    links <- rvest::html_attr(links, "href")
  ## links <- webpage %>% html_nodes("a") %>% html_attr("href")
  if (!is.null(pattern)) {
    links <- links[grepl(pattern, links, perl = TRUE, ...)]
  }
  return(links)
}

## Function to download ifn4 data using a province code
nfi4 <- function(prov){
    if(is.null(prov))
        return(invisible(NULL))
    u <- 'https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html'
all_links. <- inspect_links(u,'tablas|sig', ignore.case = TRUE) #%>% print()
all_links <- inspect_links(u, "fn4.*\\.zip") #%>% print()
all_links <- all_links[!all_links%in%all_links.]
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
if(!is.character(prov))
prov <- find_provincia_or_codigo(prov)
parsed. <- parsed[grepl(prov, parsed, ignore.case = TRUE)]
if(length(parsed.) == 0){
    cat(paste0("Warning: Data for Código '", prov., "' was not found!\n"))
    return(invisible(NULL))
}
return(parsed.)}

## Function to download ifn3 data using a province code
nfi3 <- function(prov){
    if(is.null(prov))
        return(invisible(NULL))
u <- c('https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_1_25.html',
'https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_26_50.html')
all_links <- unlist(Map(function(x)inspect_links(x,"fn3.*\\.zip"), u), use.names = FALSE)
## parsed <- mapply(function(x){paste("https://www.miteco.gob.es",x, sep ='')}, all_links, USE.NAMES = FALSE)
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
msg <- paste0("Warning: Data for Código '", prov., "' was not found!\n")
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

## Function to download ifn2 data using a province code
nfi2 <- function(prov){
    if(is.null(prov))
        return(invisible(NULL))
u <- c( 'https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_1_25.html',
       'https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_26_50.html')
## all_links <- mapply(function(x)inspect_links(x,'#\\d|zip'), u)
all_links <- unlist(Map(function(x)inspect_links(x,'zip'), u), use.names = FALSE)
## parsed <- mapply(function(x){paste("https://www.miteco.gob.es",x, sep ='')}, all_links, USE.NAMES = FALSE)
parsed <- mapply(function(x)httr::modify_url(getOption('server'), path = x), all_links, USE.NAMES = FALSE)
prov. <- prov
msg <- paste0("Warning: Data for Código '", prov., "' was not found!\n")
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



file_exten <- function(texts)
    sapply(texts, function(x) sub(".*\\.(.*)", "\\1", x),
           USE.NAMES = FALSE)


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

flev <- function(vmad, levels){
nma <- names(vmad)
app <- paste(levels, collapse = '|')
gap <- grepl(app,nma, ignore.case = TRUE)
nms <- nma[gap]
return(nms)}

## source: https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/11

gracefully_fail <- function(remote_file, timeOut = timeout(50)) {
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


units. <- c('d','h','ba','n','Hd','v')
names(units.) <- c('mm','m','m2','','m','dm3')

units.. <- units.
names(units..) <- c('cm','m','m2','','m','m3')



## /IFNdyn-master/ github proyect with dominantHeight function for NFI
## https://github.com/miquelcaceres/IFNdyn
domheight<-function(h, d, n) {
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

basifoR_figlet <- function(){
msg <- cat(
## "  _               _  __     _____  
##  | |             (_)/ _|    |  __ \\ 
##  | |__   __ _ ___ _| |_ ___ | |__) |
##  | '_ \\ / _` / __| |  _/ _\\|  _  / 
##  | |_) | (_| \\__ \\ ||| (_) | |\\\\ 
##  |_.__/\\__,_|___/_|_| \\___/|_|  \\_\\  \n\n"
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

msg <- basifoR_figlet()

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
    packageStartupMessage("Type 'citation(\"basifoR\")' for citing this R package in publications.")
    invisible()
}


        

.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(
        ## api = 'www.miteco.gov.es',
        server = "http://www.miteco.gob.es",
        ## url2 = "http://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip",
        ##           url3 = "http://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/ifn3p01_tcm30-293907.zip",
                  utm = "+proj=utm +zone=utm.z +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                  utm1 = "+proj=utm +zone=utm.z +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  longlat = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
                  fapp = 'mcmapply',
                  units = units.,
                  units1 = units..)

toset <- !(names(op.FC) %in% names(op))
  if(any(toset)) options(op.FC[toset])
invisible()

}
