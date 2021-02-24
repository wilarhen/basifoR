## Internal utility functions used by basifoR

conv <- function(nfi,var. = NULL, to = NULL, from = NULL){
    nm <- colnames(nfi)
    if(is.null(var.))
        var. <- nm
    if(is.null(to))
        return(nfi)
    unts. <- getOption('units')
    if(is.null(from)){
        un <- unts.[unts.%in%var.]
        from <- names(un)}
    unts.. <- unts. 
    names(unts..)[unts..%in%var.] <- to
    nfi[,var.] <- data.frame(mapply(function(x,y,z)
        conv_unit(nfi[,x], from = y, to = z),
        var., from, to, SIMPLIFY = TRUE))
    unts3 <- unts..[unts..%in%var.]
    colnames(nfi) <- nm
    attributes(nfi) <- append(attributes(nfi), list(units = unts3))
    return(nfi)} 


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


.onAttach <- function(lib, pkg)
{
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  
  if(interactive())
    { # > figlet basifoR
        packageStartupMessage(
          "basifoR
version: ", version)
}
else
    { packageStartupMessage(
          "Package 'basifoR' version ", version) } 

  packageStartupMessage("Type 'citation(\"basifoR\")' for citing this R package in publications.")
  invisible()
}

units. <- c('d','h','ba','n','Hd','v')
names(units.) <- c('mm','m','m2',NA,'m','dm3')

.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(url2 = "http://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip",
                  url3 = "http://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/ifn3p01_tcm30-293907.zip",
                  utm = "+proj=utm +zone=utm.z +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                  utm1 = "+proj=utm +zone=utm.z +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  longlat = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
                  fapp = 'mcmapply',
                  units = units.)

toset <- !(names(op.FC) %in% names(op))
  if(any(toset)) options(op.FC[toset])
invisible()

}
