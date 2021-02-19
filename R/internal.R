## Internal utility functions used by basifoR

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
          "Package 'ecochange' version ", version) } 

  packageStartupMessage("Type 'citation(\"ecochange\")' for citing this R package in publications.")
  invisible()
}

.onLoad <- function(libname, pkgname){
    op <- options()
    op.FC <- list(url2 = "http://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip",
                  url3 = "http://www.mapama.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/ifn3p01_tcm30-293907.zip",
                  utm = "+proj=utm +zone=utm.z +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                  utm1 = "+proj=utm +zone=utm.z +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                  longlat = '+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs',
                  fapp = 'mcmapply'                  )

toset <- !(names(op.FC) %in% names(op))
  if(any(toset)) options(op.FC[toset])
invisible()

}
