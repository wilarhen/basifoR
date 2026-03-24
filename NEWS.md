# basifoR 0.6

## Major changes

*The package is now installable on 64-bit Windows systems

*The new getNFI() function can download data from the 2nd to the 4th stages of the Spanish National Inventory using the names or codes of provinces as the main argument, eliminating the need for locally specified URLs

*readNFI() now supports the RODBC::odbcConnectAccess2007 function

*urlToTemp() is deprecated and will be made defunct. Use the replacement function fetchNFI()

## Minor improvements

*readNFI() converts numeric columns that are formatted as factors back
 to numeric while preserving the format of character columns

# basifoR 0.5

## Major changes

*Can Support access extensions of the fourth spanish national inventory

## Minor improvements

* urlToTemp has two new arguments. See the package documentation


# basifoR 0.4

## Major changes

*urlToTemp: API for the SNF has changed: 'www.mapama.gob.es' has been
replaced by 'www.miteco.gob.es'. Besides, Internet resources fail
gracefully with an informative message if the resource is not
available or has changed (and not give a check warning nor error)