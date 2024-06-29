# basifoR 0.6

## Major changes

*New getNFI() can download data from the 2nd to the 4th stages of the
 spanish national inventory using names/codes of Provincias as main
 argument

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

*urlToTemp: API for the SNFI has changed: 'www.mapama.gob.es' has been
replaced by 'www.miteco.gob.es'. Besides, Internet resources fail
gracefully with an informative message if the resource is not
available or has changed (and not give a check warning nor error)