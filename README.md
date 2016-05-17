### Spatial (linguistic/frequency) data visualization with R

This repository contains R code and examples how to do some basic spatial data visualization with R and ggplot2. The shapefile in a folder *estParishDialects* is the Map of the Place Name Database created by the Institute of Estonian Language and has been modified by Kristel Uiboaed in order to map dialectal data. Some parishes have been renamed and modified in order do correspond the traditional Estonian dialect classification. Dialectal data is retrieved from the [Corpus of Estonian Dialects](http://www.murre.ut.ee/mkweb/ "Corpus of  Estonian Dialects").

***spatialVisR.R*** is the code file that contains examples and steps necessary for map production.

***figure***-folder, see the map examples.

***spatialVisR.RData*** contains all data and code produced with the code.

***preProcessedSpatialVisR.RData*** can be downloaded and loaded into R and it contains read-in shapefiles and other steps, here you can continue with reading in your own data and merging it with read-in shapefiles.

Pay attention to column names in your data when merging your data with spatial data or make the corresponding modifications in the code, if necessary.

See also [http://rpubs.com/kristel/spatial-linguistic-data-vis](http://rpubs.com/kristel/spatial-linguistic-data-vis)