# Geographic representation of (linguistic) data with ggplot2 and shapefiles
# Author: Kristel Uiboaed
# May 16, 2016

# load packages
library("rgdal")
library("rgeos")
library("maptools")
library("ggplot2")
library("plyr")
library("dplyr")
library("ggmap")
library("stringi")
library("gridExtra")
library("splitstackshape")

# Shapefile parts are in a separate folder (estParishDialects) in the working directory.
# Read in the shapefile with readOGR in rgdal package
estParishData <- readOGR("./estParishDialects", "estParishDialects")
plot(estParishData)
summary(estParishData)

# Check the coordinate system
estParishData@proj4string

# Convert the CRS for further processing (e.g. NAD83 and WGS84 are compatible with our corpus geodata)
estParish <- spTransform(estParishData, CRS("+proj=longlat +datum=NAD83"))
estParish@data

# The shapefile contains the column which marks the dialect area of parishes.
# Now we can create a separate layer for dialects and we join polygons by Dialect_en variable.
# So all rows with the same value are joined to one polygon. Dialect is an id in the function call gUnaryUnion (rgeos package).
dialects <- gUnaryUnion(estParish, id = estParish$Dialect_en)
summary(dialects)
#gIsValid(dialects)
plot(dialects)

# how to join dialects with shapefile attribute table, if dialects were not included in the shapefile attribute table
parish.dialect <- read.csv2(file="parish-dialect.csv", header=T)
head(parish.dialect)
estParish@data <- data.frame(estParish@data, parish.dialect[match(estParish@data$Parish_id, parish.dialect$Parish.Dialect),])

# Plot two layers on top of each other
plot(estParish, border="grey")
plot(dialects, add=TRUE)

# Get the centroids of polygons to be used for labeling, use gCentroid from rgeos.
# byid=TRUE: centroid for every polygon not the single centroid for the whole map
# label positions can also be in a separate dataframe with specific coordinates (e.g. not necessarly centroids)
parish.centr <- gCentroid(estParish, byid=TRUE)
plot(estParish, border="grey", lty="dotted")
plot(dialects, add=TRUE)
text(x = parish.centr$x, y = parish.centr$y, labels = as.character(estParish$Parish_id), cex=0.5)

# Create a dataframe from the shapefile (necessary for plotting with ggplot2)
# 1. for parishes, id is the Parish_id-column (abbreviation for Parish), the parish identifier
# 2. for dialects, id is the Dialect_en column.
parish.df <- fortify(estParish, region="Parish_id")
head(parish.df)
str(parish.df)
dialect.df <- fortify(estParish, region="Dialect_en")
head(dialect.df)
str(dialect.df)

# Change the id-column names in df-s.
# fortify changes the id column name automatically to id. Changing the name is not necessary, but makes it easier to merge other tables
names(parish.df)[names(parish.df)=="id"] <- "Parish_id"
names(dialect.df)[names(dialect.df)=="id"] <- "Dialect_en"

# Check the plotting
ggplot(data = parish.df, aes(long, lat, group = group)) + geom_polygon(colour="grey", fill="white") + theme_bw() 
ggplot(data = dialect.df, aes(long, lat, group = group)) + geom_polygon(colour="grey", fill="white") + theme_bw()

# Create the axis theme for map plotting (avoids typing it in every map script)
mapAxisTheme <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank())

# Plot the dialect map from the dialect dataframe with ggplot2
ggplot(data = dialect.df, aes(x = long, y = lat)) +
          geom_polygon(aes(group = group , fill = Dialect_en), colour = 'grey',
                       alpha = .5,
                       show.legend=FALSE) +
          scale_fill_brewer(palette="Set3") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme

# Now we create the list of labels to plot dialect names on the map
# We proceed with both (dialects and parishes at the same time)
# First we create an id-list with parish name abbreviations (not necessary for dialects)
idListPar <- estParish@data$Parish_id

# "coordinates" extracts centroids of the polygons, in the order listed in estParish@data (shapefile attribute table)
# First we extract centroids for dialect polygons and then for parish polygons
# For dialects we use previously created vector layer "dialects"
# For dialects we addtionally create a separate columns from rownames for dialect column (dialects and estParish are different datatypes). For dialects we can use this dataframe for plotting later. For parisehs we continue with some extra steps.
centroidsDial <- as.data.frame(coordinates(dialects))
names(centroidsDial) <- c("Longitude", "Latitude")  # change to meaningful column names in centroid dataframe
centroidsDial$Dialect <- rownames(centroidsDial)

centroidsPar <- as.data.frame(coordinates(estParish))
names(centroidsPar) <- c("Longitude", "Latitude")  # change to meaningful column names in centroid dataframe

# Join the centroid of the polygon with the parish name
centrPar.id <- data.frame(id=idListPar, centroidsPar)
parWithCentr <- merge(parish.df, centrPar.id, by.x="Parish_id", by.y="id")

# Plot the map with parish abbreviations with ggplot
ggplot(data = parWithCentr, aes(x = long, y = lat, group = group)) +
          geom_polygon(aes(group = group), colour = 'grey', fill="yellowgreen",
                       alpha = .5,
                       show.legend=FALSE) +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          geom_text(aes(label = Parish_id, x = Longitude, y = Latitude, size=2.5), show.legend=FALSE, check_overlap = TRUE) +
          guides(fill=FALSE)

# Plot the map with dialect names and dialect polygons
ggplot(data = dialect.df, aes(x = long, y = lat)) +
          geom_polygon(aes(group = group , fill = Dialect_en), colour = 'grey',
                       alpha = .4,
                       size = .1,
                       show.legend=FALSE) +
          scale_fill_brewer(palette="Set3") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          geom_text(data = centroidsDial, aes(x = Longitude, y = Latitude, label = Dialect), size = 3)

# Plot dialects and parishes on the same map with ggplot2
ggplot() +
          geom_polygon(data=dialect.df, aes(x=long, y=lat, group=group, fill=Dialect_en), colour='white', alpha=.2, size =.4, show.legend=F) +
          geom_polygon(data=parish.df, aes(x=long, y=lat, group=group), colour='black', fill='white', alpha=.2, size=.1, show.legend=F) +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          geom_text(data  = parWithCentr, aes(x = Longitude, y = Latitude, label = Parish_id), size = 3.5)


# Now we read in the verb frequency data from the file parish-verb-frequencies.csv
parishVerbFreq <- read.csv2(file="parish-verb-frequencies.csv")
head(parishVerbFreq)

# Merge the verb frequency data with the shapefile attribute table, keep all parishes, even if the infromation from the parish is missing in corpus or the verb frequency is 0. Parishes with no information get NA value, which we replace with zero for plotting (otherwise we would see empty "holes" without borders on the map).
estParishesVerbs <- merge(estParish@data, parishVerbFreq, by="Parish_id", all.x=T, all.y=T, sort=F)
head(estParishesVerbs)

estParishesVerbs <- estParishesVerbs %>%
          mutate(VerbFreqParish = ifelse(is.na(VerbFreqParish), 0, VerbFreqParish))

# Merge the frequency data and previously create shapefile dataframe
# make sure that the dataframe is ordered based on the order-column for plotting the polygons in the right order
# ordedredShapefile <- arrange(shapefile, order)
estParishesVerbsDF <- arrange(join(estParishesVerbs, parish.df, "Parish_id"), order)

# Plot parish verb frequencies
ggplot(data = estParishesVerbsDF, aes(x = long, y = lat, fill = VerbFreqParish, group = group)) +
          geom_polygon(colour = "black") +
          scale_fill_gradient(low = "white", high = "yellowgreen", name = "Verb\nfrequencies\n") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme + 
          theme(legend.text=element_text(size=18), legend.title = element_text(size=15, face="bold"))

# Another way to proceed. Merge the linguistic data and parish centroids data
verbsWithParishCentr <- merge(parishVerbFreq, centrPar.id, by.x="Parish_id", by.y="id")
head(verbsWithParishCentr)

# And we can also plot numbers (frequencies of verbs) on the map
ggplot(data = estParishesVerbsDF, aes(x = long, y = lat, fill = VerbFreqParish, group=group)) +
          geom_polygon(colour = "black") +
          scale_fill_gradient(low = "white", high = "yellowgreen", name = "Verb\nfrequencies\n") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme + 
          theme(legend.text=element_text(size=18), legend.title = element_text(size=15, face="bold")) +
          geom_text(data = verbsWithParishCentr, aes(x = Longitude, y = Latitude, label = VerbFreqParish), size = 3, inherit.aes=FALSE)

# Same for dialects
# Now we read in the verb frequency data from the file dialect-verb-frequencies.csv
dialectVerbFreq <- read.csv2(file="dialect-verb-frequencies.csv")
head(dialectVerbFreq)

# We can
# 1. join the same data with the initial dataframe
# 2. join with previously created dataframe, which already conatains parish frequency infromation
# 3. join with the dialect dataframe
# Here we continue with the second option
# As we do not have dialect areas with zero or missing values, we can skip the value replacement steps.
dialectsVerbsDF <- arrange(merge(estParishesVerbsDF, dialectVerbFreq, by.x="Dialect_en", by.y="Dialect", all.x=T, all.y=T, sort=F), order)
head(dialectsVerbsDF)

# And we can now plot dialect frequency information
ggplot(data = dialectsVerbsDF, aes(x = long, y = lat, group = group, fill = VerbFreqDial)) +
          geom_polygon() +
          geom_polygon(colour = 'grey', alpha = .4, size = .1, show.legend=FALSE) +
          scale_fill_gradient(low = "white", high = "yellowgreen", name = "Verb frequencies\n") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          theme(legend.text=element_text(size=18), legend.title = element_text(size=20, face="bold"))

# Plotting my specific locations. Lat and long information needed in a corresponding dataframe.
# Addtionally we plot dialect names on corresponding areas with uppercase letters
myLocations <- read.csv("my-locations.csv", sep=";", header=T)
head(myLocations)

ggplot(data = dialect.df, aes(x = long, y = lat)) +
          geom_polygon(aes(group = group , fill = Dialect_en), colour = 'black',
                       alpha = .4,
                       size = .1) +
          scale_fill_brewer(palette="Set3", guide=FALSE) +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          geom_text(data = myLocations, aes(x = lon, y = lat, label = Place), size = 4) +
          geom_text(data = centroidsDial, aes(x = Longitude, y = Latitude, label = toupper(Dialect)), size = 5.5)

# We can also change the location of one dialect label, which will be overlayed by one location name.
# And then plot it again
centroidsDial$Latitude[centroidsDial$Dialect=="Mulgi"] = centroidsDial$Latitude[centroidsDial$Dialect=="Mulgi"]+0.1


# Now we plot the density map based on the same verb frequencies, but counted by villages.
# First, we need to read in the information with village coordinates in separate file.
# Then we read in the frequency information with village verb frequencies
villageCoordinates <- read.csv(file = "village-coordinates.csv", sep=";")
villageVerbFreq <- read.csv(file = "village-verb-frequencies.csv", sep=";")

# Add the column with dialect names in English.
villageCoordinates <- mutate(villageCoordinates, Dialect_en = ifelse(Dialect_et == "Ida", "Eastern",
                                                                     ifelse(Dialect_et == "Kesk", "Mid",
                                                                            ifelse(Dialect_et == "Lääne", "Western",
                                                                                   ifelse(Dialect_et == "Kirde", "Northeastern",
                                                                                          ifelse(Dialect_et == "Mulgi", "Mulgi",
                                                                                                 ifelse(Dialect_et == "Ranna", "Coastal",
                                                                                                        ifelse(Dialect_et == "Saarte", "Insular",
                                                                                                               ifelse(Dialect_et == "Setu", "Setu",
                                                                                                                      ifelse(Dialect_et == "Tartu", "Tartu",
                                                                                                                             ifelse(Dialect_et == "Võru", "Võru", "NA")))))))))))


# Merge these two tables, by village and parish, because the same village name can be present in several parishes
villageFreqCoord <- merge(villageCoordinates, villageVerbFreq, by=c("Village", "Parish_id"))
head(villageFreqCoord)

# First, we modify the dataframe by converting it to long format by repeating village frequency values
longVillageFreqs <- expandRows(villageFreqCoord, "VillageVerbFreq")
head(longVillageFreqs)
str(longVillageFreqs)

# We use Google map as a base layer.
est.map <- get_map(location = "estonia", zoom = 7, source = "google")
ggmap(est.map)

# Plotting the density map with village verb frequencies
ggmap(est.map, extent = "panel", maprange=FALSE) +
          geom_density2d(data = longVillageFreqs, aes(x = lon, y = lat)) +
          stat_density2d(data = longVillageFreqs, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level.., show.legend=FALSE), size = 0.01, geom = 'polygon') +
          scale_fill_gradient(low = "green", high = "red", guide=FALSE) +
          scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
          labs(x = "", y = "") +
          mapAxisTheme

# We can also plot the locations of data collection points.
# We use previously retrieved Google map as a base layer
ggmap(est.map) +
          geom_point(data = villageCoordinates, aes(x = lon, y = lat), size=2) +
          labs(x = "", y = "") +
          mapAxisTheme

# We can also plot the villages based on the frequencies of verbs.
# Larger points indicate higher frequency
ggmap(est.map) +
          geom_point(data = villageFreqCoord, aes(x = lon, y = lat, size=VillageVerbFreq)) +
          scale_size_continuous(range=c(1,6), name="Verb frequencies") +
          labs(x = "", y = "") + 
          mapAxisTheme

# Now we use the data of case frequencies based on how they occur with adpositions in Estonian dialects.
adpCase <- read.csv("adp-case.csv", sep=";", header=T)
head(adpCase)
str(adpCase)

# First, we need to change the uppercase dialect names in Estonian into capitalized version (as in the shapefile)
# Then we change dialect names back to factors
adpCase$Dialect_et <- as.factor(stri_trans_general(adpCase$Dialect_et, id = "Title"))

# Now we merge the dataframe (adpCase) with case frequencies and previously created shapefile dataframe
dialectsAdpCaseDF <- arrange(merge(estParishesVerbsDF, adpCase, by.x="Dialect_et", all.x=T, all.y=T, sort=F), order)
head(dialectsAdpCaseDF)

# And we can now plot dialect frequency information
partPlot <- ggplot(data = dialectsAdpCaseDF, aes(x = long, y = lat, group = group, fill = par)) +
          geom_polygon() +
          geom_polygon(colour = 'grey', alpha = .4, size = .1, show.legend=FALSE) +
          scale_fill_gradient(low = "white", high = "yellowgreen", name="") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          theme(legend.text=element_text(size=18), legend.title = element_text(size=20, face="bold")) +
          annotate("text", x = 22.5, y = 59.75, label = "Partitiiv", size=5, fontface="bold")

genPlot <- ggplot(data = dialectsAdpCaseDF, aes(x = long, y = lat, group = group, fill = gen)) +
          geom_polygon() +
          geom_polygon(colour = 'grey', alpha = .4, size = .1, show.legend=FALSE) +
          scale_fill_gradient(low = "white", high = "yellowgreen", name="") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          theme(legend.text=element_text(size=18), legend.title = element_text(size=20, face="bold")) +
          annotate("text", x = 22.5, y = 59.75, label = "Genitiiv", size=5, fontface="bold")

abePlot <- ggplot(data = dialectsAdpCaseDF, aes(x = long, y = lat, group = group, fill = abe)) +
          geom_polygon() +
          geom_polygon(colour = 'grey', alpha = .4, size = .1, show.legend=FALSE) +
          scale_fill_gradient(low = "white", high = "yellowgreen", name="") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          theme(legend.text=element_text(size=18), legend.title = element_text(size=20, face="bold")) +
          annotate("text", x = 22.5, y = 59.75, label = "Abessiiv", size=5, fontface="bold")


adePlot <- ggplot(data = dialectsAdpCaseDF, aes(x = long, y = lat, group = group, fill = ade)) +
          geom_polygon() +
          geom_polygon(colour = 'grey', alpha = .4, size = .1, show.legend=FALSE) +
          scale_fill_gradient(low = "white", high = "yellowgreen", name="", breaks=c(0, 5, 10)) +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          theme(legend.text=element_text(size=18), legend.title = element_text(size=20, face="bold")) +
          annotate("text", x = 22.5, y = 59.75, label = "Adessiiv", size=5, fontface="bold")

elaPlot <- ggplot(data = dialectsAdpCaseDF, aes(x = long, y = lat, group = group, fill = ela)) +
          geom_polygon() +
          geom_polygon(colour = 'grey', alpha = .4, size = .1, show.legend=FALSE) +
          scale_fill_gradient(low = "white", high = "yellowgreen", name="") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          theme(legend.text=element_text(size=18), legend.title = element_text(size=20, face="bold")) +
          annotate("text", x = 22.5, y = 59.75, label = "Elatiiv", size=5, fontface="bold")

komPlot <- ggplot(data = dialectsAdpCaseDF, aes(x = long, y = lat, group = group, fill = kom)) +
          geom_polygon() +
          geom_polygon(colour = 'grey', alpha = .4, size = .1, show.legend=FALSE) +
          scale_fill_gradient(low = "white", high = "yellowgreen", name="") +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme +
          theme(legend.text=element_text(size=18), legend.title = element_text(size=20, face="bold")) +
          annotate("text", x = 22.5, y = 59.75, label = "Komitatiiv", size=5, fontface="bold")

grid.arrange(partPlot, genPlot, abePlot, adePlot, elaPlot, komPlot, nrow=3, ncol=2)



# Combining Google maps and shapefile
ggmap(est.map) +
          geom_polygon(data=dialect.df, aes(x=long, y=lat, group=group, fill=Dialect_en), colour='white', alpha=.2, size =.4, show.legend=F) +
          geom_polygon(data=parish.df, aes(x=long, y=lat, group=group), colour='black', fill='white', alpha=.2, size=.1, show.legend=F) +
          labs(x = "", y = "") +
          theme_bw() +
          mapAxisTheme
