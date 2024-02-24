

install.packages("tidycensus")
library(tidycensus)

install.packages("tidyverse")
library(tidyverse)

library(tigris)
options(tigris_use_cache = TRUE)
library(dplyr)


library(sf)


#ReplaceAPIkey
census_api_key("Insert Key", install = T)

readRenviron("~/.Renviron")


############
#test#######
############

#acs_medianHouseholdIncome<- get_acs(
#  geography = "block", 
#  variables = "B19013_001")

#ACS not to block level

#acs_MedianHouseholdIncome

#View(acs_medianHouseholdIncome)

#standard output

acs_Mhi_County<- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "ohio")



# wide for GIS

acs_Mhi_County_Wide<- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "ohio",
  output = "wide")

View(acs_Mhi_County_Wide)



############
## loading variable discriptor input
######

vars <- load_variables("2022", "acs1")
View(vars)
### named vector for variables

oh_wide_varname <-get_acs(
  geography = "tract",
  state = "OH",
  variables = c(medianIncome = "B19013_001",
                medianAge = "B01002_001"),
  output = "wide"
)



# use census reporter .org to explore data variables much better



#####
### sorting and stuff for organizations
#####

#ordering
arrange(oh_wide_varname, oh_wide_varname$medianIncomeE)
arrange(oh_wide_varname, desc(oh_wide_varname$medianIncomeE))

#subsetting
above30<- filter(oh_wide_varname, medianAgeE >= 30)
above60<- filter(oh_wide_varname, medianAgeE >= 60)
above60<- arrange(above60, medianIncomeE)

View(above60)


above50<- filter(oh_wide_varname, medianAgeE >= 50)
above50<- arrange(above50, medianIncomeE)

View(above50)


#normaliztion




#####
# begin dataset
####

AvgPerCapitaIncomeAmerica <- 65423

generic_var<-c(
  AvPerCapitaIncome = "B19301_001",
  MedianIncome = "B19013_001",
  MedianAge = "B01002_001")


race_var <-c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012")

dis_var <-c(
  DisabilityTotal = "B10052_002",
  DiGrandparentRespForChild = "B10052_008")
  
Ohio_ALL <- get_acs(
  geography = "tract",
  variables = c(generic_var,
                dis_var,
                race_var),
  summary_var = "B03002_001",
  state = "ohio",
  output = "wide",
  geometry = TRUE)

#All_Above50 <- filter(Ohio_ALL, MedianAgeE >= 50)

#View(All_Above50)

#All_Above50<- arrange(All_Above50, desc(DiGrandparentRespForChildE))

#MobilityNeed50<- All_Above50 %>% 
#  mutate(DisabGranResChiPerc = 100 * (DiGrandparentRespForChildE /summary_est))




MobilityNeed <- Ohio_ALL %>% 
  mutate(DisabGranResChiPercE = 100 * (DiGrandparentRespForChildE /summary_est)) %>%
  mutate(DisabGranResChiPercM = moe_prop(DiGrandparentRespForChildE,
                                         summary_est,
                                         DiGrandparentRespForChildM,
                                         summary_moe)) %>%
  mutate(DisabilityTotalPercE = 100 * (DisabilityTotalE/summary_est)) %>%
  mutate(DisabilityTotalPercM = moe_prop(DisabilityTotalE, summary_est,
                                         DisabilityTotalM, summary_moe)) %>%
  filter(DisabilityTotalPercE >=3) %>%
  filter(DiGrandparentRespForChildE > 0) %>%
  filter(AvPerCapitaIncomeE < AvgPerCapitaIncomeAmerica) %>%
  filter(summary_est >= 3000)


GeneralNeed <- Ohio_ALL %>% 
  mutate(DisabGranResChiPercE = 100 * (DiGrandparentRespForChildE /summary_est)) %>%
  mutate(DisabGranResChiPercM = moe_prop(DiGrandparentRespForChildE,
                                         summary_est,
                                         DiGrandparentRespForChildM,
                                         summary_moe)) %>%
  mutate(DisabilityTotalPercE = 100 * (DisabilityTotalE/summary_est)) %>%
  mutate(DisabilityTotalPercM = moe_prop(DisabilityTotalE, summary_est,
                                         DisabilityTotalM, summary_moe)) %>%
  filter(AvPerCapitaIncomeE < (AvgPerCapitaIncomeAmerica*0.75))
  


  
MobilityNeed<- arrange(MobilityNeed, MedianIncomeE)

View(MobilityNeed)

######

##########
## tigris mapping
#########
install.packages("mapview")
library(mapview)
install
library(sf)
install.packages("leafsync")
library(leafsync)


oh_tracts<- tracts(state="OH", cb=T) #cb is cartographic boundary
plot(oh_tracts$geometry)

mapview(oh_tracts) #woah this is cool


install.packages("crsuggest")
library(crsuggest)

#tigris baked into tidycensus so just geomtry = T


####
#mapping with my data
###

mapview(MobilityNeed, zcol = "DiGrandparentRespForChildE")

#show more than one layer

mapview(oh_tracts) + mapview(MobilityNeed, zcol = "DisabGranResChiPercE")


# publish button right to r studio rpubs

# export -> save as webpage

#best way?
# assign to a variable then use "html widget package"

install.packages("htmlwidgets")

m1 <-mapview(MobilityNeed, zcol = "DiGrandparentRespForChildE")
htmlwidgets::saveWidget(m1, "mymap.html")


### tmap

install.packages("tmap")
library(tmap)

tm_shape(MobilityNeed) + 
  tm_polygons()

install.packages("shinyjs")
library(shinyjs)
tmaptools::palette_explorer()
#viridis is scientifically tested for colorblindness holy fuck

# pallete paremter lets you chose different color value
#   Chlorprleth should only be used for percents

# style paramter can be set to "equal", "quantile", and "jenks"

tm_shape(MobilityNeed) +
  tm_polygons(col = "DisabilityTotalPercE",
              style = "jenks",
              n = 8,
              palette = "Purples",
              title = "percent Disabled Grandparents 
Responsible for Dependents Under 18"
  ) +
  tm_layout(title = "FEMA BRIC Fundable Communities by Census Tracts
with Heigthened Mobility and ADA Needs",
  frame=FALSE,
  legend.outside = TRUE)



GeneralNeed$AvPerCapitaIncomeE

tm_shape(GeneralNeed) +
  tm_polygons(col = "AvPerCapitaIncomeE",
              style = "jenks",
              n = 8,
              palette = "Reds",
              title = "percent Disabled Grandparents 
Responsible for Dependents Under 18"
  ) +
  tm_layout(title = "FEMA BRIC Fundable Communities by Census Tracts
with Heigthened Mobility and ADA Needs",
frame=FALSE,
legend.outside = TRUE)






st_write(MobilityNeed, "")
st_write(GeneralNeed, "")


#note tm bubbles

#use "facetted" or small multiples

#tm_shape(MobilityNeed) +
#  tm_facets( by = "variable") +
#  tm_polygons(col = "DisabilityTotalPercE",
#              style = "jenks",
#              n = 8,
#              palette = "Purples",
#              title = "percent Disabled Grandparents 
#Responsible for Dependents Under 18"
#  ) +
#  tm_layout(title = "FEMA BRIC Fundable Communities by Census Tracts
#with Heigthened Mobility and ADA Needs",
#frame=FALSE,
#legend.outside = TRUE)

#also look into slice_sample with dot density which will smaple dots perportional
#to data values (takes a point for every 50 people and scatters)

#shift_geo for showing hawaii and alaska using albersusa geometries
#  set "shift_geo = T"

####
#Dont forget st write ### st_write(data)

##########################################



#####
# spatial analysis
#####



###############

library(terra)
library(tmap)
install.packages("rastertopoints")
dem <- rast("")
tm_shape(dem)+
  tm_raster(style= "cont", palette="-Blues")+
  tm_layout(legend.outside = TRUE)
cells(dem)
#over 14 million, way too many for rhino

demEasy<-terra::aggregate(dem, fact = 200)

tm_shape(demEasy)+
  tm_raster(style= "cont", palette="-Blues")+
  tm_layout(legend.outside = TRUE)
#way too few

demMedium<-terra::aggregate(dem, fact=1000)
tm_shape(demMedium)+
  tm_raster(style= "cont", palette="-Blues")+
  tm_layout(legend.outside = TRUE)
#oh im inverting the logic of factor of aggregation as additive process

demBetter<-terra::aggregate(dem, fact=10)
tm_shape(demBetter)+
  tm_raster(style= "cont", palette="-Blues")+
  tm_layout(legend.outside = TRUE)
cellSize(demBetter)
cells(demBetter)

cells(demBetter)
139625+1000

SampleDEM<-spatSample(demBetter, method = "regular", size = 140625, as.points = TRUE, xy= TRUE, values = TRUE)
plot(SampleDEM)
View(SampleDEM)

demSample<-spatSample(demBetter, method = "regular", size = 140625, as.df = TRUE, xy= TRUE, values = TRUE)
View(demSample)
#almost but the crs is northing easting, should convert to nad 3618?????
#its 6318
write.csv(demSample, "" )

crs(demBetter, proj=FALSE, describe=FALSE, parse=FALSE)
crs(demBetter, proj=FALSE, describe=T, parse=FALSE)

#demProjected <- project(demBetter, 6318)
##error

#nrow(demBetter)
#375
ncol(demBetter)
#375


#demProjected <- project(demBetter, "3754", "6318")
#error

#demProjected<- project(dem, "EPSG:6318")

demBetterProj <- project(demBetter, "EPSG:6318")

demSampleProj<-spatSample(demBetterProj, method = "regular", size = 140625, as.df = TRUE, xy= TRUE, values = TRUE)
View(demSampleProj)
demRhino<- demSampleProj %>%
  rename("X"=x, "Y"=y, "Z" = DEMmerged)
View(demRhino)

sum(is.na(demRhino))
demRhinoClean <- na.omit(demRhino)
View(demRhinoClean)
write.csv(demRhinoClean, "")


demShapeFinal<-spatSample(demBetterProj, method = "regular", size = 140625, as.point = TRUE, xy= TRUE, values = TRUE)
View(demShapeFinal)
plot(demShapeFinal)

# watch for false object ID in the first column. This will throw the project import
# in Rhino



#############
###
#############







