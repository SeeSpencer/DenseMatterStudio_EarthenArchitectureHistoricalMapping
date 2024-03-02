install.packages('dplry')
#installs the package dplyr to library

install.packages('terra')
#installs the package terra into library

library(dplyr)
#opens the package dplyr into r session

library(terra)

### reading in the files for the project
UNhist_REF<-read.csv("//Users//coyoteobjective//Desktop//Fall Studio//process//GIS//forR//UNWHEAP_modforR.csv")
WHEAPmanent_REF<-read.csv("//Users//coyoteobjective//Desktop//Fall Studio//process//GIS//forR//DocumentReferenced.csv")

head(UNhist_REF,2)
head(WHEAPmanent_REF,2)

subUN <- UNhist_REF %>%
  filter(!category == "Natural") %>%
  select("id_no", "latitude", "longitude", "area_hectares" )
  

EarthJoined <- left_join(WHEAPmanent_REF, subUN, by= c("matchID" = "id_no"))
  

RestSites <- anti_join(UNhist_REF, WHEAPmanent_REF, by= c("id_no" = "matchID")) %>%
  filter(!category == "Natural")

AllEarth <-EarthJoined %>% mutate(totalMethods= CEB_ADOBE + Cob +MasonryStoneMortar + Plaster + WattleDaub + Rammed + Shaped) %>%
  mutate(totalMethods= ifelse(totalMethods== "", totalMethods, totalMethods+1))

CEBtrueEarthOnly<-AllEarth %>% filter(CEB_ADOBE == 1)
CEBfalseEarthOnly<- AllEarth %>% filter(CEB_ADOBE == 0)
CEBonly<- CEBtrueEarthOnly %>% filter(totalMethods == 1)
#zero

write.csv(CEBtrueEarthOnly, "/Users/coyoteobjective/Desktop/Fall Studio/process/GIS/finnished_data/CEBtrue.csv", row.names=FALSE)
write.csv(CEBfalseEarthOnly, "/Users/coyoteobjective/Desktop/Fall Studio/process/GIS/finnished_data/CEBfalse.csv", row.names=FALSE)
write.csv(RestSites, "/Users/coyoteobjective/Desktop/Fall Studio/process/GIS/finnished_data/Allelse.csv", row.names=FALSE)


###########

#bigones<-UNhist_REF %>% filter(category == "Cultural")
#bigones<-top_n(bigones, 30, area_hectares)
#way too big

bigones<- AllEarth %>% slice_max(AllEarth$area_hectares, n=15)
bigones <-bigones %>% arrange(area_hectares)
bigonesCEB<- bigones %>% filter(CEB_ADOBE == 1)


ggplot(bigones, aes(x=Wname, area_hectares)) + geom_col() +
  geom_text(aes(label=matchID, vjust=-.70))


longones <-AllEarth %>% slice_max(AllEarth$WyearE-AllEarth$WyearB, n=15)
longones <-longones %>% arrange(WyearE-WyearB)
longonesCEB <- longones %>% filter(CEB_ADOBE== 1)
longonesCEB <-longonesCEB %>% arrange(WyearE-WyearB)

ggplot(longones, aes(x=(WyearE-WyearB), y=Wname)) + geom_col() +
  geom_text(aes(label=matchID, hjust=-.15))


oldones<- AllEarth %>% slice_min(AllEarth$WyearB, n=15)
oldones <-oldones %>% arrange(WyearB)
oldonesCEB <-oldones %>% filter(CEB_ADOBE == 1)

ggplot(oldones, aes(x=WyearB, y=Wname)) + geom_col() +
  geom_text(aes(label=matchID, hjust=-.20))


newones <- AllEarth %>% slice_max(AllEarth$WyearB, n=15)
newones <-newones %>% arrange(WyearB)
newonesCEB <- newones %>% filter(CEB_ADOBE==1)

ggplot(newones, aes(x=WyearB, y=Wname)) + geom_col()





