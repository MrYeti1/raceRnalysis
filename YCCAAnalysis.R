devtools::install("raceRnalysis")

library(raceRnalysis)
library(dplyr)
library(ggplot2)


resultsUrl <- "http://results.smartiming.co.uk/view-race/yccasummer2018rd3over14/" #Broken due to weird Lap / Lap 1 swap
resultsUrl <- "http://results.smartiming.co.uk/view-race/yccasummer2018rd4over14/"

webPageCurl <- RCurl::getURL(resultsUrl)

rawResultsTable <- XML::readHTMLTable(webPageCurl)$`dt-user-list`




normResultsTable <- raceRnalysis::normaliseResultsTable(rawResultsTable)
resultsTable <- normResultsTable %>% filter(Sex != "N") %>% mutate(
  fullName = paste(`First Name`, Surname)) %>%
  raceRnalysis::extractSex() %>%
  raceRnalysis::highlightNames(fullName, c("James Clews", "Robin Groves", "Robyn Culshaw", "Nick Smith")) %>%
  mutate_at(.vars=vars(starts_with("Place")), .funs=as.numeric) %>%
  raceRnalysis::stageTimesToSeconds() %>%
  arrange(`Place Overall`)


resultsTable %>% glimpse


########



resultsStages <- resultsTable %>% raceRnalysis::meltTimePerLap()
maxTime <- max(resultsStages$stage_value %>% na.omit()) + 10

raceRnalysis::plotTimePerLap(resultsStages, eventTitle="YCCA Round 4", stageTranslation="Lap", "~/Desktop/ycca-4-timePerLap.png")


########

rankedCumulative <- resultsTable %>% raceRnalysis::meltCumulativeRank() #%>% rename(fullName = Name)

raceRnalysis::plotRankCumulative(rankedCumulative, eventTitle="YCCA Round 4", stageTranslation="Lap") #, "~/Desktop/ycca-4-rankedCumulative.png")
