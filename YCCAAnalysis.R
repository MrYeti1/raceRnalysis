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


rankedCumulativeLaps <- resultsTable %>%
  select(Bib, ends_with("_lub")) %>%
  reshape2::melt() %>%
  group_by(Bib) %>%
  mutate(cum=cumsum(ifelse(is.na(value), maxTime, value))) %>%
  ungroup() %>%
  select(-value) %>%
  reshape2::dcast(Bib ~ variable) %>%
  rename_at(.vars=vars(matches("Stage [0-9]+_lub")), .funs=funs( gsub("Stage ([0-9]+).*", "Cum Stage \\1", .)))  %>%
  mutate_at(vars(starts_with("Cum")), .funs=funs(rank=rank))

rankedCumulativeResultsTable <- inner_join(resultsTable %>% select("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex"), rankedCumulativeLaps, by="Bib")

rankedCumulative <- rankedCumulativeResultsTable %>% select("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex", ends_with("_rank")) %>%
  rename_at(vars(ends_with("_rank")), function(rankname) { return(gsub("Cum (.*)_rank", "\\1", rankname))}) %>%
  reshape2::melt(id=c("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex"), value.name="Rank", variable.name="After")


raceRnalysis::plotRankCumulative(rankedCumulative, eventTitle="YCCA Round 4", stageTranslation="Lap") #, "~/Desktop/ycca-4-rankedCumulative.png")
