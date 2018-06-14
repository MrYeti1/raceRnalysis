devtools::install("raceRnalysis")
library(raceRnalysis)
library(dplyr)
library(ggplot2)

resultsUrl <- "https://www.rootsandrain.com/race6667/2018-mar-25-tweedlove-whyte-vallelujah-glentress/results/filters/overall/ajax/filtered-content/race-results?/filters/overall/&format=overall&sex=&s1=-1"
resultsUrl <- "https://www.rootsandrain.com/race6668/2018-jun-10-tweedlove-uk-enduro-national-champs-18-glentress/results/filters/overall/ajax/filtered-content/race-results?/filters/overall/&format=overall&sex=&s1=-1"

webPageCurl <- RCurl::getURL(resultsUrl)

rawResultsTable <- XML::readHTMLTable(webPageCurl)$T1

guessedStageNames <- c("Morning ...", "Fool's G...", "Scotland...", "F.E.A.R.", "Born Sli...", "There's ...")

normResultsTable <- rawResultsTable %>%
  raceRnalysis::normaliseResultsTable(guessedStageNames) %>%
  raceRnalysis::extractSex(Category) %>%
  raceRnalysis::highlightNames(Name, c("James CLEWS", "Rob GROVES", "Nicholas SMITH"))


resultsTable <- normResultsTable %>% raceRnalysis::stageTimesToSeconds()

resultsTable %>% glimpse


######

resultsStages <- resultsTable %>% raceRnalysis::meltTimePerLap()

raceRnalysis::plotTimePerLap(resultsStages, eventTitle="Tweedlove British Enduro", outfile = "~/Desktop/tweed-champs-TimePerStage.png")


######

rankedCumulative <- resultsTable %>% raceRnalysis::meltCumulativeRank() %>% rename(fullName = Name)

raceRnalysis::plotRankCumulative(rankedCumulative, eventTitle="Tweedlove British Enduro", outfile="~/Desktop/tweed-champs-rankedCumulative.png")
