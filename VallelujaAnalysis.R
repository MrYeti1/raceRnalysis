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

rankedCumulativeResultsTable <- resultsTable %>% #[resultsTable$Category == "19-29",] %>%
  mutate(
    `Cum Stage 1` = `Stage 1_lub`,
    `Cum Stage 2` = `Cum Stage 1` + `Stage 2_lub`,
    `Cum Stage 3` = `Cum Stage 2` + `Stage 3_lub`,
    `Cum Stage 4` = `Cum Stage 3` + `Stage 4_lub`,
    `Cum Stage 5` = `Cum Stage 4` + `Stage 5_lub`,
    `Cum Stage 6` = `Cum Stage 5` + `Stage 6_lub`,
  ) %>%
  mutate_at(vars(starts_with("Cum")), .funs=funs(rank=rank))


rankedCumulative <- rankedCumulativeResultsTable %>% select("Name", "Bib", "Category", "Sponsors", "Sex", "highlight", "highlightSex", "Diff", ends_with("_rank")) %>%
  rename_at(vars(ends_with("_rank")), function(rankname) { return(gsub("Cum (.*)_rank", "\\1", rankname))}) %>%
  reshape2::melt(id=c("Name", "Bib", "Category", "Sponsors", "Sex", "highlight", "highlightSex", "Diff"), value.name="Rank", variable.name="After") %>%
  rename(fullName = Name)


raceRnalysis::plotRankCumulative(rankedCumulative, eventTitle="Tweedlove British Enduro", outfile="~/Desktop/tweed-champs-rankedCumulative.png")
