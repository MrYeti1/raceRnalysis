devtools::install("raceRnalysis")
library(raceRnalysis)
library(dplyr)
library(ggplot2)

resultsUrl <- "https://www.rootsandrain.com/race6667/2018-mar-25-tweedlove-whyte-vallelujah-glentress/results/filters/overall/ajax/filtered-content/race-results?/filters/overall/&format=overall&sex=&s1=-1"
resultsUrl <- "https://www.rootsandrain.com/race6668/2018-jun-10-tweedlove-uk-enduro-national-champs-18-glentress/results/filters/overall/ajax/filtered-content/race-results?/filters/overall/&format=overall&sex=&s1=-1"

webPageCurl <- RCurl::getURL(resultsUrl)

rawResultsTable <- XML::readHTMLTable(webPageCurl)$T1

stripRank <- function(x) { return(gsub("(([0-9]+:)?[0-9]+.[0-9]+).*", "\\1", x)) }
#"51.56 (12)", "1:09.04 (1)"
addMissingMin <- function(x) { if_else(grepl("^[0-9]+:", x), x, paste0("0:", x)) }

x <- c("26.41", "1:26.41")
x %>% addMissingMin()

as.time <- function(x) { as.POSIXct(x, format="%M:%OS") }

guessedStageNames <- c("Morning ...", "Fool's G...", "Scotland...", "F.E.A.R.", "Born Sli...", "There's ...")

normResultsTable <- rawResultsTable %>%
  raceRnalysis::normaliseResultsTable(guessedStageNames) %>%
  raceRnalysis::extractSex(Category) %>%
  raceRnalysis::highlightNames(Name, c("James CLEWS", "Rob GROVES", "Nicholas SMITH"))


resultsTable <- normResultsTable %>% raceRnalysis::stageTimesToSeconds()

resultsTable %>% glimpse


#ggplot(NULL, aes(y=lubridate::period_to_seconds(`Stage 6_lub`)/60, x=1, color=highlight)) +
#  ggbeeswarm::geom_quasirandom(data=resultsTable[resultsTable$highlight == F,], alpha=0.5) +
#  geom_jitter(data=resultsTable[resultsTable$highlight == T,], width = 0.1)
#ggplot(resultsTable, aes(y=`Stage 6`, x=1, color=highlight)) + ggbeeswarm::geom_quasirandom()


resultsStages <- resultsTable %>% select("Name", "Bib", "Category", "Sponsors", "highlight", "Diff", ends_with("_lub")) %>%
  #mutate_at(vars(ends_with("_lub")), .funs=lubridate::period_to_seconds) %>%
  rename_at(vars(ends_with("_lub")), function(rankname) { return(gsub("(.*)_lub", "\\1", rankname))}) %>%
  reshape2::melt(id=c("Name", "Bib", "Category", "Sponsors", "highlight", "Diff"))

ggplot() + geom_boxplot(data=resultsStages, aes(y=value/60, x=variable)) +
  ggbeeswarm::geom_quasirandom(data=resultsStages %>% filter(!highlight), aes(y=value/60, x=variable, color=highlight), size=0.75, alpha=0.5) +
  geom_jitter(data=resultsStages %>% filter(highlight), aes(y=value/60, x=variable, color=highlight), width=0.2) +
  scale_color_brewer(guide=F, palette = "Paired") +
  ylab("Minutes") +
  xlab("Stage") +
  ggtitle("Time per stage") +
  scale_y_continuous(breaks = function(mi, ma) { return(seq(floor(mi[1]), ceiling(mi[2]), 1))}) +
  theme_minimal()



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


rankedCumulative <- rankedCumulativeResultsTable %>% select("Name", "Bib", "Category", "Sponsors", "highlight", "highlightSex", "Diff", ends_with("_rank")) %>%
  rename_at(vars(ends_with("_rank")), function(rankname) { return(gsub("Cum (.*)_rank", "\\1", rankname))}) %>%
  reshape2::melt(id=c("Name", "Bib", "Category", "Sponsors", "highlight", "highlightSex", "Diff"), value.name="Rank", variable.name="After")


ggplot(NULL, aes(x=`After`, y=Rank, color=highlightSex, group=Bib)) +
  geom_line(data=rankedCumulative %>% filter(!highlight), alpha=0.3) +
  geom_line(data=rankedCumulative %>% filter(highlight)) +
  ggtitle("Rank after each stage") +
  scale_color_brewer(guide=F, palette = "Paired", drop=F) +
  scale_y_reverse(breaks=ggthemes::extended_range_breaks()(rankedCumulative$Rank)) +
  ggthemes::geom_rangeframe(data=rankedCumulative, color="black") +
  theme_minimal() +
  geom_text(data=rankedCumulative %>% filter(highlight) %>% filter(After == "Stage 1"), aes(x=After, y=Rank, label=Rank), hjust = 1.1) +
  geom_text(data=rankedCumulative %>% filter(highlight) %>% filter(After == "Stage 6"), aes(x=After, y=Rank, label=Rank), hjust=-0.1) +
  ggrepel::geom_text_repel(data=rankedCumulative %>% filter(highlight) %>% filter(After == "Stage 4"), aes(x=After, y=Rank, label=Name))
#ggsave(path="~", file="Valleluja-Senior-CumulativeRank.png", width=7, height=7)
