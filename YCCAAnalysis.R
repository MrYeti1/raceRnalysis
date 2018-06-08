resultsUrl <- "http://results.smartiming.co.uk/view-race/yccasummer2018rd3over14/" #Broken due to weird Lap / Lap 1 swap
resultsUrl <- "http://results.smartiming.co.uk/view-race/yccasummer2018rd4over14/"

#webpageResponse <- httr::GET(url=resultsUrl)
#httr::stop_for_status(webpageResponse)
#webpageContent <- httr::content(webpageResponse)
#resultsTable <- XML::readHTMLTable(webpageContent)

webPageCurl <- RCurl::getURL(resultsUrl)

rawResultsTable <- XML::readHTMLTable(webPageCurl)$`dt-user-list`


as.time <- function(x) { as.POSIXct(x, format="%M:%OS") }

#If there is an Outlap, or...  then rename it to Lap0
if ("OutLap" %in% (rawResultsTable %>% colnames())) { rawResultsTable <- rawResultsTable %>% rename("Lap0" = "OutLap")}

resultsTable <- rawResultsTable %>% filter(Sex != "N") %>% mutate(
  fullName = paste(`First Name`, Surname),
  Sex = forcats::fct_drop(Sex) %>% forcats::fct_infreq(),
  highlight = fullName %in% c("James Clews", "Robin Groves", "Robyn Culshaw", "Nick Smith"),
  highlightSex = paste0(Sex, highlight) %>% factor(levels = c("MFALSE","MTRUE","FFALSE","FTRUE"))
  ) %>%
  mutate_at(.vars=vars(starts_with("Place")), .funs=as.numeric) %>%
  mutate_at(.vars=vars(matches("^Lap[0-9]+")), as.character) %>%
  mutate_at(.vars=vars(matches("^Lap[0-9]+")), .funs=funs(lub=lubridate::ms, at = as.time)) %>%
  mutate_at(.vars=vars(ends_with("_lub")), .funs=funs(sec=lubridate::period_to_seconds)) %>%
  arrange(`Place Overall`)


resultsTable %>% glimpse


#ggplot(NULL, aes(y=Lap1_lub_sec/60, x=1, color=highlight)) +
#  ggbeeswarm::geom_quasirandom(data=resultsTable[resultsTable$highlight == F,], alpha=0.5) +
#  geom_jitter(data=resultsTable[resultsTable$highlight == T,], width = 0.1, height=0)
#ggplot(resultsTable, aes(y=`Lap1_at`, x=1, color=highlight)) + ggbeeswarm::geom_quasirandom()


resultsStages <- resultsTable %>% select("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex", ends_with("_lub_sec")) %>%
  rename_at(vars(ends_with("_lub_sec")), function(rankname) { return(gsub("(.*)_lub", "\\1", rankname))}) %>%
  reshape2::melt(id=c("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex"))
maxTime <- max(resultsStages$value %>% na.omit()) + 10

ggplot() + geom_boxplot(data=resultsStages, aes(y=value/60, x=variable), fill="grey96") +
  ggbeeswarm::geom_quasirandom(data=resultsStages %>% filter(!highlight), aes(y=value/60, x=variable, color=highlightSex), size=0.75, alpha=0.7) +
  geom_jitter(data=resultsStages %>% filter(highlight), aes(y=value/60, x=variable, color=highlightSex), width=0.2) +
  scale_color_brewer(guide = F, palette = "Paired", drop=F) +
  ylab("Minutes") +
  xlab("Lap") +
  ggtitle("Time per lap") +
  scale_y_continuous(breaks = function(mi, ma) { return(seq(floor(mi[1]), ceiling(mi[2]), 1))}) +
  scale_x_discrete(labels = function(x) { return( gsub("_sec", "", x)) } ) +
  theme_minimal() + theme(plot.background = element_rect(fill="grey95"))
#ggsave(path="~", file="YCCA-R4-TimePerLap.png", width=2+(nLaps*0.5), height=7)


rankedCumulativeLaps <- resultsTable %>%
  select(Bib, ends_with("_lub_sec")) %>%
  reshape2::melt() %>%
  group_by(Bib) %>%
  mutate(cum=cumsum(ifelse(is.na(value), maxTime, value))) %>%
  ungroup() %>%
  select(-value) %>%
  reshape2::dcast(Bib ~ variable) %>%
  rename_at(.vars=vars(matches("Lap[0-9]+_lub_sec")), .funs=funs( gsub("Lap([0-9]+).*", "Cum Lap \\1", .)))  %>%
  mutate_at(vars(starts_with("Cum")), .funs=funs(rank=rank))

rankedCumulativeResultsTable <- inner_join(resultsTable %>% select("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex"), rankedCumulativeLaps, by="Bib")

rankedCumulative <- rankedCumulativeResultsTable %>% select("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex", ends_with("_rank")) %>%
  rename_at(vars(ends_with("_rank")), function(rankname) { return(gsub("Cum (.*)_rank", "\\1", rankname))}) %>%
  reshape2::melt(id=c("fullName", "Bib", "Category", "Sex", "Club", "highlight", "highlightSex"), value.name="Rank", variable.name="After")

nLaps = rankedCumulative$After %>% nlevels()
firstLap = rankedCumulative$After %>% levels() %>% head(1)
lastLap = rankedCumulative$After %>% levels() %>% tail(1)
ggplot(NULL, aes(x=`After`, y=Rank, color=highlightSex, group=Bib)) +
  geom_line(data=rankedCumulative %>% filter(!highlight), alpha=0.4) +
  geom_line(data=rankedCumulative %>% filter(highlight)) +
  ggtitle("Rank after each lap") +
  scale_color_brewer(guide=F, palette = "Paired", drop=F) +
  scale_y_reverse(breaks=ggthemes::extended_range_breaks()(rankedCumulative$Rank)) +
  ggthemes::geom_rangeframe(data=rankedCumulative, color="black") +
  theme_minimal() +
  geom_text(data=rankedCumulative %>% filter(highlight) %>% filter(After == firstLap), aes(x=After, y=Rank, label=Rank), hjust = 1.1) +
  geom_text(data=rankedCumulative %>% filter(highlight) %>% filter(After == lastLap), aes(x=After, y=Rank, label=Rank), hjust=-0.1) +
  ggrepel::geom_text_repel(data=rankedCumulative %>% filter(highlight) %>% filter(After == "Lap 5"), aes(x=After, y=Rank, label=fullName)) +
  theme(plot.background = element_rect(fill="grey95"))
ggsave(path="~", file="YCCA-R4-CumulativeRank.png", width=2+(nLaps*0.5), height=7)
