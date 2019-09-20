devtools::install("raceRnalysis")

library(raceRnalysis)
library(dplyr)
library(ggplot2)


resultsUrl <- "http://results.smartiming.co.uk/view-race/yccasummer2018rd3over14/" #Broken due to weird Lap / Lap 1 swap
resultsUrl <- "http://results.smartiming.co.uk/view-race/yccasummer2018rd4over14/"
resultsUrl <- "https://www.aire.org.uk/results-archive/2019/2019-07-10-holt-park-summer-sprint-league/1_sprint_course_splits.html"
resultsUrl <- "https://www.aire.org.uk/results-archive/2019/2019-07-17-weetwood-summer-sprint-league/1_sprint_course_splits.html"
resultsUrl <- "https://www.aire.org.uk/results-archive/2019/2019-08-20-bradford-grammar-school-and-lister-park/stage1_long_course_splits.html"
resultsUrl <- "https://www.aire.org.uk/results-archive/2019/2019-08-20-bradford-grammar-school-and-lister-park/stage1_long_course_splits.html"
resultsUrl <- "https://www.aire.org.uk/results-archive/2019/2019-08-20-bradford-grammar-school-and-lister-park/stage1_long_course_splits_bgs.html"
webPageCurl <- RCurl::getURL(resultsUrl, .encoding = 'UTF-8')

pageTables <- XML::readHTMLTable(webPageCurl)
rawResultsTable <- pageTables[[(pageTables %>% names %>% first)]]

#WARNING This column guessing requires the `fit` variable to be included from columnLabeller
load(file="columnFit.Rdata")
load(file="columnTdm.Rdata")
colNames <- rawResultsTable %>% colnames
corpus.prod <- tm::VCorpus(tm::VectorSource(colNames %>%stringr::str_replace_all("[0-9]", "X") %>% make.names))
tdm.prod <- tm::DocumentTermMatrix(corpus.prod, control=list(dictionary=tm::Terms(columnTdm), stemming=T, stopwords=F))
tdm_prod <- tdm.prod %>% as.matrix() %>% as.data.frame()

test_pred <- predict(fit, newdata = tdm_prod, type="raw")
labelledCoLNames <- data.frame(colName=colNames, label=test_pred)


#guessedStageNames <- c("1 (217)", "2 (218)", "3 (219)", " (F1)")
#guessedStageNames <- c('1 211','2 210','3 209','4 208','5 207','6 201','7 205','8 204','9 203','10 202','11 200','12 198','13 197','14 196','15 195','16 194','17 192','18 206','19 193','F F1')
#guessedStageNames <- c('1 158', '2 157', '3 163', '4 167', '5 170', '6 166', '7 168', '8 152', '9 162', '10 153', '11 161', '12 164', '13 151', '14 169', '15 159', '16 160', '17 165', 'F F1')
guessedStageNames <- labelledCoLNames %>% filter(label=="stage") %>% pull(colName) %>% as.character

nameHighlights <- c()
normResultsTable <- raceRnalysis::normaliseResultsTable(rawResultsTable %>% select(-one_of(" (S1)", "S S1")), guessedStageNames=guessedStageNames)

resultsTable <- normResultsTable %>% mutate(
  fullName = Name %>% gsub("([a-zA-Z ]*)[[:space:]]*[[:upper:]]*", "\\1", .), Bib=Name, Category=`Age Class` ) %>% #paste(`First Name`, Surname)) %>%
  raceRnalysis::extractSex(`Age Class`) %>%
  raceRnalysis::highlightNames(fullName, nameHighlights) %>%
  mutate_at(.vars=vars(starts_with("Place")), .funs=as.numeric) %>%
  raceRnalysis::stageTimesToSeconds() #%>%


resultsTable %>% glimpse


########



resultsStages <- resultsTable %>% raceRnalysis::meltTimePerLap()
maxTime <- max(resultsStages$stage_value %>% na.omit()) + 10

raceRnalysis::plotTimePerLap(resultsStages, eventTitle="Aire Race the Castles - Bradford", stageTranslation="C") #, "~/Desktop/ycca-4-timePerLap.png")


########

rankedCumulative <- resultsTable %>% raceRnalysis::meltCumulativeRank() #%>% rename(fullName = Name)

raceRnalysis::plotRankCumulative(rankedCumulative, eventTitle="Aire Race the Castles - Bradford Grammar Indoor", stageTranslation="Control" , "~/Desktop/CastlesIndoorRank.png")
