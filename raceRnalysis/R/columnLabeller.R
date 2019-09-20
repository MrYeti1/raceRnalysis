library(dplyr)
resultUrls <- c("https://www.rootsandrain.com/race6667/2018-mar-25-tweedlove-whyte-vallelujah-glentress/results/filters/overall/ajax/filtered-content/race-results?/filters/overall/&format=overall&sex=&s1=-1"
,"https://www.rootsandrain.com/race6668/2018-jun-10-tweedlove-uk-enduro-national-champs-18-glentress/results/filters/overall/ajax/filtered-content/race-results?/filters/overall/&format=overall&sex=&s1=-1"
,"http://results.smartiming.co.uk/view-race/yccasummer2018rd4over14/"
,"https://www.aire.org.uk/results-archive/2019/2019-07-10-holt-park-summer-sprint-league/1_sprint_course_splits.html"
,"https://www.aire.org.uk/results-archive/2019/2019-07-17-weetwood-summer-sprint-league/1_sprint_course_splits.html"
,"https://www.aire.org.uk/results-archive/2019/2019-08-20-bradford-grammar-school-and-lister-park/stage1_long_course_splits.html"
,"https://www.aire.org.uk/results-archive/2019/2019-08-20-bradford-grammar-school-and-lister-park/stage1_long_course_splits.html"
,"https://www.aire.org.uk/results-archive/2019/2019-08-20-bradford-grammar-school-and-lister-park/stage1_long_course_splits_bgs.html"
)

getColNames <- function(resultsUrl) {
  webPageCurl <- RCurl::getURL(resultsUrl, .encoding = 'UTF-8')

  pageTables <- XML::readHTMLTable(webPageCurl)
  rawResultsTable <- pageTables[[(pageTables %>% names %>% first)]]
  return(rawResultsTable %>% colnames())
}


colNameList <- resultUrls %>% lapply(getColNames)
colNameFrame <- colNameList %>% unlist() %>% lava::trim()

colNameFrame %>% write.csv("~/Desktop/colnames.csv", row.names = F)

refullSet <- left_join(labelledSet, data.frame(colName = colNameList %>% unlist))

labelledSet <- read.csv("~/raceRnalysis/raceRnalysis/labelledColNames.csv") %>% mutate(colName = as.character(colName) %>% stringr::str_replace_all("[0-9]", "X") %>% make.names)

(possibleLabels <- labelledSet$label %>% levels)

trainRatio <- 0.6
trainObs <- sample(nrow(labelledSet), trainRatio * nrow(labelledSet), replace = FALSE)
testObs <- sample(nrow(labelledSet), (1-trainRatio) * nrow(labelledSet), replace = FALSE)
train_dat <- labelledSet[trainObs,]
test_dat <- labelledSet[testObs,]

train_dat <-  bind_rows(train_dat, c(colName="sex",label="sex"), c(colName="Penalties", label="penalties"), c(colName="firstName",label="firstName"), c(colName="lastName",label="surname"), c(colName="id",label="id")) %>%
  mutate(label = factor(label, levels=possibleLabels))
corpus <- tm::VCorpus(tm::VectorSource(train_dat$colName))
tdm <- tm::DocumentTermMatrix(corpus, control=list(stemming=T, stopwords=F))

tdm_train <- tdm %>% as.matrix() %>% as.data.frame()
tdm_train$label <- factor(train_dat$label, levels=possibleLabels)

columnFit <- caret::train(label ~ ., data = tdm_train,
                     method = "ranger",
                     num.trees = 10,
                     importance = "impurity",
                     trControl = caret::trainControl(method = "oob"))

#ctrl <- caret::trainControl(method = "repeatedcv", repeats = 10,
#                            classProbs = F
#) #, summaryFunction = multiClassSummary)
#
#columnFit <- caret::train(label ~ ., data = tdm_train,
#                    method = "svmLinear",
#                    #metric = "ROC",
#                    preProc = c("zv"),# "center", "scale"),
#                    trControl = ctrl)
save(columnFit, file="columnFit.Rdata")
columnTdm <- tdm
save(columnTdm, file="columnTdm.Rdata")


### Confusion matrix of the tested data
test_pred <- predict(columnFit, newdata = tdm_train, type="raw")
caret::confusionMatrix(test_pred, train_dat$label)

(resultTable <- table(
  orig=train_dat$label,
  guess=test_pred
))
propTable <- prop.table(resultTable, margin=1)
#Diagonal = probability that the guess and original are the same in both. Measure the mean of each original.
percCorrect <- ((diag(nrow(resultTable)) * propTable) %>% margin.table(1) %>% mean() * 100 ) %>% round()
ggplot(as.data.frame(propTable), aes(x=orig, y=guess, fill=Freq)) + geom_tile() + viridis::scale_fill_viridis() + coord_equal() + ggtitle(paste0(percCorrect, "% of guessed colours equal original"), subtitle = "Yellow diagonal line suggests all guesses are correct")




corpus.test <- tm::VCorpus(tm::VectorSource(test_dat$colName))
tdm.test <- tm::DocumentTermMatrix(corpus.test, control=list(dictionary=tm::Terms(tdm), stemming=T, stopwords=F))
tdm_test <- tdm.test %>% as.matrix() %>% as.data.frame()
#tdm_test$label <- factor(tdm_test$label, levels=possibleLabels)

test_pred <- predict(columnFit, newdata = tdm_test, type="raw")

(resultTable <- table(
  orig=test_dat$label,
  guess=test_pred
))


actualTest <- c("Pos", "Name", "Age Class", "Time", "S S1", "1 158", "2 157",
                "3 163", "4 167", "5 170", "6 166", "7 168", "8 152", "9 162",
                "10 153", "11 161", "12 164", "13 151", "14 169", "15 159", "16 160",
                "17 165", "F F1")
corpus.prod <- tm::VCorpus(tm::VectorSource(actualTest %>%stringr::str_replace_all("[0-9]", "X") %>% make.names))

tdm.prod <- tm::DocumentTermMatrix(corpus.prod, control=list(dictionary=tm::Terms(tdm), stemming=T, stopwords=F))
tdm_prod <- tdm.prod %>% as.matrix() %>% as.data.frame()

test_pred <- predict(columnFit, newdata = tdm_prod, type="raw")

data.frame(colname=actualTest, predLabel=test_pred)


