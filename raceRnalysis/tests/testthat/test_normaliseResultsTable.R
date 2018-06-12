context("normaliseResultsTable Test")
library(raceRnalysis)

test_that("normaliseResultsTable changes lap columns to stage", {
  t1 <- data.frame(`Bib`=c(1,2,3), `Lap 0`=c(5,6,7), `Lap2`=c(5,64,1), "Lap 3"=c(8,1,5)) %>% dplyr::rename("Lap 0" = "Lap.0")

  expect_equal(colnames(normaliseResultsTable(t1)), c("Bib", "Stage 0", "Stage 2", "Stage 3"))
})


test_that("normaliseResultsTable changes named stages to stage", {
  t1 <- data.frame(`Bib`=c(1,2,3), `The start`=c(5,6,7), `Next one`=c(5,64,1), "FINISHING"=c(8,1,5), check.names=F)

  expect_equal(colnames(normaliseResultsTable(t1, c("The start", "Next one", "FINISHING"))), c("Bib", "Stage 1", "Stage 2", "Stage 3"))
})

test_that("extractSex takes the sex from a column with gender in brackets", {

  t1 <- data.frame(`Bib`=c(1,2,3), `Cat` = c("E1: 16-17", "E1: 18-50", "E1: 16-17 (F)"), `The start`=c(5,6,7), `Next one`=c(5,64,1), "FINISHING"=c(8,1,5), check.names=F)
 actualSex <- extractSex(t1, Cat)$Sex
   expect_equal(
    actualSex,
    c("M", "M", "F")
  )
})

test_that("extractSex doesn't override a sex column", {

  t1 <- data.frame(`Bib`=c(1,2,3), `Sex` = c("M", "M", "F"), `The start`=c(5,6,7), `Next one`=c(5,64,1), "FINISHING"=c(8,1,5), check.names=F, stringsAsFactors = F)
  actualSex <- extractSex(t1)$Sex
  expect_equal(
    actualSex,
    c("M", "M", "F")
  )
})



test_that("highlightNames", {

  t1 <- data.frame(`Bib`=c(1,2,3), `name`=c("a", "b", "c"), Sex=c("F","F","F"), `The start`=c(5,6,7), `Next one`=c(5,64,1), "FINISHING"=c(8,1,5), check.names=F)
  actualHighlights <- highlightNames(t1, name, c("b"))$highlight
  expect_equal(
    actualHighlights,
    c(F,T,F)
  )
})

test_that("highlightNames - produces highlightSex factor", {

  t1 <- data.frame(`Bib`=c(1,2,3), `name`=c("a", "b", "c"), Sex=c("F","F","M"), `The start`=c(5,6,7), `Next one`=c(5,64,1), "FINISHING"=c(8,1,5), check.names=F)
  actualHighlights <- highlightNames(t1, name, c("b"))$highlightSex
  expect_equal(
    actualHighlights,
    c("FFALSE","FTRUE","MFALSE") %>% factor(levels = c("MFALSE","MTRUE","FFALSE","FTRUE"))
  )
})

test_that("StripRank", {
  #"51.56 (12)", "1:09.04 (1)"
  expect_equal(
    stripRank(c("51.56 (12)", "1:09.04 (1)", "01:09.04")),
              c("51.56", "1:09.04", "01:09.04")
  )
})

test_that("Add Missing Min", {
  expect_equal(
    addMissingMin(c("51.56", "1:09.04", "01:09.04")),
    c("0:51.56", "1:09.04", "01:09.04")
  )
})
