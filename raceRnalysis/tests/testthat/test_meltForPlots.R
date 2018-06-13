context("meltForPlots Test")
library(raceRnalysis)

test_that("meltTimePerLap", {
  t1 <- data.frame("Bib" = c(1,2,3, 1), "Category"=c("a","a","a","b"), "highlight"=c(F,F,F,F), "unexpected" = c(6,1,6,2),  "Stage 1_lub" = c(10,20,10,40), "Stage 2_lub" = c(100,200,100,400))
  ta <- meltTimePerLap(t1)

  expect_equal(nrow(ta), 8)
  expect_equal(colnames(ta), c("Bib", "Category", "highlight", "unexpected", "stage_number", "stage_value"))
})
