meltTimePerLap <- function(resultsTable) {

  dplyr::inner_join(
    resultsTable %>% dplyr::select(-dplyr::ends_with("_lub"), -dplyr::matches("^Stage [0-9]+")),
    resultsTable %>% dplyr::select("Bib", "Category", dplyr::ends_with("_lub")) %>%
      reshape2::melt(id=c("Bib", "Category"), value.name="stage_value", variable.name = "stage_number"),
    by=c("Bib", "Category"))

}


meltCumulativeRank <- function(resultsTable) {




  rankedCumulativeLaps <- resultsTable %>%
    dplyr::select(Bib, Category, dplyr::ends_with("_lub")) %>%
    reshape2::melt(id = c("Bib","Category"), value.name="stage_value", variable.name = "stage_number") %>%
    dplyr::group_by(Bib, Category)

  maxTime <- max(rankedCumulativeLaps$stage_value %>% na.omit()) + 10

  rankedCumulativeLaps <- rankedCumulativeLaps %>%
    dplyr::mutate(cum=cumsum(ifelse(is.na(stage_value), maxTime, stage_value))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-stage_value) %>%
    reshape2::dcast(Bib + Category ~ stage_number, value.var="cum") %>%
    dplyr::rename_at(.vars=dplyr::vars(dplyr::matches("Stage [0-9]+_lub")), .funs=dplyr::funs( gsub("Stage ([0-9]+).*", "Cum Stage \\1", .)))  %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Cum")), .funs=dplyr::funs(rank=rank))

  rankedCumulativeResultsTable <- dplyr::inner_join(
    resultsTable %>% dplyr::select(-dplyr::ends_with("_lub"), -dplyr::matches("^Stage [0-9]+")),
    rankedCumulativeLaps,
    by=c("Bib", "Category"))

  rankedCumulativeLaps2 <- rankedCumulativeResultsTable %>% dplyr::select(Bib, Category, dplyr::ends_with("_rank")) %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with("_rank")), function(rankname) { return(gsub("Cum (.*)_rank", "\\1", rankname))}) %>%
    reshape2::melt(id=c("Bib", "Category"), value.name="Rank", variable.name="After")

  dplyr::inner_join(
    resultsTable %>% dplyr::select(-dplyr::ends_with("_lub"), -dplyr::matches("^Stage [0-9]+")),
    rankedCumulativeLaps2,
    by=c("Bib", "Category")
  )

}
