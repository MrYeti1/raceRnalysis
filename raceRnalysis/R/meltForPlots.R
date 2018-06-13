meltTimePerLap <- function(resultsTable) {

  dplyr::inner_join(
    resultsTable %>% dplyr::select(-dplyr::ends_with("_lub")),
    resultsTable %>% dplyr::select("Bib", "Category", dplyr::ends_with("_lub")) %>% reshape2::melt(id=c("Bib", "Category"), value.name="stage_value", variable.name = "stage_number"),
    by=c("Bib", "Category"))

}
