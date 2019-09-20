#' Normalise Results Table
#'
#' Takes a table of results, one column per lap/stage, one row per rider.
#' renames numbered lap and named stage columns to numbered stages

normaliseResultsTable <- function(rawResultsTable, guessedStageNames=c()) {

  if ("OutLap" %in% (rawResultsTable %>% colnames())) { rawResultsTable <- rawResultsTable %>% dplyr::rename("Lap 0" = "OutLap")}

  #Drop some columns if they exist
  suppressWarnings(normalisedResultsTable <- rawResultsTable %>%
    dplyr::select(-dplyr::one_of(c("")), -dplyr::starts_with("Rank")))

  #Rename Lap to Stage
  normalisedResultsTable <- normalisedResultsTable %>%
    dplyr::rename_at(.vars=dplyr::vars(dplyr::starts_with("Lap")), .funs = function(x) { gsub("Lap[ \\.]?", "Stage ", x) })

  #If there are named stages (currently only passed in to this function) rename them
  if (length(guessedStageNames)) {
    normalisedResultsTable <- normalisedResultsTable %>%
      dplyr::rename_at(.vars=dplyr::vars(dplyr::one_of(guessedStageNames)), .funs = function(x) { paste("Stage", seq_along(guessedStageNames)) })
  }
return(normalisedResultsTable)
}

#' Add Sex Column to Results Table, extracting from a suggested field
#'
#' Takes a table of results, and a suggested column to extract the sex from.
extractSex <- function(resultsTable, suggestedColumnName) {
  if ("Sex" %in% colnames(resultsTable)) {
    #We want a M,F,N? as the genders
    return(resultsTable)
  }
  suggestedColumn <- dplyr::enquo(suggestedColumnName)
  resultsTable %>% dplyr::mutate(
    #Sex = !!dplyr::enquo(suggestedColumnName) %>% gsub("(.*)\\(([M|F])\\)", "\\2", .)
    Sex = ifelse(stringr::str_detect(!!suggestedColumn, "[F|W]"), "F", "M")
  )

}

#' Add highlight and highlightSex Column to Results Table
#'
#' Takes a table of results, a suggested column to match and a vector to match against.
highlightNames <- function(resultsTable, suggestedNameColumn, namesToHighlight) {
  columnForName <- dplyr::enquo(suggestedNameColumn)
  resultsTable %>% dplyr::mutate(
    highlight = (match(!!columnForName, namesToHighlight, nomatch = 0) > 0), # columnForName %in% namesToHighlight
    highlightSex = paste0(Sex, highlight) %>% factor(levels = c("MFALSE","MTRUE","FFALSE","FTRUE"))
  )
}

#' RootsAndRain shows the rank within each stage in the stage time - keep only the time info
stripRank <- function(x) { return(gsub("(([0-9]+:)?[0-9]+.[0-9]+).*", "\\1", x)) }
stripRank <- function(x) { return(gregexpr("(([0-9]+:)?[0-9]{1,2}[.:][0-9]+)", x) %>% regmatches(x, .)  %>% lapply(FUN=dplyr::last) ) }

#' Some sites with laps under one minute don't include any minute prefix
addMissingMin <- function(x) { ifelse(grepl("^[0-9]+:", x), x, paste0("0:", x)) }

#' Tidy up the stage time strings, and convert to seconds '_lub' suffix
stageTimesToSeconds <- function(normResultsTable) {
  normResultsTable %>%
    mutate_at(.vars=vars(starts_with("Stage")), .funs=stripRank) %>%
    mutate_at(.vars=vars(starts_with("Stage")), .funs=addMissingMin) %>%
    mutate_at(.vars=vars(starts_with("Stage")), .funs=funs(lub=lubridate::ms)) %>%
    mutate_at(vars(ends_with("_lub")), .funs=lubridate::period_to_seconds)
}


