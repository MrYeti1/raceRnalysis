
plotTimePerLap <- function(resultsStages, eventTitle="", stageTranslation="Stage", outfile=NA) {
  nLaps = resultsStages$stage_number %>% nlevels()
  nBibs = resultsStages$Bib %>% nlevels()

  dotAlpha = nBibs * -0.001333 + 0.9777

  p <- ggplot() +
    geom_boxplot(data=resultsStages, aes(y=stage_value/60, x=stage_number), fill="grey91") +
    ggbeeswarm::geom_quasirandom(data=resultsStages %>% filter(!highlight), aes(y=stage_value/60, x=stage_number, color=highlightSex), size=0.75, alpha=dotAlpha) +
    geom_jitter(data=resultsStages %>% filter(highlight), aes(y=stage_value/60, x=stage_number, color=highlightSex), width=0.2) +
    scale_color_brewer(guide = F, palette = "Paired", drop=F) +
    ylab("Minutes") +
    xlab(stageTranslation) +
    ggtitle(paste0("Time per ", stageTranslation), subtitle = eventTitle) +
    scale_y_continuous(breaks = function(mi, ma) { return(seq(floor(mi[1]), ceiling(mi[2]), 1))}) +
    scale_x_discrete(labels = function(x) { return( gsub("_lub", "", x) %>% gsub("Stage", stageTranslation, .)) } ) +
    theme_minimal() +
    theme(plot.background = element_rect(fill="grey90"))

  if (!is.na(outfile)) {
    ggsave(file=outfile, plot=p, width=2+(nLaps*0.5), height=7, dpi=300)

  }
  return(p)
}

plotRankCumulative <- function(rankedCumulative, eventTitle="", stageTranslation="Stage", outfile=NA) {
  nLaps = rankedCumulative$After %>% nlevels()
  firstLap = rankedCumulative$After %>% levels() %>% head(1)
  lastLap = rankedCumulative$After %>% levels() %>% tail(1)
  #TODO angle the x-axis text if there are too many controls

  p <- ggplot(NULL, aes(x=`After`, y=Rank, color=highlightSex, group=paste0(Bib, Category))) +
    geom_line(data=rankedCumulative %>% filter(!highlight), alpha=0.4) +
    geom_line(data=rankedCumulative %>% filter(highlight)) +
    ggtitle(paste0("Rank after each ", stageTranslation), subtitle = eventTitle) +
    scale_color_brewer(guide=F, palette = "Paired", drop=F) +
    scale_y_reverse(breaks=ggthemes::extended_range_breaks()(rankedCumulative$Rank)) +
    scale_x_discrete(labels = function(x) { return( gsub("Stage", stageTranslation, x)) } ) +
    ggthemes::geom_rangeframe(data=rankedCumulative, color="black") +
    theme_minimal() +
    geom_text(data=rankedCumulative %>% filter(highlight) %>% filter(After == firstLap), aes(x=After, y=Rank, label=Rank), hjust = 1.1) +
    geom_text(data=rankedCumulative %>% filter(highlight) %>% filter(After == lastLap), aes(x=After, y=Rank, label=Rank), hjust=-0.1) +
    ggrepel::geom_text_repel(data=rankedCumulative %>% filter(highlight) %>% filter(After == paste0("Stage 10")), aes(x=After, y=Rank, label=fullName), force=5, hjust=0.5 ) +
    theme(plot.background = element_rect(fill="grey90"),
          axis.text.x = element_text(angle=60, hjust=1))

  if (!is.na(outfile)) {
    ggsave(file=outfile, plot=p, width=2+(nLaps*0.5), height=7, dpi=300)
  }
  return(p)
}
