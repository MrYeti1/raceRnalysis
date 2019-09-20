#curl https://www.strava.com/api/v3/athlete/activities -H "Authorization: Bearer bearertoken"

#curl -G https://www.strava.com/api/v3/activities/1111222233 -H "Authorization: Bearer bearertoken"
library(httr)
library(dplyr)
library(ggplot2)

athleteActivitiesUrl <- paste0("https://www.strava.com/api/v3/athlete/activities")
athleteBearer <- paste(outhContent()$token_type, outhContent()$access_token, sep=" ")
athleteBearer <- "Bearer bearertoken"

athleteStatsResponse <- httr::GET(athleteActivitiesUrl, httr::add_headers(Authorization = athleteBearer ), query=list(per_page=200))
stop_for_status(athleteStatsResponse)


activities <- jsonlite::fromJSON(content(athleteStatsResponse, as = "text")) %>% select(-athlete, -map) %>% mutate(start_date = as.POSIXct(start_date))
runs <- activities %>% filter(type=="Run")
ride <- activities %>% filter(type=="Ride")

bramleyRuns <- runs %>% filter(name %>% grepl('bramley', ., ignore.case=T), name %>% grepl('parkrun', ., ignore.case=T))
bramleyRuns <- bramleyRuns %>% head(3)

getActivityStreamFrame <- function(actId) {
  activityStreamUrl <- paste0("https://www.strava.com/api/v3/activities/", actId, "/streams?keys=time,distance,altitude,latlng,velocity_smooth")
  activityStreamResponse <- httr::GET(activityStreamUrl, httr::add_headers(Authorization = athleteBearer ), query=list(per_page=200))
  stop_for_status(activityStreamResponse)
  activityStream <- jsonlite::fromJSON(content(activityStreamResponse, as = "text"))
  
  activityStreamFrame <- as.data.frame(activityStream$data, col.names = activityStream$type)
  activityStreamFrame$activityId <- as.character(actId)
  return(activityStreamFrame)
}

allStreamsFrame <- bramleyRuns$id %>% sapply(getActivityStreamFrame, simplify = F) %>% bind_rows() %>% mutate(activityId = as.factor(activityId))


ggplot(allStreamsFrame , aes(x=distance, color=activityId)) + geom_point(aes(y=velocity_smooth)) + geom_smooth(aes(y=velocity_smooth), se=F)

BUCKET_DISTANCE = 50

bucketedDistanceFrame <- allStreamsFrame  %>% mutate(gDistance = cut_width(distance, BUCKET_DISTANCE, labels=F, boundary=0)) %>% group_by(activityId, gDistance) %>% summarise_all(mean)
PBActivityId <- (bramleyRuns %>% arrange(elapsed_time) %>% head(1))$id
PBRecord <- bucketedDistanceFrame %>% filter(activityId == PBActivityId)
PBRecord <- bucketedDistanceFrame[bucketedDistanceFrame$activityId == PBActivityId, ] %>% ungroup

compBest <- function (gd, velocity_smooth){ 
  return(velocity_smooth >= PBRecord[gd,"velocity_smooth"]$velocity_smooth)
}
diffBest <- function (gd, velocity_smooth){ 
  return(velocity_smooth - PBRecord[gd,"velocity_smooth"]$velocity_smooth)
}

a <- bucketedDistanceFrame %>% 
  group_by(gDistance) %>% 
  arrange(gDistance, activityId) %>%
  mutate(
    fasterPrev = velocity_smooth >= lag(velocity_smooth),
    fasterFastest = velocity_smooth == max(velocity_smooth), 
    diffPrev = velocity_smooth - lag(velocity_smooth),
    diffFastest = velocity_smooth - max(velocity_smooth), 
  ) %>% mutate ( 
    fasterPB = compBest(gDistance, velocity_smooth),
    diffPB = diffBest(gDistance, velocity_smooth)
    )


ggplot(a %>% ungroup() %>% mutate(activityId = forcats::fct_rev(activityId)), 
       aes(x=gDistance*BUCKET_DISTANCE, y=altitude)) + 
  geom_point(aes(color=diffPB)) + 
  facet_wrap(activityId~., ncol=1) +
  scale_color_gradient2(name="Pace Difference m/s", low="#d7191c", high="#2c7bb6", mid="grey90", na.value="grey80") +
  ggtitle("Elevation Speed Difference")

nameVector <- paste(bramleyRuns$start_date, bramleyRuns$name, lubridate::seconds_to_period(bramleyRuns$moving_time), sep=" - ") 
names(nameVector) <- bramleyRuns$id

(splitPlot <- ggplot(a %>% ungroup() %>% mutate(activityId = forcats::fct_rev(activityId)), 
       aes(x=gDistance*BUCKET_DISTANCE)) + 
  geom_raster(aes(y="lastTime", fill=fasterPrev)) + 
  geom_raster(aes(y="fastestSplit", fill=fasterFastest)) + 
    
    geom_raster(aes(y="PB Pace", fill=fasterPB)) + 
  xlab("distance (m)") + ylab(NULL) +
  facet_wrap(activityId~., ncol=1, labeller = labeller(activityId = nameVector)) + 
  scale_fill_manual(name=NULL, values=c("TRUE"="#2c7bb6", "FALSE"="#d7191c"), na.value="grey80", labels=c("TRUE"="Faster", "FALSE"="Slower", "NA"="No Comparison") ) +
  #theme_minimal() + 
  theme(
  
    legend.position = "bottom",
    plot.background = element_rect(fill="grey90"),
    legend.background = element_rect(fill="grey90"),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_blank(), 
    #axis.ticks.y.left = element_line(color="grey80"), 
    axis.ticks = element_line(color="grey70"), 
    strip.background = element_rect(fill="grey70"),
    
    axis.ticks.length.y =unit(.4, "cm"),
    legend.key = element_rect(fill = NULL, colour = NA)
    ) +# xlim(c(0,5100)) +
    #scale_x_continuous(lim)
  ggtitle("Bramley Parkruns - Vs PB Pace, LastTime, FastestSplit") +
 coord_cartesian(xlim=c(0,max(a$distance) %>% signif(digits = 2)), expand = F))

ggsave(splitPlot, filename = "BramleySplits.png", dpi=300)

ggplot(allStreamsFrame, aes(x=distance, y=altitude)) + geom_area(alpha=0.2, size=2, aes(color=activityId)) + geom_boxplot()
ggplot(allStreamsFrame, aes(x=distance, y=altitude)) + geom_point(alpha=0.2, size=2, aes(group=activityId))

max(a$diffFastest, a$diffPB, a$diffPrev, na.rm=T)
max(a$diffPB, na.rm=T)

(splitPacePlot <- ggplot(a %>% ungroup() %>% mutate(activityId = forcats::fct_rev(activityId)), 
                     aes(x=gDistance*BUCKET_DISTANCE)) + 
    geom_raster(aes(y="lastTime", fill=diffPrev)) + 
    geom_raster(aes(y="fastestSplit", fill=diffFastest)) + 
    geom_raster(aes(y="PB Pace", fill=diffPB)) + 
    xlab("distance (m)") + ylab(NULL) +
    facet_wrap(activityId~., ncol=1, labeller = labeller(activityId = nameVector)) + 
  #  scale_fill_manual(name=NULL, values=c("TRUE"="#232388", "FALSE"="#D2222D"), na.value="grey80", labels=c("TRUE"="Faster", "FALSE"="Slower", "NA"="No Comparison") ) +
    scale_fill_gradient2(name="Pace Difference m/s", low="#d7191c", high="#2c7bb6", mid="grey90", na.value="grey80") +
    #theme_minimal() + 
    #scale_fill_distiller(type="div", palette = "RdYlBu", direction = 1, limits = c(-2,2)) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill="grey90"),
      legend.background = element_rect(fill="grey90"),
      panel.ontop = TRUE,
      panel.background = element_rect(fill = NA),
      panel.grid.major.y = element_blank(), 
      #axis.ticks.y.left = element_line(color="grey80"), 
      axis.ticks = element_line(color="grey70"), 
      strip.background = element_rect(fill="grey70"),
      
      axis.ticks.length.y =unit(.4, "cm"),
      legend.key = element_rect(fill = NULL, colour = NA)
    ) +# xlim(c(0,5100)) +
    #scale_x_continuous(lim)
    ggtitle("Bramley Parkruns - Vs PB Pace, LastTime, FastestSplit") +
  coord_cartesian(xlim=c(0,max(a$distance) %>% signif(digits = 2)), expand = F) +
    scale_y_discrete(labels = labelFunc)
)  
ggsave(splitPacePlot, filename = "BramleySplitPace.png", dpi=300)

labelFunc <- function(x, y=NA) {
  print(x)
  print(y)
  return(x)
}

(lastEventSplitPace <- ggplot(a %>% ungroup() %>% filter(activityId == last(levels(activityId))), 
                         aes(x=gDistance*BUCKET_DISTANCE))  + 
    geom_raster(aes(y="lastTime", fill=diffPrev)) + 
    geom_raster(aes(y="fastestSplit", fill=diffFastest)) + 
    geom_raster(aes(y="PB Pace", fill=diffPB)) + 
    xlab("distance (m)") + ylab(NULL) +
    facet_wrap(activityId~., ncol=1, labeller = labeller(activityId = nameVector)) + 
    #  scale_fill_manual(name=NULL, values=c("TRUE"="#232388", "FALSE"="#D2222D"), na.value="grey80", labels=c("TRUE"="Faster", "FALSE"="Slower", "NA"="No Comparison") ) +
    scale_fill_gradient2(name="Pace Difference m/s", low="#d7191c", high="#2c7bb6", mid="grey90", na.value="grey80") +
    #theme_minimal() + 
    #scale_fill_distiller(type="div", palette = "RdYlBu", direction = 1, limits = c(-2,2)) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill="grey90"),
      legend.background = element_rect(fill="grey90"),
      panel.ontop = TRUE,
      panel.background = element_rect(fill = NA),
      panel.grid.major.y = element_blank(), 
      #axis.ticks.y.left = element_line(color="grey80"), 
      axis.ticks = element_line(color="grey70"), 
      strip.background = element_rect(fill="grey70"),
      
      axis.ticks.length.y =unit(.4, "cm"),
      legend.key = element_rect(fill = NULL, colour = NA)
    ) +# xlim(c(0,5100)) +
    #scale_x_continuous(lim)
    ggtitle("Bramley Parkruns - Vs PB Pace, LastTime, FastestSplit") +
    coord_cartesian(xlim=c(0,max(a$distance) %>% signif(digits = 2)), expand = F)
)  
ggsave(lastEventSplitPace, filename = "LastEventSplitPace.png", dpi=300, height=unit(2.5, "in"), width=unit(6.12, "in"))


activityFasterPercents <- a %>% ungroup() %>% group_by(activityId) %>% summarise(
  fasterPrevPercent = round(sum(fasterPrev, na.rm=T) / n() * 100),
  fasterFastestPercent = round(sum(fasterFastest, na.rm=T) / n() * 100),
  fasterPBPercent = round(sum(fasterPB, na.rm=T) / n() * 100)
)

#Plan:
# * Work out average pace at each gDistance
# * Work out fastest pace at each gDistance
# * Work out average pace from last three runs at each gDistance
# * Show three bars for each 
# * Show percentage of time this run was faster
