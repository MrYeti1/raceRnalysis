library(httr)
library(dplyr)
library(ggplot2)

stopifnot(!is.null(athleteBearer)) #athleteBearer ~= "Bearer 1234562ce494d8642ed33789199dc3e085456123"

MAXEVENTLENGTHMINS <- 60
eventName <- "Meanwood"

getFlybyActivityStream <- function (baseActivityId, compareActivityId) {

  activityStreamUrl <- paste0('https://nene.strava.com/flyby/stream_compare/',baseActivityId,"/",compareActivityId)
print(activityStreamUrl)
  activityStreamResponse <- httr::GET(activityStreamUrl)
  stop_for_status(activityStreamResponse)
  activityStream <- jsonlite::fromJSON(content(activityStreamResponse, as = "text"))

  activityStreamFrame <- tibble(
    activityId=as.character(compareActivityId),
    lat=activityStream$stream$point$lat,
    long=activityStream$stream$point$lng,
    time=activityStream$stream$time
  )
  return(activityStreamFrame)
}


peopleActivityFrame <- read.csv(paste0("~/raceRnalysis/orienteeringControls/peopleActivities-", eventName,".csv"))
peopleActivities <- as.numeric(peopleActivityFrame$activityId); names(peopleActivities) = peopleActivityFrame$person

activityPeople <- names(peopleActivities)
names(activityPeople) <- peopleActivities
controlsFrame <- read.csv(paste0("~/raceRnalysis/orienteeringControls/controls-", eventName,".csv"))

###############################
## Interactive Map with control destinations
###############################

# Map with control destinations
ggplot(allStreamsFrame %>% arrange(originalTime)) +
  geom_point(aes(x=long, y=lat, color=activityId, alpha=activityId == 1234567)) +
  scale_color_brewer(type="qual", labels=activityPeople, palette = "Dark2") +
  geom_point(data=controlsFrame, aes(x=longitude, y=latitude), size=10, color="purple", shape=1) + coord_quickmap() +
  geom_text(data=controlsFrame, aes(x=longitude, y=latitude, label=control), size=10, color="purple")

#geom_point(data=allStreamsFrame %>% arrange(originalTime) %>% filter(originalTime > startTimeFilter, originalTime < endTimeFilter ), aes(x=long, y=lat, color=activityId), size=5)
plotly::ggplotly() %>%
  htmlwidgets::onRender("
                        function(el) {
                        el.on('plotly_click', function(d) {
                        console.log(d)
                        var pointArray = d.points.reduce(function(acc, p) { acc.x.push(p.x); acc.y.push(p.y); return acc; }, {'x':[],'y':[]} );
                        console.log(
                        (pointArray.y.reduce(function(acc, y) { return acc+y; }, 0) / pointArray.y.length )
                        + ', ' +
                        (pointArray.x.reduce(function(acc, x) { return acc+x; }, 0) / pointArray.x.length)
                        )
                        });
                        }") %>% plotly::toWebGL() %>%
htmlwidgets::saveWidget("~/Desktop/mapClicker.html")

###############################
## Get Distance from points and visited controls
###############################

allStreamsFrame <- peopleActivities %>% sapply(getFlybyActivityStream, baseActivityId=first(peopleActivities), simplify = F) %>% bind_rows() %>% mutate(activityId = as.factor(activityId))

#Do you need to trim a single activity?
#allStreamsFrame <- allStreamsFrame %>% filter((activityId == 1234567 & time < 1579118396) | activityId != 1234567 )


allDistanceMatrix <- geosphere::distm(allStreamsFrame[,c("long","lat")], controlsFrame[,c("longitude","latitude")], fun = geosphere::distHaversine) %>% as.data.frame()
colnames(allDistanceMatrix) <- controlsFrame$control
allDistanceMatrix$activityId <- allStreamsFrame$activityId
allDistanceMatrix$time <- allStreamsFrame$time


allMelt <- allDistanceMatrix %>% reshape2::melt(id.vars=c("activityId", "time"), value.name="distance", variable.name="control") %>% as.data.frame()

closestFrame <- allMelt %>% group_by(activityId, control) %>% slice(which.min(distance))

visitedFrame <- closestFrame %>% ungroup %>% filter(distance < 20)

visitedScores <- inner_join(controlsFrame, visitedFrame, by=c("control")) %>%
  mutate(
     time = as.POSIXct(time, origin="1970-01-01"),
     tenMinute = lubridate::floor_date(time, "10 mins"))

(visitedControls <- visitedScores %>% group_by(activityId) %>%
    summarise(visitedControls = paste0(stringr::str_pad(control,2), collapse=",")) %>%
    mutate(name = activityPeople[as.character(activityId)])
)

###############################
## Controls over time
###############################


penaltyScore <- allMelt %>% group_by(activityId) %>%
  summarise(startTime = min(time), endTime = max(time)) %>%
  mutate(
    startTime = as.POSIXct(startTime, origin="1970-01-01"),
    endTime = as.POSIXct(endTime, origin="1970-01-01"),
    groupStart = median(startTime),
    elapsedTime = endTime - groupStart
  ) %>% group_by(activityId) %>%
  mutate(
    penalty = ceiling(max(MAXEVENTLENGTHMINS,elapsedTime) - MAXEVENTLENGTHMINS) %>% as.numeric()*-10
  )

summedScore <- bind_rows(visitedScores, penaltyScore %>% select(activityId, time=endTime, score = penalty)) %>% group_by(activityId) %>% arrange(time) %>% mutate(cumScore = cumsum(score)) %>% ungroup %>% mutate(activityId = forcats::fct_reorder(activityId, -cumScore))

(tenMinuteScores <- visitedScores %>% group_by(activityId, tenMinute) %>% summarise(sumScore = sum(score)) %>% mutate(cumulativeScore = cumsum(sumScore)))

(finalScores = summedScore %>% group_by(activityId) %>% summarise(finalScore = last(cumScore), time=last(time)))

ggplot(
  summedScore %>% arrange(time),
    aes(x=time, y=cumScore, color=activityId, group=activityId)
  ) +
  geom_line() +
  geom_point() +
  scale_x_datetime(date_breaks="10 mins", date_labels="%H:%M") +
  geom_text(data = finalScores, aes(x=time, y=finalScore, label=finalScore), nudge_x=300, show.legend =FALSE) +

  ggtitle(paste0("Cumulative Score over time - ",eventName)) +
  ylab("Cumulative Score") +
  scale_color_brewer(type="qual", labels=activityPeople, palette = "Dark2") +
  theme(
    plot.background = element_rect(fill="grey90"),
    legend.background = element_rect(fill="grey90"),

    panel.background = element_rect(fill = NA),
    axis.ticks = element_line(color="grey70"),
    panel.grid = element_line(color="grey70"),
    strip.background = element_rect(fill="grey70"),

    axis.ticks.length.y =unit(.4, "cm"))

ggsave(paste0("~/Desktop/StravaControlsTime-",eventName,".png"))

###############################
## Mapping of people who went from control X -> control Y, & their time
###############################

controlPairTime <- summedScore %>%
  group_by(activityId) %>%
  arrange(time) %>%
  select(activityId, control, time) %>%
  mutate(controlPair = paste0(control, " -> ", lead(control)), time = lead(time) - time) %>%
  ungroup %>%
  mutate(controlPair = forcats::fct_infreq(controlPair)) %>%
  add_count(controlPair) %>%
  filter(n > 2)

sortedControlPairTime <- controlPairTime %>%
  select(-control) %>%
  ungroup %>%
  filter(!is.na(time)) %>%
  arrange(controlPair, time) %>%
  mutate(name = activityPeople[as.character(activityId)])

(numFastests <- sortedControlPairTime %>% group_by(controlPair) %>% mutate(first = min(time) == time) %>% ungroup %>% group_by(activityId, name) %>% summarise(numFastests = sum(first)))

ggplot(sortedControlPairTime %>% group_by(controlPair) %>% mutate(first = min(time) == time), aes(y = reorder(controlPair, desc(controlPair)), x=time, color=activityId, size=first)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 100, 2) ) +
  scale_color_brewer(name="Person", type="qual", labels=activityPeople, palette = "Dark2") +
  scale_size_discrete(name="Fastest on Leg", labels=c("Not First", "First")) +
  xlab("Minutes") + ylab("Control Pair") + ggtitle("Fastest Legs") +
  expand_limits(x = 0) +
  theme(
    plot.background = element_rect(fill="grey90"),
    legend.background = element_rect(fill="grey90"),

    panel.background = element_rect(fill = NA),
    axis.ticks = element_line(color="grey70"),
    panel.grid = element_line(color="grey70"),
    strip.background = element_rect(fill="grey70")
  )
ggsave(paste0("~/Desktop/StravaControlsLegs-",eventName,".png"))







