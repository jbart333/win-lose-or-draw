library(stattleshipR)
library(dplyr)
library(ggplot2)

set_token("YOUR_TOKEN_GOES_HERE")


## pull all 'team_game_logs' from the Stattleship API

## set parameters here
sport <- 'basketball'
league <- 'nba'
ep <- 'team_game_logs'
q_body <- list(interval_type="regularseason")

## make the requests and combine the results
tgls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, version=1, verbose=TRUE, walk=TRUE)

## pull out the relevant data
tms <-do.call('rbind', lapply(tgls, function(x) x$teams))
gms <-do.call('rbind', lapply(tgls, function(x) x$games))
lgs <-do.call('rbind', lapply(tgls, function(x) x$team_game_logs))
vns <-do.call('rbind', lapply(tgls, function(x) x$venues))

## note that gms are individual games, so there is only 1 for each game but
##    lgs are team logs and there are 2 for each game, we will use both:
##    - lgs for Tableau
##    - gms for ggplot

## using team IDs, generate readable slugs and names
gms$home_slug <- tms[match(gms$home_team_id, tms$id),]$slug
gms$away_slug <- tms[match(gms$away_team_id, tms$id),]$slug
gms$home_team <- tms[match(gms$home_team_id, tms$id),]$nickname
gms$away_team <- tms[match(gms$away_team_id, tms$id),]$nickname

lgs$team_slug <- tms[match(lgs$team_id, tms$id),]$slug
lgs$opp_slug <- tms[match(lgs$opponent_id, tms$id),]$slug
lgs$team_name <- tms[match(lgs$team_id, tms$id),]$nickname
lgs$opp_name <- tms[match(lgs$opponent_id, tms$id),]$nickname

## using venue IDs, generate useful venue info
gms$venue <- vns[match(gms$venue_id, vns$id),]$name
gms$capacity <- vns[match(gms$venue_id, vns$id),]$capacity
gms$pct_full <- (gms$attendance / gms$capacity)

lgs$venue_id <- gms[match(lgs$game_id, gms$id),]$venue_id
lgs$venue <- vns[match(lgs$venue_id, vns$id),]$name
lgs$capacity <- vns[match(lgs$venue_id, vns$id),]$capacity
lgs$attendance <- gms[match(lgs$game_id, gms$id),]$attendance
lgs$pct_full <- (lgs$attendance / lgs$capacity)

## let's make sure we have the correct number of game logs
if (nrow(lgs) == 30*82) print("we are good to go!") else print("we have a problem with game logs (lgs)")
if (nrow(gms) == 30*82/2) print("we are good to go!") else print("we have a problem with game logs (gms)")

## remove international games which would skew our results
gms <- gms[-grep("The O2 Arena", gms$venue),]
gms <- gms[-grep("Mexico City Arena", gms$venue),]

lgs <- lgs[-grep("The O2 Arena", lgs$venue),]
lgs <- lgs[-grep("Mexico City Arena", lgs$venue),]

## do some cleanup before we save our data

## let's only keep the columns we care about
keeps_lgs <- c("is_home_team", "is_away_team", "team_name", "opp_name", "venue", "capacity", "attendance", "pct_full" )
lgs <- lgs[keeps_lgs]
keeps_gms <- c("attendance", "home_team", "away_team", "venue", "capacity", "pct_full")
gms <- gms[keeps_gms]

## cut down to only 3 decimal places for pct_full
gms$pct_full <- round(gms$pct_full, digits = 3)
lgs$pct_full <- round(lgs$pct_full, digits = 3)

## calculate average attendance 
att <- gms %>% 
  group_by(home_team, away_team) %>% 
  summarise(avg_attendance = mean(attendance), avg_capacity = mean(pct_full), venue = venue[[1]]) %>%
  arrange(-avg_attendance)

## calculate averages for away games
away <- gms %>% 
  group_by(away_team) %>% 
  summarise(avg_attendance = mean(attendance), avg_capacity = mean(pct_full)) %>%
  arrange(-avg_capacity)

## calculate average for home games 
home <- gms %>% 
  group_by(home_team) %>% 
  summarise(avg_attendance = mean(attendance), avg_capacity = mean(pct_full)) %>%
  arrange(-avg_capacity)

## checking out venues
thevenues <- gms %>% 
  group_by(home_team, venue) %>% 
  summarise(avg_attendance = mean(attendance), avg_capacity = mean(pct_full))

## we have stored winning percentage and conference/playoff strings in a simple 30x3 CSV
## file with team names, winning pct, and conf/playoff columns; one could get creative
## and use the API to pull these out, but that is beyond the scope of this example
winpct<-read.csv(file="winpct.csv")
away$winpct <- winpct[match(away$away_team, winpct$team),]$winningpct
home$winpct <- winpct[match(home$home_team, winpct$team),]$winningpct
away$confplay <- winpct[match(away$away_team, winpct$team),]$confplay
home$confplay <- winpct[match(home$home_team, winpct$team),]$confplay

## let's once again make sure we have the correct number of teams in all these data frames
if (nrow(home) == 30) print("home is all set!") else print("we have a problem with home")
if (nrow(away) == 30) print("away is all set!") else print("we have a problem with away")
if (nrow(thevenues) == 30) print("thevenues is all set!") else print("we have a problem with thevenues")
if (nrow(winpct) == 30) print("winpct is all set!") else print("we have a problem with winpct")

## generate plot for home games
ggplot(home, aes(x=winpct, y=avg_capacity, label=home_team, color=confplay)) + geom_text() +
  geom_smooth(aes( group = 1 ), method = "lm", se = FALSE, color = "orange") +
  ggtitle("Home Games Capactiy Ratio vs. Winning %") +
  labs(x="Winning %",y="Capacity Ratio (Avg. Attendance / Areana Capacity)") 

## generate plot for away games
ggplot(away, aes(x=winpct, y=avg_capacity, label=away_team, color=confplay)) + geom_text() +
  geom_smooth(aes( group = 1 ), method = "lm", se = FALSE, color = "orange") +
  ggtitle("Road Games Capactiy Ratio vs. Winning %") +
  labs(x="Winning %",y="Capacity Ratio (Avg. Attendance / Areana Capacity)") 

## calculate correlation
cor(home$winpct, home$avg_capacity)
cor(away$winpct, away$avg_capacity)

## store our data in CSV files, draw.csv is what we pass to Tableau to generate visualization
write.csv(att, file="att.csv")
write.csv(lgs, file="draw.csv")