# devtools::install_github(repo = "saiemgilani/cfbscrapR")
library(tidyverse)
library(data.table)
library(cfbscrapR)
library(sqldf)

seasons_list <- 2013:2020
teams_list <- c("Louisville", "Ohio State", "Indiana")
betting_lines <- data.frame()
schedules <- data.frame()
for (t in teams_list){
  print(t)
  for (s in seasons_list) { 
    print(s)
    
    ## get schedule
    tmp_schedule <- cfbscrapR::cfb_game_info(year = s, team = t) %>% 
      mutate(opponent = ifelse(home_team == t, away_team, home_team), 
             team = t)
    ## add to schedules df
    schedules <- bind_rows(schedules, tmp_schedule)
    
    ## get lines
    tmp_lines <- cfbscrapR::cfb_betting_lines(team = t, year = s) %>% 
      mutate(over_under = ifelse(is.na(over_under), 0, over_under)) %>% 
      mutate(team = t, 
             total_points = home_score + away_score, 
             hit_ou_ind = ifelse(over_under != 0 & (total_points > over_under), 1, 0),
             opponent = ifelse(home_team == t, away_team, home_team), 
             team_score = ifelse(home_team == t, home_score, away_score),
             opp_score = ifelse(home_team == t, away_score, home_score),
             win_ind = ifelse(team_score > opp_score, 1, 0), 
             season_week = paste(season, week, sep = '_')) %>% 
      filter(stringr::str_to_lower(season_type) == 'regular')
    
    ## change columns with "spread" in name to "gambling_line"
    ## "spread" column name causes issues, "spread" is a function name
    names(tmp_lines) <- gsub(pattern = 'spread', replacement = 'gambling_line', x = names(tmp_lines))
    
    tmp_lines <- tmp_lines %>% 
      mutate(negative_mov = opp_score - team_score, 
             gambling_line = as.numeric(gambling_line), 
             team_gambling_line = ifelse(home_team == team, gambling_line, gambling_line*-1), 
             cover_ind = ifelse(negative_mov < team_gambling_line, 1, 0))
    
    ## add to betting_lines df
    betting_lines <- bind_rows(betting_lines, tmp_lines)
    
    } ## end seasons loop

  } ## end teams loop

## remove for space
rm(tmp_lines, tmp_schedule, s, t, season_list, teams_list)

## remove duplicates from game times 
game_times <- schedules %>% 
  distinct(id, start_date)

## add game date to betting_lines
betting_lines <- sqldf("select bl.*, s.start_date
      from betting_lines bl 
      join game_times s on bl.game_id = s.id") %>% 
  data.frame() %>% 
  mutate(game_date = lubridate::ymd(str_sub(start_date, start = 1, end = 10))) %>% 
  select(-start_date)

## filter to weeks where:
## 1) all 3 teams won
## 2) all 3 teams covered
## 3) all 3 teams' games hit the OVER 
betting_lines %>% 
  filter(cover_ind == 1 &       ## team covered
           win_ind == 1 &       ## team won
           hit_ou_ind == 1) %>% ## game hit the OVER
  filter(provider == 'teamrankings') %>% 
  mutate(success_ind = ifelse(cover_ind == 1 & win_ind == 1 & hit_ou_ind == 1, 1, 0)) %>% 
  group_by(season_week) %>% 
  mutate(success_games = sum(success_ind)) %>% 
  ungroup() %>% 
  arrange(-success_games) %>% 
  data.frame() %>%
  filter(success_games == 3)

## 2015_13 
## UL beat UK 38-24 @UK
## IU beat PUR 54-36 @PUR
## OSU beat MICH 42-13 @MICH