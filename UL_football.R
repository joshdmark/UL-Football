# # Then can install using the devtools package from either of the following:
# devtools::install_github(repo = "saiemgilani/cfbscrapR")
# # or the following (these are the exact same packages):
# devtools::install_github(repo = "meysubb/cfbscrapR")

library(tidyverse)
library(data.table)
library(cfbscrapR)
library(sqldf)


# cfbscrapR::cfb_game_info(year = 2019, week = 1) %>% filter(home_conference == 'ACC')

## get louisville schedule
lou_schedule <- cfbscrapR::cfb_game_info(year = 2020, team = 'Louisville') %>% 
  mutate(opponent = ifelse(home_team == 'Louisville', away_team, home_team))

## get game stats 
lou_stats <- cfb_game_player_stats(year = 2020, team = 'Louisville') %>% 
  filter(team == 'Louisville') %>% 
  separate(c_att, sep = '/', into = c('completions', 'attempts')) %>% 
  separate(fg, sep = '/', into = c('fgm', 'fga')) %>% 
  separate(xp, sep = '/', into = c('xpm', 'xpa'))

## get team total stats 
team_stats <- cfb_game_team_stats(year = 2020, team = 'Louisville') %>% data.frame()
## remove 'allowed' columns 
team_stats <- team_stats[, !grepl(pattern = 'allowed', x = names(team_stats))]

## clean team_stats
team_stats <- team_stats %>% 
  mutate(down3_eff = third_down_eff, 
         down4_eff = fourth_down_eff,
         penalties_yards_total = total_penalties_yards) %>% 
  separate(third_down_eff, into = c('down3_attempts', 'down3_success'), sep = '-') %>% 
  separate(fourth_down_eff, into = c('down4_attempts', 'down4_success'), sep = '-') %>% 
  separate(total_penalties_yards, into = c('penalties', 'penalty_yardage'), sep = '-')# %>% 
  # gather(key = 'stat', value = 'metric', -game_id, -school, -conference, -home_away)
## add opponent to team_stats 
team_stats <- sqldf("select t.*, ls.opponent
             from team_stats t 
             left join lou_schedule ls on t.game_id = ls.id")

fwrite(lou_schedule, 'C:/Users/joshua.mark/OneDrive - Accenture/Desktop/Sports/UL Football/lou_schedule.csv')
fwrite(lou_stats, 'C:/Users/joshua.mark/OneDrive - Accenture/Desktop/Sports/UL Football/lou_stats.csv')
fwrite(team_stats, 'C:/Users/joshua.mark/OneDrive - Accenture/Desktop/Sports/UL Football/lou_team_stats.csv')
