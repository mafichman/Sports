library(tidyverse)
library(baseballr)
library(lubridate)

# Here's a fun challenge - can we grab data from the Baseball Reference API and look for spurious correlations?

phi <- bref_team_results("PHI", 2021) %>%
  rbind(., bref_team_results("PHI", 2022)) %>%
  rbind(., bref_team_results("PHI", 2023)) %>%
  rbind(., bref_team_results("PHI", 2024)) %>%
  mutate(win = ifelse(str_detect(Result,"W") == TRUE, 1, 0),
         loss = ifelse(str_detect(Result, "L") == TRUE, 1, 0),
         wins = cumsum(win),
         losses = cumsum(loss),
         win_pct = 100*(wins / (wins + losses)),
         delmelle = ifelse(Gm < 78 & Year == 2022 |
                             Year == 2021, "1. Before Elizabeth", "2. After Elizabeth"))
  
nym <-  bref_team_results("NYM", 2021) %>%
  rbind(., bref_team_results("NYM", 2022))  %>%
  rbind(., bref_team_results("NYM", 2023)) %>%
  rbind(., bref_team_results("NYM", 2024)) %>%
  mutate(win = ifelse(str_detect(Result,"W") == TRUE, 1, 0),
         loss = ifelse(str_detect(Result, "L") == TRUE, 1, 0),
         wins = cumsum(win),
         losses = cumsum(loss),
         win_pct = 100*(wins / (wins + losses)),
         delmelle = ifelse(Gm < 78 & Year == 2022 |
                             Year == 2021, "1. Before Elizabeth", "2. After Elizabeth"))

# number of wins as of 4/13/2023 - added 11-6 record for Phillies in playoffs, 1-2 for Mets
# Phillies 4-8 in regular season, Mets 7-6

nym %>% 
  rbind(., phi) %>%
  select(Tm, delmelle, win, loss) %>%
  group_by(delmelle, Tm) %>%
  summarize(wins = sum(win),
            losses = sum(loss)) %>%
  mutate(win_pct = 100*(wins / (wins + losses))) %>%
  ungroup() %>%
  ggplot()+
  geom_point(aes(y = win_pct, x = delmelle, color = Tm, group = Tm))+
  geom_line(aes(y = win_pct, x = delmelle, color = Tm, group = Tm),
            alpha = 0.6)+
  scale_color_manual(values = c("blue", "red"))+
  ylim(45, 65)+
  labs(title = "Delmelle Locational Effect on Major League Winning Percentages\n2021 - Present (Regular Season)",
          subtitle = "Strong, Positive, Causal Effect Leads to Phillies Success\nMets Falling Into Statistically Significant Despair",
       y = "Winning Percentage",   
       caption = "Data - Baseball Reference, 5/6/24")+
  theme_minimal()


nym %>% 
  rbind(., phi) %>%
  mutate(date = mdy(paste(Date, Year))) %>%
  ggplot()+
  geom_line(aes(x = date, y = win_pct, color = Tm))+
  theme_minimal()


nym %>%
  mutate(game = row_number()) %>%
  rbind(., phi %>%
          mutate(game = row_number())) %>%
  ggplot()+
  geom_line(aes(x = game, y = win_pct, color = Tm))+
  scale_color_manual(values = c("blue", "red"))+
  theme_minimal()
