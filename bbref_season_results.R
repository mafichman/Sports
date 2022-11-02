library(tidyverse)
library(baseballr)

# Here's a fun challenge - can we grab data from the Baseball Reference API and look for spurious correlations?

phi <- bref_team_results("PHI", 2022) %>%
  mutate(win = ifelse(str_detect(Result,"W") == TRUE, 1, 0),
         loss = ifelse(str_detect(Result, "L") == TRUE, 1, 0),
         wins = cumsum(win),
         losses = cumsum(loss),
         win_pct = 100*(wins / (wins + losses)),
         delmelle = ifelse(Gm > 77, "2. After Elizabeth", "1. Before Elizabeth"))
  
nym <- bref_team_results("NYM", 2022)  %>%
  mutate(win = ifelse(str_detect(Result,"W") == TRUE, 1, 0),
         loss = ifelse(str_detect(Result, "L") == TRUE, 1, 0),
         wins = cumsum(win),
         losses = cumsum(loss),
         win_pct = 100*(wins / (wins + losses)),
         delmelle = ifelse(Gm > 77, "2. After Elizabeth", "1. Before Elizabeth"))

# number of wins as of 11/1 - added 11-3 record for Phillies in playoffs, 1-2 for Mets

nym %>% 
  rbind(., phi) %>%
  select(Tm, delmelle, win, loss) %>%
  group_by(delmelle, Tm) %>%
  summarize(wins = sum(win),
            losses = sum(loss)) %>%
  mutate(wins = ifelse(Tm == "PHI", wins +11, wins +1),
         losses = ifelse(Tm == "PHI", losses + 3, losses +2),
         win_pct = 100*(wins / (wins + losses))) %>%
  ggplot()+
  geom_bar(aes(y = win_pct, x = Tm, fill = delmelle), 
           stat = "identity", position = "dodge")+
  scale_fill_manual(values = c("blue", "red"))+
  labs(title = "Winning Percentages (Incl Playoffs) By MUSA Administration - 2022",
          subtitle = "Phillies Performance Positively Associated With Delmelle Admin\nMets Unaffected",
          caption = "Data - Baseball Reference")+
  theme_minimal()

