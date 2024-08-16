# Phillies quality of opponent

library(tidyverse)
library(baseballr)
library(lubridate)
library(purrr)

phi <- bref_team_results("PHI", 2024)

opponents <- unique(dat$Opp)

# Function to get results for multiple teams
get_all_team_results <- function(teams, year) {
  # Map over the list of teams and apply bref_team_results
  results <- map(teams, ~ bref_team_results(.x, year))
  
  # Combine all the results into a single DataFrame
  combined_results <- bind_rows(results, .id = "team")
  
  return(combined_results)
}

# Call the function for the 2024 season
all_team_results <- get_all_team_results(opponents, 2024)

test <- all_team_results %>%
  separate(Record, into = c("Wins", "Losses"), sep = "-") %>%
  mutate(Wins_post = as.integer(Wins),
    Losses_post = as.integer(Losses),
    Wins_pre = ifelse(str_detect(Result, "W") == TRUE, Wins_post - 1, Wins_post),
    Losses_pre = ifelse(str_detect(Result, "W") == FALSE, Losses_post - 1, Losses_post),
    Win_Pct_Entering = 100*(Wins_pre / (Wins_pre + Losses_pre)))

phls_and_opp <- left_join(phi, test %>% 
                            select(Tm, Win_Pct_Entering, Date) %>%
                            rename(Opp = Tm,
                                   Opp_Win_Pct_Entering = Win_Pct_Entering) %>%
                            mutate(Opp_PCt_under_500 = 10*(Opp_Win_Pct_Entering - 50))) %>%
  mutate(W_L = ifelse(str_detect(Result, "W") == TRUE, "W", "L")) %>%
  separate(Record, into = c("Wins", "Losses"), sep = "-") %>%
  mutate(phi_win_pct = 100*(as.integer(Wins) / (as.integer(Wins) + as.integer(Losses))))

ggplot()+
  geom_bar(data = phls_and_opp, 
           aes(x = Gm, y = Opp_PCt_under_500/1000, fill = factor(W_L, levels = c("W", "L"))), 
           stat = "identity", alpha = 0.7)+
  scale_fill_manual(values = c("blue", "red"))+
 # geom_line(data = phls_and_opp,
  #              aes(x = Gm, y = (phi_win_pct-50)/100)) +
  geom_text(aes(x = max(phls_and_opp$Gm) - 10, y = 0.25, label = "Opponent above .500"),
            hjust = 1, vjust = 1, color = "black", size = 4) +
  geom_text(aes(x = max(phls_and_opp$Gm) - 10, y = -0.25, label = "Opponent below .500"),
            hjust = 1, vjust = 0, color = "black", size = 4) +
  labs(title = "The 2024 Phillies Struggle Against Winning Teams\n...and Their Schedule Has Been Soft",
       subtitle = "Phillies are .676 in 71 games vs sub-500 teams, .458 in 48 games vs teams over .500",
       x = "Game Number",
       y = "Opp. Win Pct Relative to .500",  
       fill = "Outcome",
       caption = "Data - Baseball Reference, 8/15/24")+
  theme_minimal()

phls_and_opp %>% 
  mutate(opp_over_500 = ifelse(Opp_PCt_under_500 > 0, "Over 500", "Under 500")) %>%
  group_by(opp_over_500, W_L) %>% 
  tally() %>%
  pivot_wider(
    names_from = W_L,
    values_from = n,
    names_prefix = ""
  ) %>%
  mutate(
    W = replace_na(W, 0),
    L = replace_na(L, 0),
    Pct = W/(W+L),
    Total_Games = W+L)


# Now try this with the dodgers

lad <- bref_team_results("LAD", 2024)

opponents_lad <- unique(lad$Opp)

# Call the function for the 2024 season
all_team_results_lad <- get_all_team_results(opponents_lad, 2024)

test_lad <- all_team_results_lad %>%
  separate(Record, into = c("Wins", "Losses"), sep = "-") %>%
  mutate(Wins_post = as.integer(Wins),
         Losses_post = as.integer(Losses),
         Wins_pre = ifelse(str_detect(Result, "W") == TRUE, Wins_post - 1, Wins_post),
         Losses_pre = ifelse(str_detect(Result, "W") == FALSE, Losses_post - 1, Losses_post),
         Win_Pct_Entering = 100*(Wins_pre / (Wins_pre + Losses_pre)))

lad_and_opp <- left_join(lad, test_lad %>% 
                            select(Tm, Win_Pct_Entering, Date) %>%
                            rename(Opp = Tm,
                                   Opp_Win_Pct_Entering = Win_Pct_Entering) %>%
                            mutate(Opp_PCt_under_500 = 10*(Opp_Win_Pct_Entering - 50))) %>%
  mutate(W_L = ifelse(str_detect(Result, "W") == TRUE, "W", "L")) %>%
  separate(Record, into = c("Wins", "Losses"), sep = "-") %>%
  mutate(phi_win_pct = 100*(as.integer(Wins) / (as.integer(Wins) + as.integer(Losses))))

ggplot()+
  geom_bar(data = lad_and_opp, 
           aes(x = Gm, y = Opp_PCt_under_500/1000, fill = factor(W_L, levels = c("W", "L"))), 
           stat = "identity", alpha = 0.7)+
  scale_fill_manual(values = c("blue", "grey"))+
  # geom_line(data = phls_and_opp,
  #              aes(x = Gm, y = (phi_win_pct-50)/100)) +
  geom_text(aes(x = max(lad_and_opp$Gm) - 10, y = 0.25, label = "Opponent above .500"),
            hjust = 1, vjust = 1, color = "black", size = 4) +
  geom_text(aes(x = max(lad_and_opp$Gm) - 10, y = -0.25, label = "Opponent below .500"),
            hjust = 1, vjust = 0, color = "black", size = 4) +
  labs(title = "The 2024 Dodgers Are Holding Their Own Against Winning Teams\nBut they haven't played very many games against them.",
       subtitle = "Dodgers are .602 in 83 games vs sub-500 teams, .514 in 37 games vs teams over .500",
       x = "Game Number",
       y = "Opp. Win Pct Relative to .500",  
       fill = "Outcome",
       caption = "Data - Baseball Reference, 8/16/24\nData exclude results vs. teams with 0-0 record")+
  theme_minimal()

lad_and_opp %>% 
  mutate(opp_over_500 = ifelse(Opp_PCt_under_500 > 0, "Over 500", "Under 500")) %>%
  group_by(opp_over_500, W_L) %>% 
  tally() %>%
  pivot_wider(
    names_from = W_L,
    values_from = n,
    names_prefix = ""
  ) %>%
  mutate(
    W = replace_na(W, 0),
    L = replace_na(L, 0),
    Pct = W/(W+L),
    Total_Games = W+L)

# What about the 2023 Phillies?

phi_2023 <- bref_team_results("PHI", 2023)

opponents_phi_2023 <- unique(phi_2023$Opp)

# Call the function for the 2024 season
all_team_results_phi_2023 <- get_all_team_results(opponents_phi_2023, 2023)

test_phi_2023 <- all_team_results_phi_2023 %>%
  separate(Record, into = c("Wins", "Losses"), sep = "-") %>%
  mutate(Wins_post = as.integer(Wins),
         Losses_post = as.integer(Losses),
         Wins_pre = ifelse(str_detect(Result, "W") == TRUE, Wins_post - 1, Wins_post),
         Losses_pre = ifelse(str_detect(Result, "W") == FALSE, Losses_post - 1, Losses_post),
         Win_Pct_Entering = 100*(Wins_pre / (Wins_pre + Losses_pre)))

phi_2023_and_opp <- left_join(phi_2023, test_phi_2023 %>% 
                           select(Tm, Win_Pct_Entering, Date) %>%
                           rename(Opp = Tm,
                                  Opp_Win_Pct_Entering = Win_Pct_Entering) %>%
                           mutate(Opp_PCt_under_500 = 10*(Opp_Win_Pct_Entering - 50))) %>%
  mutate(W_L = ifelse(str_detect(Result, "W") == TRUE, "W", "L")) %>%
  separate(Record, into = c("Wins", "Losses"), sep = "-") %>%
  mutate(phi_win_pct = 100*(as.integer(Wins) / (as.integer(Wins) + as.integer(Losses))))

ggplot()+
  geom_bar(data = phi_2023_and_opp, 
           aes(x = Gm, y = Opp_PCt_under_500/1000, fill = factor(W_L, levels = c("W", "L"))), 
           stat = "identity", alpha = 0.7)+
  scale_fill_manual(values = c("blue", "red"))+
  # geom_line(data = phls_and_opp,
  #              aes(x = Gm, y = (phi_win_pct-50)/100)) +
  geom_text(aes(x = max(lad_and_opp$Gm) - 10, y = 0.25, label = "Opponent above .500"),
            hjust = 1, vjust = 1, color = "black", size = 4) +
  geom_text(aes(x = max(lad_and_opp$Gm) - 10, y = -0.25, label = "Opponent below .500"),
            hjust = 1, vjust = 0, color = "black", size = 4) +
  labs(title = "The 2024 Dodgers Are Holding Their Own Against Winning Teams\nBut they haven't played very many games against them.",
       subtitle = "Dodgers are .602 in 83 games vs sub-500 teams, .514 in 37 games vs teams over .500",
       x = "Game Number",
       y = "Opp. Win Pct Relative to .500",  
       fill = "Outcome",
       caption = "Data - Baseball Reference, 8/16/24\nData exclude results vs. teams with 0-0 record")+
  theme_minimal()

phi_2023_and_opp %>% 
  mutate(opp_over_500 = ifelse(Opp_PCt_under_500 > 0, "Over 500", "Under 500")) %>%
  group_by(opp_over_500, W_L) %>% 
  tally() %>%
  pivot_wider(
    names_from = W_L,
    values_from = n,
    names_prefix = ""
  ) %>%
  mutate(
    W = replace_na(W, 0),
    L = replace_na(L, 0),
    Pct = W/(W+L),
    Total_Games = W+L)
