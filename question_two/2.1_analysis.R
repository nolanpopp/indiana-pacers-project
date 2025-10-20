library(ggplot2)
library(tidyverse)
library(readr)

# Load the data
league_avg <- read.csv("C:/Users/nolan/OneDrive/Documents/Indiana Pacers Project/question_two/league_avg_four_factors.csv")
pacers_avg <- read.csv("C:/Users/nolan/OneDrive/Documents/Indiana Pacers Project/question_two/pacers_results_four_factors.csv")

# Convert game_date to Date format
pacers_avg$game_date <- as.Date(pacers_avg$game_date, format="%m/%d/%Y")

# Sort by date
pacers_avg <- pacers_avg %>% arrange(game_date)

# Define colors
pacers_color <- "#002d62"  # Pacers blue
opponent_color <- "#C8102E"  # Red

# ============================================
# PLOT 1: eFG%
# ============================================
plot_data_efg <- pacers_avg %>%
  select(game_date, win, pacers = efg_pct, opponent = opp_efg_pct) %>%
  pivot_longer(cols = c(pacers, opponent), 
               names_to = "team", 
               values_to = "value") %>%
  mutate(win_label = factor(ifelse(win, "Wins", "Losses"), 
                            levels = c("Wins", "Losses")))

p1 <- ggplot(plot_data_efg, aes(x = game_date, y = value, 
                                color = team,
                                group = team)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(aes(yintercept = league_avg$efg_pct, linetype = "League Avg"), 
             color = "black", linewidth = 0.8) +
  facet_wrap(~win_label, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("pacers" = pacers_color,
                                "opponent" = opponent_color),
                     labels = c("Pacers", "Opponent"),
                     breaks = c("pacers", "opponent"),
                     name = "") +
  scale_linetype_manual(name = "", values = c("League Avg" = "dashed")) +
  labs(title = "Effective Field Goal % (eFG%)",
       subtitle = "2022-2023 Season",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p1)

# ============================================
# PLOT 2: OREB%
# ============================================
plot_data_oreb <- pacers_avg %>%
  select(game_date, win, pacers = oreb_pct, opponent = opp_oreb_pct) %>%
  pivot_longer(cols = c(pacers, opponent), 
               names_to = "team", 
               values_to = "value") %>%
  mutate(win_label = factor(ifelse(win, "Wins", "Losses"), 
                            levels = c("Wins", "Losses")))

p2 <- ggplot(plot_data_oreb, aes(x = game_date, y = value, 
                                 color = team,
                                 group = team)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(aes(yintercept = league_avg$oreb_pct, linetype = "League Avg"), 
             color = "black", linewidth = 0.8) +
  facet_wrap(~win_label, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("pacers" = pacers_color,
                                "opponent" = opponent_color),
                     labels = c("Pacers", "Opponent"),
                     breaks = c("pacers", "opponent"),
                     name = "") +
  scale_linetype_manual(name = "", values = c("League Avg" = "dashed")) +
  labs(title = "Offensive Rebound % (OREB%)",
       subtitle = "2022-2023 Season",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p2)

# ============================================
# PLOT 3: TOV%
# ============================================
plot_data_tov <- pacers_avg %>%
  select(game_date, win, pacers = tov_pct, opponent = opp_tov_pct) %>%
  pivot_longer(cols = c(pacers, opponent), 
               names_to = "team", 
               values_to = "value") %>%
  mutate(win_label = factor(ifelse(win, "Wins", "Losses"), 
                            levels = c("Wins", "Losses")))

p3 <- ggplot(plot_data_tov, aes(x = game_date, y = value, 
                                color = team,
                                group = team)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(aes(yintercept = league_avg$tov_pct, linetype = "League Avg"), 
             color = "black", linewidth = 0.8) +
  facet_wrap(~win_label, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("pacers" = pacers_color,
                                "opponent" = opponent_color),
                     labels = c("Pacers", "Opponent"),
                     breaks = c("pacers", "opponent"),
                     name = "") +
  scale_linetype_manual(name = "", values = c("League Avg" = "dashed")) +
  labs(title = "Turnover % (TOV%)",
       subtitle = "2022-2023 Season",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p3)

# ============================================
# PLOT 4: FTM Rate
# ============================================
plot_data_ftm <- pacers_avg %>%
  select(game_date, win, pacers = ftm_rate, opponent = opp_ftm_rate) %>%
  pivot_longer(cols = c(pacers, opponent), 
               names_to = "team", 
               values_to = "value") %>%
  mutate(win_label = factor(ifelse(win, "Wins", "Losses"), 
                            levels = c("Wins", "Losses")))

p4 <- ggplot(plot_data_ftm, aes(x = game_date, y = value, 
                                color = team,
                                group = team)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(aes(yintercept = league_avg$ftm_rate, linetype = "League Avg"), 
             color = "black", linewidth = 0.8) +
  facet_wrap(~win_label, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("pacers" = pacers_color,
                                "opponent" = opponent_color),
                     labels = c("Pacers", "Opponent"),
                     breaks = c("pacers", "opponent"),
                     name = "") +
  scale_linetype_manual(name = "", values = c("League Avg" = "dashed")) +
  labs(title = "Free Throw Rate (FTM Rate)",
       subtitle = "2022-2023 Season",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(1, "lines"))

print(p4)




# Print summary statistics
summary_stats <- pacers_avg %>%
  group_by(win) %>%
  summarise(
    games = n(),
    avg_efg = mean(efg_pct, na.rm = TRUE),
    opp_avg_efg = mean(opp_efg_pct, na.rm = TRUE),
    avg_oreb = mean(oreb_pct, na.rm = TRUE),
    opp_avg_oreb = mean(opp_oreb_pct, na.rm = TRUE),
    avg_tov = mean(tov_pct, na.rm = TRUE),
    opp_avg_tov = mean(opp_tov_pct, na.rm = TRUE),
    avg_ftm = mean(ftm_rate, na.rm = TRUE),
    opp_avg_ftm = mean(opp_ftm_rate, na.rm = TRUE)
  )

print(summary_stats)

# Differentials 
summary_diff <- pacers_avg %>%
  group_by(win) %>%
  summarise(
    games = n(),
    efg_diff = mean(efg_pct, na.rm = TRUE) - mean(opp_efg_pct, na.rm = TRUE),
    oreb_diff = mean(oreb_pct, na.rm = TRUE) - mean(opp_oreb_pct, na.rm = TRUE),
    tov_diff = mean(tov_pct, na.rm = TRUE) - mean(opp_tov_pct, na.rm = TRUE),
    ftm_diff = mean(ftm_rate, na.rm = TRUE) - mean(opp_ftm_rate, na.rm = TRUE)
  )

print(summary_diff)


