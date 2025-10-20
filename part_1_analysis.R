library(hoopR)
library(httr)
library(tidyverse)
library(gt)
library(gtExtras)

# ==== PART 1: GET PACERS DATA ====


# Establish the current season (2024-2025)
current_season <- 2025

# Load NBA team box scores for season
nba_team_box <- load_nba_team_box(seasons = current_season)

# Remove the NBA All-star game data
nba_team_box <- nba_team_box |>
  filter(game_date != '2025-02-16')

# Convert columns to numeric for analysis
nba_team_box <- nba_team_box |>
  mutate(
    points_in_paint   = parse_number(points_in_paint),
    fast_break_points = parse_number(fast_break_points),
    turnover_points = parse_number(turnover_points)
  )

# Add opponent_offensive_rebounds and opponent_defensive_rebounds to the data frame
# Self join: match every row (team) to its opponent (same game, opposite team)
nba_team_box <- nba_team_box |>
  left_join(
    nba_team_box |>
      select(
        game_id,
        opponent_team_id = team_id,
        opponent_defensive_rebounds = defensive_rebounds,
        opponent_offensive_rebounds = offensive_rebounds
      ),
    by = c("game_id", "opponent_team_id")
  )

# Create Four Factors Columns 
nba_team_box <- nba_team_box |>
  mutate(
    # Effective Field Goal %
    efg_pct = (field_goals_made + 0.5 * three_point_field_goals_made) / 
      pmax(field_goals_attempted, 1),
    
    # Turnover Percentage
    tov_pct = turnovers / 
      pmax(field_goals_attempted + 0.44 * free_throws_attempted + turnovers, 1),
    
    # Offensive Rebound %
    # ORB% = ORB / (ORB + Opponent DRB)
    orb_pct = offensive_rebounds / 
      pmax(offensive_rebounds + opponent_defensive_rebounds, 1),
    
    # Defensive Rebound %
    # DRB% = DRB / (DRB + Opponent ORB)
    drb_pct = defensive_rebounds / 
      pmax(defensive_rebounds + opponent_offensive_rebounds, 1),
    
    # Free Throw Rate (getting to the line)
    #ft_rate = free_throws_attempted / pmax(field_goals_attempted, 1),
    
    # Free Throw %
    ft_pct = free_throws_made / pmax(free_throws_attempted, 1)
  )


# Calculate league wide season averages
all_team_averages <- nba_team_box |>
  filter(team_display_name != 'Indiana Pacers') |>
  summarise(
    games_played = n(),
    avg_points = round(mean(team_score, na.rm = TRUE), 2),
    avg_opp_points = round(mean(opponent_team_score, na.rm = TRUE), 2),
    avg_fg_pct = round(mean(field_goal_pct, na.rm = TRUE), 2),
    avg_three_pct = round(mean(three_point_field_goal_pct, na.rm = TRUE), 2),
    avg_assists = round(mean(assists, na.rm = TRUE), 2),
    avg_rebounds = round(mean(total_rebounds, na.rm = TRUE), 2),
    avg_turnovers = round(mean(turnovers, na.rm = TRUE), 2),
    
    avg_efg_pct = round(mean(efg_pct, na.rm = TRUE), 3),
    avg_tov_pct = round(mean(tov_pct, na.rm = TRUE), 3),
    avg_orb_pct = round(mean(orb_pct, na.rm = TRUE), 3),
    avg_drb_pct = round(mean(drb_pct, na.rm = TRUE), 3),
    #avg_ft_rate = round(mean(ft_rate, na.rm = TRUE), 3),
    avg_ft_pct = round(mean(ft_pct, na.rm = TRUE), 3)
  )

print("Rest of the League Season Averages:")
print(all_team_averages)


# Get Pacers specific data
pacers_data <- nba_team_box |>
  filter(team_display_name == "Indiana Pacers",
  team_short_display_name == "Pacers")

# Calculate Pacers season averages
pacers_averages <- pacers_data |>
  summarise(
    games_played = n(),
    avg_points = round(mean(team_score, na.rm = TRUE), 2),
    avg_opp_points = round(mean(opponent_team_score, na.rm = TRUE), 2),
    avg_fg_pct = round(mean(field_goal_pct, na.rm = TRUE), 2),
    avg_three_pct = round(mean(three_point_field_goal_pct, na.rm = TRUE), 2),
    avg_assists = round(mean(assists, na.rm = TRUE), 2),
    avg_rebounds = round(mean(total_rebounds, na.rm = TRUE), 2),
    avg_turnovers = round(mean(turnovers, na.rm = TRUE), 2),
    
    avg_efg_pct = round(mean(efg_pct, na.rm = TRUE), 3),
    avg_tov_pct = round(mean(tov_pct, na.rm = TRUE), 3),
    avg_orb_pct = round(mean(orb_pct, na.rm = TRUE), 3),
    avg_drb_pct = round(mean(drb_pct, na.rm = TRUE), 3),
    #avg_ft_rate = round(mean(ft_rate, na.rm = TRUE), 3),
    avg_ft_pct = round(mean(ft_pct, na.rm = TRUE), 3)
  )

print("Pacers Season Averages:")
print(pacers_averages)


colnames(nba_team_box)






# ==== PART 2: GET PACERS PLAYER DATA ====

# Per-game/team lookup for opponent rebounds from NBA team box
opp_reb_lookup <- nba_team_box |>
  select(
    game_id,
    team_id,
    opponent_defensive_rebounds,
    opponent_offensive_rebounds
  )


# Load player box scores for the season and attach the opp_reb_lookup to each player game row
nba_player_box <- load_nba_player_box(seasons = current_season) |>
  filter(game_date != '2025-02-16') |>
  left_join(opp_reb_lookup, by = c("game_id", "team_id"))


# Get Pacers roster data
pacers_players <- nba_player_box |>
  filter(team_display_name == "Indiana Pacers") |>
  group_by(athlete_id, athlete_display_name, athlete_position_abbreviation) |>
  summarise(
    games = n(),
    mpg   = mean(minutes, na.rm = TRUE),
    
    # Box score per-game means
    ppg   = mean(points, na.rm = TRUE),
    rpg   = mean(rebounds, na.rm = TRUE),
    apg   = mean(assists, na.rm = TRUE),
    stl   = mean(steals,  na.rm = TRUE),
    blk   = mean(blocks,  na.rm = TRUE),
    tov   = mean(turnovers, na.rm = TRUE),
    
    # Shooting efficiencies
    fg_pct    = sum(field_goals_made, na.rm = TRUE) / pmax(sum(field_goals_attempted, na.rm = TRUE), 1),
    three_pct = sum(three_point_field_goals_made, na.rm = TRUE) / 
      pmax(sum(three_point_field_goals_attempted, na.rm = TRUE), 1),
    ft_pct    = sum(free_throws_made, na.rm = TRUE) / pmax(sum(free_throws_attempted,  na.rm = TRUE), 1),
    ts_pct    = sum(points, na.rm = TRUE) /
      pmax(2 * (sum(field_goals_attempted, na.rm = TRUE) +
                  0.44 * sum(free_throws_attempted, na.rm = TRUE)), 1),
    
    # Four Factors
    efg_pct = (sum(field_goals_made, na.rm = TRUE) + 0.5 * sum(three_point_field_goals_made, na.rm = TRUE)) /
      pmax(sum(field_goals_attempted, na.rm = TRUE), 1),
    
    tov_pct = sum(turnovers, na.rm = TRUE) /
      pmax(sum(field_goals_attempted, na.rm = TRUE) +
             0.44 * sum(free_throws_attempted, na.rm = TRUE) +
             sum(turnovers, na.rm = TRUE), 1),
    
    # Rebounds
    # ORB% = player_OR / (player_OR + opponent_team_DREB in those games)
    # DRB% = player_DR / (player_DR + opponent_team_OREB in those games)
    # We sum per player across their games to keep the exact same structure.
    player_or = sum(offensive_rebounds, na.rm = TRUE),
    player_dr = sum(defensive_rebounds, na.rm = TRUE),
    opp_dreb_total = sum(opponent_defensive_rebounds, na.rm = TRUE),
    opp_oreb_total = sum(opponent_offensive_rebounds, na.rm = TRUE),
    
    orb_pct = player_or / pmax(player_or + opp_dreb_total, 1),
    drb_pct = player_dr / pmax(player_dr + opp_oreb_total, 1),
    
    .groups = "drop"
  ) |>
  # Keep regular rotation only (players playing more the 15 games)
  filter(games >= 15) |>
  arrange(desc(mpg)) |>
  # Get rid of helper columns for a cleaner approach
  select(-player_or, -player_dr, -opp_dreb_total, -opp_oreb_total)

print("Top Pacers Players by Minutes (with Four Factors):")
print(head(pacers_players, 10))









# ==== PART 3: GET LEAGUE WIDE PLAYER DATA ====


# Get all NBA players data
all_nba_players <- nba_player_box |>
  group_by(athlete_id, athlete_display_name, team_short_display_name,
           athlete_position_abbreviation) |>
  summarise(
    games = n(),
    mpg = mean(minutes, na.rm = TRUE),
    ppg = mean(points, na.rm = TRUE),
    rpg = mean(rebounds, na.rm = TRUE),
    apg = mean(assists, na.rm = TRUE),
    spg = mean(steals, na.rm = TRUE),
    bpg = mean(blocks, na.rm = TRUE),
    topg = mean(turnovers, na.rm = TRUE),
    fg_pct = sum(field_goals_made, na.rm = TRUE) / sum(field_goals_attempted, na.rm = TRUE),
    three_pct = sum(three_point_field_goals_made, na.rm = TRUE) / 
      sum(three_point_field_goals_attempted, na.rm = TRUE),
    three_pa = sum(three_point_field_goals_attempted, na.rm = TRUE) / n(),
    ft_pct = sum(free_throws_made, na.rm = TRUE) / sum(free_throws_attempted, na.rm = TRUE),
    ts_pct = sum(points, na.rm = TRUE) / 
      (2 * (sum(field_goals_attempted, na.rm = TRUE) + 
              0.44 * sum(free_throws_attempted, na.rm = TRUE))),
    total_fga = sum(field_goals_attempted, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(
    games >= 15,
    mpg >= 15,
    team_short_display_name != "IND"
  )
  
print(paste("Total qualifying NBA players:", nrow(all_nba_players)))









# ==== PART 4: IDENTIFY PACERS STRENGTHS & WEAKNESSES ====


# Compare Pacers to league averages
pacers_vs_league <- pacers_averages |>
  mutate(
    league_avg_points      = all_team_averages$avg_points,
    league_avg_opp_points  = all_team_averages$avg_opp_points,
    league_avg_fg_pct      = all_team_averages$avg_fg_pct,
    league_avg_three_pct   = all_team_averages$avg_three_pct,
    league_avg_assists     = all_team_averages$avg_assists,
    league_avg_rebounds    = all_team_averages$avg_rebounds,
    league_avg_turnovers   = all_team_averages$avg_turnovers,
    
    league_avg_efg_pct     = all_team_averages$avg_efg_pct,
    league_avg_tov_pct     = all_team_averages$avg_tov_pct,
    league_avg_orb_pct     = all_team_averages$avg_orb_pct,
    league_avg_drb_pct     = all_team_averages$avg_drb_pct,
    league_avg_ft_pct      = all_team_averages$avg_ft_pct
  ) |>
  mutate(
    points_diff     = avg_points    - league_avg_points,
    opp_points_diff = avg_opp_points - league_avg_opp_points,  # negative = allowing fewer than league
    fg_pct_diff     = avg_fg_pct    - league_avg_fg_pct,
    three_pct_diff  = avg_three_pct - league_avg_three_pct,
    assists_diff    = avg_assists   - league_avg_assists,
    rebounds_diff   = avg_rebounds  - league_avg_rebounds,
    turnovers_diff  = avg_turnovers - league_avg_turnovers,    # negative = allowing fewer turnovers than the league
    
    efg_pct_diff    = avg_efg_pct   - league_avg_efg_pct,
    tov_pct_diff    = avg_tov_pct   - league_avg_tov_pct,       # negative = better ball security
    orb_pct_diff    = avg_orb_pct   - league_avg_orb_pct,
    drb_pct_diff    = avg_drb_pct   - league_avg_drb_pct,
    ft_pct_diff     = avg_ft_pct    - league_avg_ft_pct
  )



print("Pacers vs League Averages:")
print(pacers_vs_league %>% select(ends_with("_diff")))



# --- visualizing the Pacers Strengths and Weaknesses ---

# Create a clean comparison dataset
comparison_data <- tibble(
  metric = c("Points", "Opp Points", "FG%", "3PT%", "Assists", "Rebounds", 
             "Turnovers", "eFG%", "TOV%", "ORB%", "DRB%", "FT%"),
  
  # Extract Pacers values
  pacers = c(
    pacers_vs_league$avg_points,
    pacers_vs_league$avg_opp_points,
    pacers_vs_league$avg_fg_pct,
    pacers_vs_league$avg_three_pct,
    pacers_vs_league$avg_assists,
    pacers_vs_league$avg_rebounds,
    pacers_vs_league$avg_turnovers,
    pacers_vs_league$avg_efg_pct,
    pacers_vs_league$avg_tov_pct,
    pacers_vs_league$avg_orb_pct,
    pacers_vs_league$avg_drb_pct,
    pacers_vs_league$avg_ft_pct
  ),
  
  # Extract League averages
  league = c(
    pacers_vs_league$league_avg_points,
    pacers_vs_league$league_avg_opp_points,
    pacers_vs_league$league_avg_fg_pct,
    pacers_vs_league$league_avg_three_pct,
    pacers_vs_league$league_avg_assists,
    pacers_vs_league$league_avg_rebounds,
    pacers_vs_league$league_avg_turnovers,
    pacers_vs_league$league_avg_efg_pct,
    pacers_vs_league$league_avg_tov_pct,
    pacers_vs_league$league_avg_orb_pct,
    pacers_vs_league$league_avg_drb_pct,
    pacers_vs_league$league_avg_ft_pct
  ),
  
  # Extract Differences
  difference = c(
    pacers_vs_league$points_diff,
    pacers_vs_league$opp_points_diff,
    pacers_vs_league$fg_pct_diff,
    pacers_vs_league$three_pct_diff,
    pacers_vs_league$assists_diff,
    pacers_vs_league$rebounds_diff,
    pacers_vs_league$turnovers_diff,
    pacers_vs_league$efg_pct_diff,
    pacers_vs_league$tov_pct_diff,
    pacers_vs_league$orb_pct_diff,
    pacers_vs_league$drb_pct_diff,
    pacers_vs_league$ft_pct_diff
  )
) |>
  mutate(
    # Identify percentage metrics
    is_percentage = metric %in% c("FG%", "3PT%", "eFG%", "TOV%", "ORB%", "DRB%", "FT%"),
    
    # Determine if lower is better
    lower_is_better = metric %in% c("Opp Points", "Turnovers", "TOV%"),
    
    # Calculate percentage difference
    pct_difference = (difference / league) * 100,
    
    # Calculate if strength or weakness
    is_strength = case_when(
      lower_is_better & difference < 0 ~ TRUE,
      !lower_is_better & difference > 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Absolute percentage difference for ranking
    abs_pct_diff = abs(pct_difference)
  )



# For strengths: need to account for "lower is better" metrics
strengths_ranked <- comparison_data |>
  mutate(
    # For lower_is_better metrics, flip the sign for ranking
    strength_score = if_else(lower_is_better, -pct_difference, pct_difference)
  ) |>
  arrange(desc(strength_score))

# Top strengths
top_strengths <- strengths_ranked |>
  head(3) |>
  mutate(category = "STRENGTH")

# Top weaknesses
bottom_weaknesses <- strengths_ranked |>
  tail(3) |>
  mutate(category = "WEAKNESS")

# Combine for visualization
focus_metrics <- bind_rows(top_strengths, bottom_weaknesses) |>
  mutate(
    metric = fct_reorder(metric, pct_difference)
  )

# Create custom ordering: Strengths first (ranked), then Weaknesses (ranked)
focus_metrics_ordered <- focus_metrics |>
  arrange(desc(strength_score)) |>
  mutate(
    plot_order = row_number(),
    metric_label = paste0(metric, "\n",
                          if_else(pct_difference > 0, "+", ""),
                          round(pct_difference, 1), "%")
  )

# Create the ordering factor
focus_metrics_ordered <- focus_metrics_ordered |>
  mutate(
    metric_label = factor(metric_label, levels = unique(metric_label))
  )

# Prepare data for side-by-side bars
plot_data <- focus_metrics_ordered |>
  select(metric, metric_label, pacers, league, category, is_percentage, plot_order) |>
  pivot_longer(cols = c(pacers, league),
               names_to = "team",
               values_to = "value") |>
  mutate(
    team_label = if_else(team == "pacers", "Pacers", "Rest of League"),
    team_label = factor(team_label, levels = c("Pacers", "Rest of League"))
  )


# Create the plot
strengths_weaknesses_plot <- ggplot(plot_data, 
                                    aes(x = team_label, y = value, fill = team_label)) +
  geom_col(width = 0.7, alpha = 0.9) +
  # Add value labels
  geom_text(aes(label = if_else(is_percentage,
                                paste0(round(value * 100, 1), "%"),
                                as.character(round(value, 1)))),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  # Facet by metric with custom order
  facet_wrap(~metric_label, scales = "free_y", ncol = 3) +
  # Color scheme
  scale_fill_manual(values = c("Pacers" = "#FFD520",      # Pacers navy
                               "Rest of League Avg" = "#EEEEEE")) + # Pacers gold
  labs(
    title = "Indiana Pacers: Top 3 Strengths & Bottom 3 Weaknesses vs Rest of League Average",
    subtitle = "2024-25 Season | Percentage shown = difference from league average",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: hoopR NBA Data | First row = Strengths, Second row = Weaknesses"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    strip.text = element_text(face = "bold", size = 9, lineheight = 1.2),
    strip.background = element_rect(fill = NA, color = NA),
    strip.placement = "outside",
    strip.text.y = element_text(margin = margin(b = 2, t = 2)),
    legend.position = "top",
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9),
    panel.spacing = unit(1, "lines"),
  )

print(strengths_weaknesses_plot)






# ==== PART 5: FIND UNDERVALUED PLAYERS ====


# Create efficiency score
all_nba_players <- all_nba_players |>
  mutate(
    # PER-like metric (simplified)
    efficiency_score = (ppg + rpg + apg + spg + bpg - topg) / mpg * ts_pct * 100,
    
    # Value score: production relative to usage
    pts_per_fga = ifelse(total_fga > 0, total_points / total_fga, 0),
    
    # 3&D score for wings 
    three_and_d_score = ifelse(
      athlete_position_abbreviation %in% c('SF', 'SG', 'PF'),
      (three_pct * three_pa) + (spg + bpg) * 10,
      NA
    )
  )

# Find undervalued players (high efficiency, reasonable minutes)
undervalued_targets <- all_nba_players |>
  filter(
    mpg >= 15,
    mpg <= 28,  
    efficiency_score > quantile(efficiency_score, 0.70, na.rm = TRUE),
    ts_pct > 0.55
  ) |>
  arrange(desc(efficiency_score)) |>
  select(athlete_display_name, team_short_display_name, 
         athlete_position_abbreviation, mpg, ppg, rpg, apg, 
         ts_pct, efficiency_score)

print("Top 10 Undervalued Players:")
print(head(undervalued_targets, 10))









# ==== PART 6: POSITION SPECIFIC TARGETS ====


# Find 3&D wing targets (addresses common Pacers need)
three_and_d_wings <- all_nba_players |>
  filter(
    athlete_position_abbreviation %in% c("SF", "SG", "PF"),
    mpg >= 20,
    three_pct >= 0.36,  # League average ~36%
    three_pa >= 3,       # Takes 3+ threes per game
    spg >= 0.8,          # Defensive activity
    !is.na(three_and_d_score)
  ) |>
  arrange(desc(three_and_d_score)) |>
  select(athlete_display_name, team_short_display_name, 
         athlete_position_abbreviation, mpg, ppg, three_pct, 
         three_pa, spg, bpg, three_and_d_score)

print("Top 3&D Wing Targets:")
print(head(three_and_d_wings, 15))









# ==== PART 7: VISUALIZATIONS ====


# Scatter plot: Efficiency vs Minutes
efficiency_plot <- ggplot(
  undervalued_targets %>% head(20),
  aes(x = mpg, y = efficiency_score, 
      color = ts_pct, size = ppg)
) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = athlete_display_name), 
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_color_gradient(low = "red", high = "green") +
  theme_minimal() +
  labs(
    title = "Undervalued Players: High Efficiency, Moderate Usage",
    subtitle = "Size = PPG, Color = True Shooting %",
    x = "Minutes Per Game",
    y = "Efficiency Score",
    color = "TS%",
    size = "PPG"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(efficiency_plot)

# 3&D visualization
three_d_plot <- ggplot(
  three_and_d_wings %>% head(20),
  aes(x = three_pct, y = spg, 
      size = three_pa, color = mpg)
) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = athlete_display_name), 
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(
    title = "3&D Wing Targets",
    subtitle = "Size = 3PA per game, Color = Minutes",
    x = "Three Point %",
    y = "Steals Per Game",
    size = "3PA",
    color = "MPG"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(three_d_plot)









# Top 5 recommendations table
top_recommendations <- undervalued_targets %>%
  head(5) %>%
  select(Player = athlete_display_name, 
         Team = team_short_display_name,
         Pos = athlete_position_abbreviation,
         MPG = mpg, PPG = ppg, RPG = rpg, APG = apg,
         `TS%` = ts_pct, `Eff Score` = efficiency_score) %>%
  mutate(
    MPG = round(MPG, 1),
    PPG = round(PPG, 1),
    RPG = round(RPG, 1),
    APG = round(APG, 1),
    `TS%` = round(`TS%`, 3),
    `Eff Score` = round(`Eff Score`, 2)
  )

# Create nice table with gt
recommendations_table <- top_recommendations %>%
  gt() %>%
  tab_header(
    title = "Top Trade Target Recommendations",
    subtitle = "Based on Efficiency & Value Analysis"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(recommendations_table)














# Make column for DREB: total rebounds - OREB = DREB?



# Broken code
ind_logs <- nba_teamgamelogs(season = season, team_id = TRUE)

ind_logs <- nba_teamgamelogs(season = season) |>
  filter(teamSlug == "pacers") |>
  hoopr_clean_names()
















# --- Optional: OREB % and DREB% --- 
paired <- nba_team_box %>%
  select(game_id, team_slug, offensive_rebounds, defensive_rebounds) %>%
  inner_join(
    nba_team_box %>%
      select(game_id,
             opp_offensive_rebounds = offensive_rebounds,
             opp_defensive_rebounds = defensive_rebounds,
             opp_team_slug = team_slug),
    by = "game_id"
  ) %>%
  filter(team_slug != opp_team_slug)

pacers_reb_rates <- paired %>%
  filter(team_slug == "indiana-pacers") %>%
  mutate(
    orb_pct_game  = offensive_rebounds /
      pmax(offensive_rebounds + opp_defensive_rebounds, 1),
    dreb_pct_game = defensive_rebounds /
      pmax(defensive_rebounds + opp_offensive_rebounds, 1)
  ) %>%
  summarise(
    gp                 = n(),
    orb_pct_season_avg = mean(orb_pct_game,  na.rm = TRUE),
    dreb_pct_season_avg= mean(dreb_pct_game, na.rm = TRUE)
  )


league_reb_rates <- paired %>%
  filter(team_slug != "indiana-pacers") %>%
  mutate(
    orb_pct_game  = offensive_rebounds /
      pmax(offensive_rebounds + opp_defensive_rebounds, 1),
    dreb_pct_game = defensive_rebounds /
      pmax(defensive_rebounds + opp_offensive_rebounds, 1)
  ) %>%
  summarise(
    gp                 = n(),
    orb_pct_season_avg = mean(orb_pct_game,  na.rm = TRUE),
    dreb_pct_season_avg= mean(dreb_pct_game, na.rm = TRUE)
  )


print(pacers_reb_rates)
print(league_reb_rates)










# Pacers roster with Four Factors-style metrics
all_nba_players <- nba_player_box |>
  group_by(athlete_id, athlete_display_name, athlete_position_abbreviation) |>
  summarise(
    games = n(),
    mpg   = mean(minutes, na.rm = TRUE),
    
    # Box score per-game means
    ppg   = mean(points, na.rm = TRUE),
    rpg   = mean(rebounds, na.rm = TRUE),
    apg   = mean(assists, na.rm = TRUE),
    stl   = mean(steals,  na.rm = TRUE),
    blk   = mean(blocks,  na.rm = TRUE),
    tov   = mean(turnovers, na.rm = TRUE),
    
    # Shooting efficiencies (season aggregates -> season rate)
    fg_pct    = sum(field_goals_made, na.rm = TRUE) / pmax(sum(field_goals_attempted, na.rm = TRUE), 1),
    three_pct = sum(three_point_field_goals_made, na.rm = TRUE) / 
      pmax(sum(three_point_field_goals_attempted, na.rm = TRUE), 1),
    ft_pct    = sum(free_throws_made, na.rm = TRUE) / pmax(sum(free_throws_attempted,  na.rm = TRUE), 1),
    ts_pct    = sum(points, na.rm = TRUE) /
      pmax(2 * (sum(field_goals_attempted, na.rm = TRUE) +
                  0.44 * sum(free_throws_attempted, na.rm = TRUE)), 1),
    
    # Four Factors (player analogs)
    efg_pct = (sum(field_goals_made, na.rm = TRUE) + 0.5 * sum(three_point_field_goals_made, na.rm = TRUE)) /
      pmax(sum(field_goals_attempted, na.rm = TRUE), 1),
    
    tov_pct = sum(turnovers, na.rm = TRUE) /
      pmax(sum(field_goals_attempted, na.rm = TRUE) +
             0.44 * sum(free_throws_attempted, na.rm = TRUE) +
             sum(turnovers, na.rm = TRUE), 1),
    
    # For rebound percentages, use the same denominators as team Four Factors:
    # ORB% = player_OR / (player_OR + opponent_team_DREB in those games)
    # DRB% = player_DR / (player_DR + opponent_team_OREB in those games)
    # We sum per player across their games to keep the exact same structure.
    player_or = sum(offensive_rebounds, na.rm = TRUE),
    player_dr = sum(defensive_rebounds, na.rm = TRUE),
    opp_dreb_total = sum(opponent_defensive_rebounds, na.rm = TRUE),
    opp_oreb_total = sum(opponent_offensive_rebounds, na.rm = TRUE),
    
    orb_pct = player_or / pmax(player_or + opp_dreb_total, 1),
    drb_pct = player_dr / pmax(player_dr + opp_oreb_total, 1),
    
    .groups = "drop"
  ) |>
  # keep regular rotation only (adjust as you like)
  filter(games >= 15) |>
  arrange(desc(mpg)) |>
  # tidy up helper columns if you don't want them in the final table
  select(-player_or, -player_dr, -opp_dreb_total, -opp_oreb_total)

print("Top Players by Minutes (with Four Factors):")
print(head(all_nba_players, 10))











# Create the plot
strengths_weaknesses_plot <- ggplot(plot_data, 
                                    aes(x = team_label, y = value, fill = team_label)) +
  geom_col(width = 0.7, alpha = 0.9) +
  # Add value labels
  geom_text(aes(label = if_else(is_percentage,
                                paste0(round(value * 100, 1), "%"),
                                as.character(round(value, 1)))),
            vjust = -0.5, fontface = "plain", size = 3.5) +
  # Facet by metric with custom order
  facet_wrap(~metric_label, scales = "free_y", ncol = 3) +
  # Color scheme
  scale_fill_manual(values = c("Pacers" = "#FFD520",      # Pacers navy
                               "Rest of League Avg" = "#EEEEEE")) + # Pacers gold
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Indiana Pacers: Top 3 Strengths & Bottom 3 Weaknesses vs Rest of League Average",
    subtitle = "2024-25 Season | Percentage shown = difference from rest of league average",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: hoopR NBA Data | First row = Strengths, Second row = Weaknesses"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    strip.text = element_text(face = "bold", size = 11, lineheight = 1.1),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text.y = element_text(margin = margin(b = 2, t = 2)),
    legend.position = "top",
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9),
    panel.spacing = unit(1, "lines"),
  )

print(strengths_weaknesses_plot)













##### FINAL STRENGTHS AND WEAKNESSES VISUALIZATIONS #####
# --- Split data ---
plot_data_str <- plot_data |>
  filter(category == "STRENGTH") |>
  arrange(plot_order) |>
  mutate(metric = factor(metric, levels = unique(metric)))

plot_data_weak <- plot_data |>
  filter(category == "WEAKNESS") |>
  arrange(plot_order) |>
  mutate(metric_label = factor(metric_label, levels = unique(metric_label)))

# --- Strengths plot ---
strengths_plot <- ggplot(plot_data_str, aes(x = team_label, y = value, fill = team_label)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = if_else(is_percentage,
                        paste0(round(value * 100, 1), "%"),
                        as.character(round(value, 1)))),
    vjust = -0.5, fontface = "plain", size = 4
  ) +
  facet_wrap(~ metric, strip.position = "bottom", scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Pacers" = "#FFD520",
                               "Rest of League" = "#EEEEEE")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Top Strengths vs Rest of League",
    subtitle = "2024-25 Season Average",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, vjust = 5, color = "gray40"),
    strip.text = element_text(face = "bold", size = 11, lineheight = 1.1),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text.y = element_text(margin = margin(b = 2, t = 2)),
    legend.position = "top",
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

print(strengths_plot)

# --- Weaknesses plot ---
weaknesses_plot <- ggplot(plot_data_weak, aes(x = team_label, y = value, fill = team_label)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = if_else(is_percentage,
                        paste0(round(value * 100, 1), "%"),
                        as.character(round(value, 1)))),
    vjust = -0.5, fontface = "plain", size = 3.5
  ) +
  facet_wrap(~ metric_label, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Pacers" = "#FFD520",
                               "Rest of League Avg" = "#EEEEEE")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Indiana Pacers: Bottom 3 Weaknesses vs Rest of League Avg",
    subtitle = "2024-25 Season | Percentage shown = difference from rest of league average",
    x = NULL, y = NULL, fill = NULL,
    caption = "Source: hoopR NBA Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    strip.text = element_text(face = "bold", size = 11, lineheight = 1.1),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text.y = element_text(margin = margin(b = 2, t = 2)),
    legend.position = "top",
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

# --- Show plots ---
print(strengths_plot)
print(weaknesses_plot)



# --- Weakness plot NEW ---
weakness_plot <- ggplot(plot_data_weak, aes(x = team_label, y = value, fill = team_label)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = if_else(is_percentage,
                        paste0(round(value * 100, 1), "%"),
                        as.character(round(value, 1)))),
    vjust = -0.5, fontface = "plain", size = 4
  ) +
  facet_wrap(~ metric, strip.position = "bottom", scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Pacers" = "#FFD520",
                               "Rest of League" = "#EEEEEE")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Top Weaknesses vs Rest of League",
    subtitle = "2024-25 Season Average",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, vjust = 5, color = "gray40"),
    strip.text = element_text(face = "bold", size = 11, lineheight = 1.1),
    strip.background = element_rect(fill = NA, color = NA),
    strip.text.y = element_text(margin = margin(b = 2, t = 2)),
    legend.position = "top",
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

print(weakness_plot)













pacers_data <- pacers_data %>%
  mutate(
    points_in_paint   = parse_number(points_in_paint),
    fast_break_points = parse_number(fast_break_points),
    turnover_points = parse_number(turnover_points)
  )


nba_team_box <- nba_team_box %>%
  mutate(
    points_in_paint   = parse_number(points_in_paint),
    fast_break_points = parse_number(fast_break_points),
    turnover_points = parse_number(turnover_points)
  )


#### Testing with more stats for LEAGUE VS PACERS ####

# Calculate league wide season averages
all_team_averages_test <- nba_team_box |>
  filter(team_display_name != 'Indiana Pacers') |>
  summarise(
    games_played = n(),
    avg_points = round(mean(team_score, na.rm = TRUE), 2),
    avg_opp_points = round(mean(opponent_team_score, na.rm = TRUE), 2),
    avg_fg_pct = round(mean(field_goal_pct, na.rm = TRUE), 2),
    avg_three_pct = round(mean(three_point_field_goal_pct, na.rm = TRUE), 2),
    avg_assists = round(mean(assists, na.rm = TRUE), 2),
    avg_rebounds = round(mean(total_rebounds, na.rm = TRUE), 2),
    avg_turnovers = round(mean(turnovers, na.rm = TRUE), 2),
    
    avg_efg_pct = round(mean(efg_pct, na.rm = TRUE), 3),
    avg_tov_pct = round(mean(tov_pct, na.rm = TRUE), 3),
    avg_orb_pct = round(mean(orb_pct, na.rm = TRUE), 3),
    avg_drb_pct = round(mean(drb_pct, na.rm = TRUE), 3),
    #avg_ft_rate = round(mean(ft_rate, na.rm = TRUE), 3),
    avg_ft_pct = round(mean(ft_pct, na.rm = TRUE), 3),
    
    
    avg_blocks                        = round(mean(blocks, na.rm = TRUE), 2),
    avg_defensive_rebounds            = round(mean(defensive_rebounds, na.rm = TRUE), 2),
    avg_offensive_rebounds            = round(mean(offensive_rebounds, na.rm = TRUE), 2),
    avg_points_in_paint               = round(mean(points_in_paint, na.rm = TRUE), 2),
    avg_steals                        = round(mean(steals, na.rm = TRUE), 2),
    avg_team_turnovers                = round(mean(team_turnovers, na.rm = TRUE), 2),
    avg_technical_fouls               = round(mean(technical_fouls, na.rm = TRUE), 2),
    avg_flagrant_fouls                = round(mean(flagrant_fouls, na.rm = TRUE), 2),
    avg_fouls                         = round(mean(fouls, na.rm = TRUE), 2),
    avg_fast_break_points             = round(mean(fast_break_points, na.rm = TRUE), 2),
    
    avg_field_goals_made              = round(mean(field_goals_made, na.rm = TRUE), 2),
    avg_field_goals_attempted         = round(mean(field_goals_attempted, na.rm = TRUE), 2),
    
    avg_three_point_field_goals_made  = round(mean(three_point_field_goals_made, na.rm = TRUE), 2),
    avg_three_point_field_goals_att   = round(mean(three_point_field_goals_attempted, na.rm = TRUE), 2),
    
    avg_free_throws_made              = round(mean(free_throws_made, na.rm = TRUE), 2),
    avg_free_throws_attempted         = round(mean(free_throws_attempted, na.rm = TRUE), 2),

    
    avg_total_technical_fouls         = round(mean(total_technical_fouls, na.rm = TRUE), 2),
    avg_total_turnovers               = round(mean(total_turnovers, na.rm = TRUE), 2),
    avg_turnover_points               = round(mean(turnover_points, na.rm = TRUE), 2)
  )

print("Rest of the League Season Averages:")
print(all_team_averages_test)







# Calculate Pacers season averages
pacers_averages_test <- pacers_data |>
  summarise(
    games_played = n(),
    avg_points = round(mean(team_score, na.rm = TRUE), 2),
    avg_opp_points = round(mean(opponent_team_score, na.rm = TRUE), 2),
    avg_fg_pct = round(mean(field_goal_pct, na.rm = TRUE), 2),
    avg_three_pct = round(mean(three_point_field_goal_pct, na.rm = TRUE), 2),
    avg_assists = round(mean(assists, na.rm = TRUE), 2),
    avg_rebounds = round(mean(total_rebounds, na.rm = TRUE), 2),
    avg_turnovers = round(mean(turnovers, na.rm = TRUE), 2),
    
    avg_efg_pct = round(mean(efg_pct, na.rm = TRUE), 3),
    avg_tov_pct = round(mean(tov_pct, na.rm = TRUE), 3),
    avg_orb_pct = round(mean(orb_pct, na.rm = TRUE), 3),
    avg_drb_pct = round(mean(drb_pct, na.rm = TRUE), 3),
    #avg_ft_rate = round(mean(ft_rate, na.rm = TRUE), 3),
    avg_ft_pct = round(mean(ft_pct, na.rm = TRUE), 3),
    
    
    avg_blocks                        = round(mean(blocks, na.rm = TRUE), 2),
    avg_defensive_rebounds            = round(mean(defensive_rebounds, na.rm = TRUE), 2),
    avg_offensive_rebounds            = round(mean(offensive_rebounds, na.rm = TRUE), 2),
    avg_points_in_paint               = round(mean(points_in_paint, na.rm = TRUE), 2),
    avg_steals                        = round(mean(steals, na.rm = TRUE), 2),
    avg_team_turnovers                = round(mean(team_turnovers, na.rm = TRUE), 2),
    avg_technical_fouls               = round(mean(technical_fouls, na.rm = TRUE), 2),
    avg_flagrant_fouls                = round(mean(flagrant_fouls, na.rm = TRUE), 2),
    avg_fouls                         = round(mean(fouls, na.rm = TRUE), 2),
    avg_fast_break_points             = round(mean(fast_break_points, na.rm = TRUE), 2),
    
    avg_field_goals_made              = round(mean(field_goals_made, na.rm = TRUE), 2),
    avg_field_goals_attempted         = round(mean(field_goals_attempted, na.rm = TRUE), 2),
    
    avg_three_point_field_goals_made  = round(mean(three_point_field_goals_made, na.rm = TRUE), 2),
    avg_three_point_field_goals_att   = round(mean(three_point_field_goals_attempted, na.rm = TRUE), 2),
    
    avg_free_throws_made              = round(mean(free_throws_made, na.rm = TRUE), 2),
    avg_free_throws_attempted         = round(mean(free_throws_attempted, na.rm = TRUE), 2),
    
    
    avg_total_technical_fouls         = round(mean(total_technical_fouls, na.rm = TRUE), 2),
    avg_total_turnovers               = round(mean(total_turnovers, na.rm = TRUE), 2),
    avg_turnover_points               = round(mean(turnover_points, na.rm = TRUE), 2)
  )













comparison_data_test <- tibble(
  metric = c(
    "Points","Opp Points","FG%","3PT%","Assists","Rebounds","Turnovers",
    "eFG%","TOV%","ORB%","DRB%","FT%",
    "Blocks","Steals","Def Reb","Off Reb","Total Reb",
    "Paint Pts",
    "Total Turnovers","Turnover Points"
  ),
  
  # Pacers values (from pacers_averages_test)
  pacers = c(
    pacers_averages_test$avg_points,
    pacers_averages_test$avg_opp_points,
    pacers_averages_test$avg_fg_pct,
    pacers_averages_test$avg_three_pct,
    pacers_averages_test$avg_assists,
    pacers_averages_test$avg_rebounds,
    pacers_averages_test$avg_turnovers,
    pacers_averages_test$avg_efg_pct,
    pacers_averages_test$avg_tov_pct,
    pacers_averages_test$avg_orb_pct,
    pacers_averages_test$avg_drb_pct,
    pacers_averages_test$avg_ft_pct,
    pacers_averages_test$avg_blocks,
    pacers_averages_test$avg_steals,
    pacers_averages_test$avg_defensive_rebounds,
    pacers_averages_test$avg_offensive_rebounds,
    pacers_averages_test$avg_rebounds,
    pacers_averages_test$avg_points_in_paint,
    pacers_averages_test$avg_total_turnovers,
    pacers_averages_test$avg_turnover_points
  ),
  
  # League values (from all_team_averages_test)
  league = c(
    all_team_averages_test$avg_points,
    all_team_averages_test$avg_opp_points,
    all_team_averages_test$avg_fg_pct,
    all_team_averages_test$avg_three_pct,
    all_team_averages_test$avg_assists,
    all_team_averages_test$avg_rebounds,
    all_team_averages_test$avg_turnovers,
    all_team_averages_test$avg_efg_pct,
    all_team_averages_test$avg_tov_pct,
    all_team_averages_test$avg_orb_pct,
    all_team_averages_test$avg_drb_pct,
    all_team_averages_test$avg_ft_pct,
    all_team_averages_test$avg_blocks,
    all_team_averages_test$avg_steals,
    all_team_averages_test$avg_defensive_rebounds,
    all_team_averages_test$avg_offensive_rebounds,
    all_team_averages_test$avg_rebounds,
    all_team_averages_test$avg_points_in_paint,
    all_team_averages_test$avg_total_turnovers,
    all_team_averages_test$avg_turnover_points
  )
) %>%
  mutate(
    difference = pacers - league,
    
    # mark percentage-ish metrics for formatting (your mix of 0–1 rates and 0–100 pcts)
    is_percentage = metric %in% c("FG%","3PT%","eFG%","TOV%","ORB%","DRB%","FT%"),
    
    # where "lower is better"
    lower_is_better = metric %in% c(
      "Opp Points","Turnovers","TOV%","Total Turnovers"
    ),
    
    # percent difference (against league). Guard against 0/NA.
    pct_difference = ifelse(is.finite(league) & abs(league) > 1e-9,
                            (difference / league) * 100, NA_real_),
    
    # classify as strength if the sign goes in the helpful direction
    is_strength = case_when(
      lower_is_better & difference < 0 ~ TRUE,
      !lower_is_better & difference > 0 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    abs_pct_diff = abs(pct_difference)
  )










# For strengths: need to account for "lower is better" metrics
strengths_ranked <- comparison_data_test |>
  mutate(
    # For lower_is_better metrics, flip the sign for ranking
    strength_score = if_else(lower_is_better, -pct_difference, pct_difference)
  ) |>
  arrange(desc(strength_score))

# Top strengths
top_strengths <- strengths_ranked |>
  head(3) |>
  mutate(category = "STRENGTH")

# Top weaknesses
reb_metric_names <- c("Def Reb", "Off Reb", "Total Reb", "ORB%", "DRB%", "Rebounds")

# We get it, rebounding underperformed, but want to show a little bit of variety outside of rebounding flaws
weaknesses <- strengths_ranked |>
  filter(!is_strength)

off_reb <- weaknesses |>
  filter(metric == "Off Reb") |>
  slice_max(abs_pct_diff, n = 1, with_ties = FALSE)

def_reb <- weaknesses |>
  filter(metric == "Def Reb") |>
  slice_max(abs_pct_diff, n = 1, with_ties = FALSE)

opp_points <- weaknesses |>
  filter(metric == "Opp Points") |>
  slice_max(abs_pct_diff, n = 1, with_ties = FALSE)

bottom_weaknesses <- bind_rows(off_reb, def_reb, opp_points) |>
  distinct(metric, .keep_all = TRUE) |>     
  mutate(category = "WEAKNESS") |>
  arrange(desc(abs_pct_diff))

# Combine for visualization
focus_metrics <- bind_rows(top_strengths, bottom_weaknesses) |>
  mutate(
    metric = fct_reorder(metric, pct_difference)
  )

# Create custom ordering: Strengths first (ranked), then Weaknesses (ranked)
focus_metrics_ordered <- focus_metrics |>
  arrange(desc(strength_score)) |>
  mutate(
    plot_order = row_number(),
    metric_label = paste0(metric, "\n",
                          if_else(pct_difference > 0, "+", ""),
                          round(pct_difference, 1), "%")
  )

# Create the ordering factor
focus_metrics_ordered <- focus_metrics_ordered |>
  mutate(
    metric_label = factor(metric_label, levels = unique(metric_label))
  )

# Prepare data for side-by-side bars
plot_data <- focus_metrics_ordered |>
  select(metric, metric_label, pacers, league, category, is_percentage, plot_order) |>
  pivot_longer(cols = c(pacers, league),
               names_to = "team",
               values_to = "value") |>
  mutate(
    team_label = if_else(team == "pacers", "Pacers", "Rest of League"),
    team_label = factor(team_label, levels = c("Pacers", "Rest of League"))
  )













colnames(nba_player_box)










