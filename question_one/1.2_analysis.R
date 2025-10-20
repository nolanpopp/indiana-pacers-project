library(hoopR)
library(tidyverse)



# Establish the current season (2024-2025)
current_season <- 2025

# Load player box scores and exclude the All star game
nba_player_box <- load_nba_player_box(seasons = current_season) |>
  filter(game_date != '2025-02-16')




# ==== STEP 1: CALCULATE ADVANCED METRICS FOR ALL PLAYERS ====


all_players_advanced <- nba_player_box |>
  filter(game_date != '2025-02-16') |>  # Remove All-Star game
  # Convert plus_minus to numeric first
  mutate(plus_minus = as.numeric(plus_minus)) |>
  group_by(athlete_id, athlete_display_name, team_display_name, 
           team_short_display_name, athlete_position_abbreviation) |>
  summarise(
    games = n(),
    mpg = mean(minutes, na.rm = TRUE),
    
    # REBOUNDING METRICS
    trb_pg = mean(rebounds, na.rm = TRUE),
    orb_pg = mean(offensive_rebounds, na.rm = TRUE),
    drb_pg = mean(defensive_rebounds, na.rm = TRUE),
    
    # Rebounding Rate (rebounds per 36 minutes)
    reb_per_36 = (sum(rebounds, na.rm = TRUE) / sum(minutes, na.rm = TRUE)) * 36,
    orb_per_36 = (sum(offensive_rebounds, na.rm = TRUE) / sum(minutes, na.rm = TRUE)) * 36,
    drb_per_36 = (sum(defensive_rebounds, na.rm = TRUE) / sum(minutes, na.rm = TRUE)) * 36,
    
    # DEFENSIVE METRICS (available from box score)
    stl_pg = mean(steals, na.rm = TRUE),
    blk_pg = mean(blocks, na.rm = TRUE),
    
    # Defensive stocks (steals + blocks)
    stocks_pg = stl_pg + blk_pg,
    stocks_per_36 = (sum(steals, na.rm = TRUE) + sum(blocks, na.rm = TRUE)) / 
      sum(minutes, na.rm = TRUE) * 36,
    
    # Plus/Minus (basic defensive indicator - now numeric)
    avg_plus_minus = mean(plus_minus, na.rm = TRUE),
    
    # OFFENSIVE EFFICIENCY (to ensure they're not a liability)
    ppg = mean(points, na.rm = TRUE),
    apg = mean(assists, na.rm = TRUE),
    topg = mean(turnovers, na.rm = TRUE),
    
    fg_pct = sum(field_goals_made, na.rm = TRUE) / 
      sum(field_goals_attempted, na.rm = TRUE),
    
    three_pct = sum(three_point_field_goals_made, na.rm = TRUE) / 
      sum(three_point_field_goals_attempted, na.rm = TRUE),
    
    # True Shooting %
    ts_pct = sum(points, na.rm = TRUE) / 
      (2 * (sum(field_goals_attempted, na.rm = TRUE) + 
              0.44 * sum(free_throws_attempted, na.rm = TRUE))),
    
    # Fouls per game (lower is better for defense)
    fpg = mean(fouls, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  filter(
    games >= 15,           # Minimum sample size
    mpg >= 15,             # Getting real minutes
    team_short_display_name != "IND"  # Not currently on Pacers
  )

print(paste("Total players analyzed:", nrow(all_players_advanced)))




# ==== STEP 2: CALCULATE LEAGUE DISTRIBUTIONS ====


qualified_players <- all_players_advanced |>
  filter(mpg >= 20, games >= 15)

league_stats <- qualified_players |>
  summarise(
    # Rebounding
    mean_trb = mean(trb_pg, na.rm = TRUE),
    median_trb = median(trb_pg, na.rm = TRUE),
    q25_trb = quantile(trb_pg, 0.25, na.rm = TRUE),
    q75_trb = quantile(trb_pg, 0.75, na.rm = TRUE),
    
    mean_reb36 = mean(reb_per_36, na.rm = TRUE),
    median_reb36 = median(reb_per_36, na.rm = TRUE),
    q75_reb36 = quantile(reb_per_36, 0.75, na.rm = TRUE),
    
    # Defense
    mean_stocks = mean(stocks_pg, na.rm = TRUE),
    median_stocks = median(stocks_pg, na.rm = TRUE),
    q60_stocks = quantile(stocks_pg, 0.60, na.rm = TRUE),
    q70_stocks = quantile(stocks_pg, 0.70, na.rm = TRUE),
    
    # Efficiency
    mean_ts = mean(ts_pct, na.rm = TRUE),
    median_ts = median(ts_pct, na.rm = TRUE),
    q40_ts = quantile(ts_pct, 0.40, na.rm = TRUE),
    
    # Sample size
    n_players = n()
  )



# ==== STEP 3: CREATE COMPOSITE REBOUNDING & DEFENSE SCORE ====


# Apply data to set thresholds for scale
trade_targets <- all_players_advanced |>
  filter(
    # REBOUNDING: Top 25% (75th percentile = 6.04 RPG from the data)
    trb_pg >= 6.0,              # Top quartile rebounders (slightly below 75th percentile for more targets)
    reb_per_36 >= 7.4,          # 75th percentile efficiency (7.42 from the data)
    
    # DEFENSE: Above average defenders
    stocks_pg >= 1.5,           # 44th percentile (just above median of 1.41)
    
    # EFFICIENCY: Not offensive liability
    ts_pct >= 0.52,             # Captures 90% of players (very lenient - ensures offensive competence)
    
    # AVAILABILITY: Realistic targets
    mpg >= 20,                  # Regular rotation
    mpg <= 32,                  # Not untouchable star 
    ppg <= 18,                  # Not primary scorer
    
    # DURABILITY: Played enough games
    games >= 20
  ) |>
  # Add rankings
  mutate(
    # Simple composite score
    reb_score = trb_pg + (reb_per_36 * 0.3),
    def_score = stocks_pg * 2 + (avg_plus_minus * 0.5),
    total_score = reb_score + def_score,
    
    # Percentile ranks (within all NBA players with 20+ MPG)
    reb_percentile = round(percent_rank(reb_per_36) * 100, 0),
    def_percentile = round(percent_rank(stocks_pg) * 100, 0)
  ) |>
  arrange(desc(total_score)) |>
  select(athlete_display_name, team_short_display_name, 
         athlete_position_abbreviation, mpg, 
         trb_pg, reb_per_36, reb_percentile,
         stocks_pg, def_percentile,
         ppg, ts_pct, reb_score, def_score, 
         total_score)

print(paste("Found", nrow(trade_targets), "qualified targets"))



# ==== STEP 4: FIND TOP TEN TARGETS ====
print("Top 10 Trade Targets:")
print(trade_targets)




# ==== STEP 5: VISUALIZE THE TRADE TARGETS ====


# Scatter plot showing all targets
target_viz <- ggplot(trade_targets, 
                     aes(x = reb_per_36, y = stocks_pg, 
                         color = total_score, size = mpg)) +
  geom_point(alpha = 0.7) +
  # Label top 5 with smart positioning
  geom_text(data = trade_targets |> head(5) |>
              mutate(
                hjust_val = if_else(athlete_display_name == "Zach Edey", 
                                    1.1,  # Right-align (moves label to left of point)
                                    -0.1), # Left-align (normal)
                vjust_val = if_else(athlete_display_name == "Zach Edey",
                                    0.5,   # Center vertically
                                    0.4)
              ),
            aes(label = athlete_display_name, hjust = hjust_val, vjust = vjust_val),
            size = 3.5, fontface = "bold",
            show.legend = FALSE) +
  scale_color_gradient(low = "#cde5fd", high = "#002D62", 
                       name = "Total Score") +
  labs(
    title = "Trade Target Analysis",
    subtitle = paste("Analyzed", nrow(trade_targets), 
                     "qualified players meeting rebounding + defense thresholds"),
    x = "Rebounds Per 36 Minutes (Efficiency)",
    y = "Defensive Stocks Per Game (STL + BLK)",
    size = "Minutes/Game",
    caption = paste(
      "Top 5 Trade Targets Labeled")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(target_viz)





















# Apply data to set thresholds for scale
trade_targets <- all_players_advanced |>
  filter(
    # REBOUNDING: Top 25% (75th percentile = 6.04 RPG from the data)
    trb_pg >= 6.0,              # Top quartile rebounders (slightly below 75th percentile for more targets)
    reb_per_36 >= 7.4,          # 75th percentile efficiency (7.42 from the data)
    
    # DEFENSE: Above average defenders
    stocks_pg >= 1.5,           # 44th percentile (just above median of 1.41)
    
    # EFFICIENCY: Not offensive liability
    ts_pct >= 0.52,             # Captures 90% of players (very lenient - ensures offensive competence)
    
    # AVAILABILITY: Realistic targets
    mpg >= 20,                  # Regular rotation
    mpg <= 32,                  # Not untouchable star 
    ppg <= 18,                  # Not primary scorer
    
    # DURABILITY: Played enough games
    games >= 20
  ) |>
  # Add rankings
  mutate(
    # Simple composite score
    reb_score = trb_pg + (reb_per_36),
    def_score = stocks_pg * 2 + (avg_plus_minus),
    total_score = reb_score + def_score,
    
    # Percentile ranks (within all NBA players with 20+ MPG)
    reb_percentile = round(percent_rank(reb_per_36) * 100, 0),
    def_percentile = round(percent_rank(stocks_pg) * 100, 0)
  ) |>
  arrange(desc(total_score)) |>
  select(athlete_display_name, team_short_display_name, 
         athlete_position_abbreviation, mpg, 
         trb_pg, reb_per_36, reb_percentile,
         stocks_pg, def_percentile,
         ppg, ts_pct, reb_score, def_score, 
         total_score)

print(paste("Found", nrow(trade_targets), "qualified targets"))