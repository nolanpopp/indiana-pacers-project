library(hoopR)
library(tidyverse)


# ==== STEP 1: LOAD AND PROCESS DATA ==== 


# Establish the current season (2024-2025)
current_season <- 2025

# Load player box scores and exclude the All star game
nba_player_box <- load_nba_player_box(seasons = current_season) |>
  filter(game_date != '2025-02-16')





# ==== STEP 2:calculate league averages for comparison ====
league_averages <- nba_player_box |>
  filter(minutes > 10, season_type == 2) |>
  mutate(
    plus_minus = as.numeric(plus_minus),
    ts_pct = points / (2 * (field_goals_attempted + 0.44 * free_throws_attempted))
  ) |>
  summarise(
    league_avg_ts = mean(ts_pct, na.rm = TRUE),
    league_avg_pts_per_36 = mean((points / minutes) * 36, na.rm = TRUE),
    league_avg_reb_per_36 = mean((rebounds / minutes) * 36, na.rm = TRUE),
    league_avg_stl_per_36 = mean((steals / minutes) * 36, na.rm = TRUE),
    league_avg_blk_per_36 = mean((blocks / minutes) * 36, na.rm = TRUE),
    league_avg_def_reb_per_36 = mean((defensive_rebounds / minutes) * 36, na.rm = TRUE)
  )

# View the league averages
print(league_averages)




# ==== STEP 3: Calculate per-36-minute stats and efficiency ====
nba_analysis <- nba_player_box |>
  filter(minutes > 10, season_type == 2) |>
  mutate(
    plus_minus = as.numeric(plus_minus),
    ts_pct = points / (2 * (field_goals_attempted + 0.44 * free_throws_attempted)),
    ast_to_ratio = assists / pmax(turnovers, 1),
    reb_per_36 = (rebounds / minutes) * 36,
    pts_per_36 = (points / minutes) * 36,
    pm_per_36 = (plus_minus / minutes) * 36,
    stl_per_36 = (steals / minutes) * 36,
    blk_per_36 = (blocks / minutes) * 36,
    def_reb_per_36 = (defensive_rebounds / minutes) * 36
  ) |>
  group_by(athlete_id, athlete_display_name, 
           athlete_position_abbreviation) |>
  summarise(
    games = n(),
    current_team = last(team_abbreviation),
    current_team_name = last(team_name),
    teams_played_for = paste(unique(team_abbreviation), collapse = "/"),
    avg_minutes = mean(minutes),
    avg_ts = mean(ts_pct, na.rm = TRUE),
    avg_pts_per_36 = mean(pts_per_36, na.rm = TRUE),
    avg_ast_to = mean(ast_to_ratio, na.rm = TRUE),
    avg_reb_per_36 = mean(reb_per_36, na.rm = TRUE),
    avg_def_reb_per_36 = mean(def_reb_per_36, na.rm = TRUE),
    avg_stl_per_36 = mean(stl_per_36, na.rm = TRUE),
    avg_blk_per_36 = mean(blk_per_36, na.rm = TRUE),
    avg_pm36 = mean(pm_per_36, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(team_abbreviation = current_team,
         team_name = current_team_name)


# ==== STEP 4: FIND UNDERVALUED PLAYERS ====


# UNDERVALUED SCORER/SHOOTER (Guards and Wings only)
undervalued_scorer <- nba_analysis |>
  filter(
    avg_minutes >= 15 & avg_minutes <= 28,  # Not a star, not deep bench
    games >= 20,
    avg_pm36 > 0, # Positive impact while on the court
    team_abbreviation != 'IND', # Filter out Pacers players
    athlete_position_abbreviation %in% c("SG", "SF", "PG", "G", "F"),
    
    avg_ts > league_averages$league_avg_ts,  # Above average efficiency
    avg_pts_per_36 > league_averages$league_avg_pts_per_36,  # Above average scoring
  ) |>
  arrange(desc(avg_ts)) |>
  select(athlete_display_name, team_abbreviation, athlete_position_abbreviation,
         games, avg_minutes, avg_pts_per_36, avg_ts, avg_pm36)

print("=== UNDERVALUED SCORERS/SHOOTERS ===")
print(head(undervalued_scorer, 15))

# UNDERVALUED REBOUNDER/DEFENDER
undervalued_defender <- nba_analysis |>
  filter(
    avg_minutes >= 15 & avg_minutes <= 28,
    games >= 20,
    avg_pm36 > 0, # Positive impact while on the court
    avg_reb_per_36 > league_averages$league_avg_reb_per_36,
    (avg_stl_per_36 > league_averages$league_avg_stl_per_36 | 
       avg_blk_per_36 > league_averages$league_avg_blk_per_36),
    team_abbreviation != 'IND' # Filter out Pacers players
  ) |>
  mutate(
    defensive_score = avg_reb_per_36 + (avg_stl_per_36 * 2) + (avg_blk_per_36 * 2)
  ) |>
  arrange(desc(defensive_score)) |>
  select(athlete_display_name, team_abbreviation, athlete_position_abbreviation,
         games, avg_minutes, avg_reb_per_36, avg_stl_per_36, avg_blk_per_36, 
         defensive_score, avg_pm36)

print("=== UNDERVALUED REBOUNDERS/DEFENDERS ===")
print(head(undervalued_defender, 15))





# Statistical Comparison Function:
# Function to compare individual player statistics against league averages
# Returns a data frame showing player vs league average for key efficiency metrics
create_comparison <- function(player_name, nba_analysis, league_averages) {
  
  # Filter for the specified player
  player_stats <- nba_analysis |> 
    filter(athlete_display_name == player_name)
  
  # Check if player exists
  if(nrow(player_stats) == 0) {
    stop(paste("Player", player_name, "not found in dataset"))
  }
  
  # If multiple rows, take the first one
  if(nrow(player_stats) > 1) {
    warning(paste("Multiple entries found for", player_name, "- using first entry"))
    player_stats <- player_stats[1, ]
  }
  
  comparison <- data.frame(
    Metric = c("TS%", "Pts/36", "Reb/36", "Stl/36", "Blk/36"),
    Player = c(
      round(player_stats$avg_ts * 100, 1),
      round(player_stats$avg_pts_per_36, 1),
      round(player_stats$avg_reb_per_36, 1),
      round(player_stats$avg_stl_per_36, 1),
      round(player_stats$avg_blk_per_36, 1)
    ),
    League_Avg = c(
      round(league_averages$league_avg_ts * 100, 1),
      round(league_averages$league_avg_pts_per_36, 1),
      round(league_averages$league_avg_reb_per_36, 1),
      round(league_averages$league_avg_stl_per_36, 1),
      round(league_averages$league_avg_blk_per_36, 1)
    )
  ) |>
    mutate(Difference = Player - League_Avg)
  
  # Add player name and team to the output
  cat("\n=== Comparison for", player_name, "(",player_stats$team_abbreviation,") ===\n")
  cat("Minutes per game:", round(player_stats$avg_minutes, 1), "\n")
  cat("Games played:", player_stats$games, "\n\n")
  
  return(comparison)
}

# Use on #1 undervalued scorer/shooter and defender
comparison_jerome <- create_comparison("Ty Jerome", nba_analysis, league_averages)
print(comparison_jerome)

comparison_looney <- create_comparison("Kevon Looney", nba_analysis, league_averages)
print(comparison_looney)









# ==== STEP 5: Visualizing the undervalued players vs league average comparison ====

library(kableExtra)



# Ty Jerome comparison
player_stats_jerome <- nba_analysis |> 
  filter(athlete_display_name == "Ty Jerome")

player_stats_jerome <- player_stats_jerome[1, ]

comparison_jerome <- data.frame(
  Metric = c("TS%", "Pts/36", "Reb/36", "Stl/36", "Blk/36"),
  Player = c(
    round(player_stats_jerome$avg_ts * 100, 1),
    round(player_stats_jerome$avg_pts_per_36, 1),
    round(player_stats_jerome$avg_reb_per_36, 1),
    round(player_stats_jerome$avg_stl_per_36, 1),
    round(player_stats_jerome$avg_blk_per_36, 1)
  ),
  League_Avg = c(
    round(league_averages$league_avg_ts * 100, 1),
    round(league_averages$league_avg_pts_per_36, 1),
    round(league_averages$league_avg_reb_per_36, 1),
    round(league_averages$league_avg_stl_per_36, 1),
    round(league_averages$league_avg_blk_per_36, 1)
  )
) |>
  mutate(Difference = Player - League_Avg)


color_diff <- function(x) {
  max_pos <- max(x[x > 0], na.rm = TRUE)
  max_neg <- abs(min(x[x < 0], na.rm = TRUE))
  
  colors <- sapply(x, function(val) {
    if(val > 0) {
      # Positive = green (darker green for larger values)
      # Higher values should have LOWER intensity for darker color
      intensity <- val / max_pos
      rgb(0, 0.8 - intensity * 0.5, 0, alpha = 0.7)  # Reversed: 0.8 down to 0.3
    } else if(val < 0) {
      # Negative = red (darker red for larger negative values)
      intensity <- abs(val) / max_neg
      rgb(1.0 - intensity * 0.5, 0, 0, alpha = 0.7)  # Reversed: 1.0 down to 0.5
    } else {
      "lightgray"
    }
  })
  return(colors)
}


# Create styled table for Ty Jerome
styled_jerome <- comparison_jerome |>
  kbl(caption = paste0("Ty Jerome (", 
                       player_stats_jerome$team_name, " - ",
                       player_stats_jerome$athlete_position_abbreviation, ") | ",
                       round(player_stats_jerome$avg_minutes, 1), " MPG | ",
                       player_stats_jerome$games, " Games"),
      align = c("l", "c", "c", "c")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                font_size = 20) |>
  column_spec(4, 
              background = color_diff(comparison_jerome$Difference),
              color = "white",
              bold = TRUE)

styled_jerome


# Kevon Looney comparison
player_stats_looney <- nba_analysis %>% 
  filter(athlete_display_name == "Kevon Looney")

player_stats_looney <- player_stats_looney[1, ]

comparison_looney <- data.frame(
  Metric = c("TS%", "Pts/36", "Reb/36", "Stl/36", "Blk/36"),
  Player = c(
    round(player_stats_looney$avg_ts * 100, 1),
    round(player_stats_looney$avg_pts_per_36, 1),
    round(player_stats_looney$avg_reb_per_36, 1),
    round(player_stats_looney$avg_stl_per_36, 1),
    round(player_stats_looney$avg_blk_per_36, 1)
  ),
  League_Avg = c(
    round(league_averages$league_avg_ts * 100, 1),
    round(league_averages$league_avg_pts_per_36, 1),
    round(league_averages$league_avg_reb_per_36, 1),
    round(league_averages$league_avg_stl_per_36, 1),
    round(league_averages$league_avg_blk_per_36, 1)
  )
) %>%
  mutate(Difference = Player - League_Avg)

# Create styled table for Kevon Looney
styled_looney <- comparison_looney %>%
  kbl(caption = paste0("Kevon Looney (", 
                       player_stats_looney$team_name, " - ",
                       player_stats_looney$athlete_position_abbreviation, ") | ",
                       round(player_stats_looney$avg_minutes, 1), " MPG | ",
                       player_stats_looney$games, " Games"),
      align = c("l", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                font_size = 20) %>%  # Much bigger font
  column_spec(4, 
              background = color_diff(comparison_looney$Difference),
              color = "white",
              bold = TRUE)

styled_looney

