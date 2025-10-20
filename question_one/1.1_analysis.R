library(hoopR)
library(tidyverse)


# ==== PART 1: FINDING PACERS STRENGTHS AND WEAKNESSES ====


# == 1.1: Load and Process the NBA team data == 


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

    
    # Free Throw %
    ft_pct = free_throws_made / pmax(free_throws_attempted, 1)
  )


# == 1.2 Calculate Averages for both the Pacers and Rest of the League ==

# Calculate league wide season averages not including the Pacers
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

print("Pacers Season Averages:")
print(pacers_averages)



# == 1.3: Comparing the averages between the Pacers and the rest of the League

comparison_data <- tibble(
  metric = c(
    "Points","Opp Points","FG%","3PT%","Assists","Rebounds","Turnovers",
    "eFG%","TOV%","ORB%","DRB%","FT%",
    "Blocks","Steals","Def Reb","Off Reb","Total Reb",
    "Paint Pts",
    "Total Turnovers","Turnover Points"
  ),
  
  # Pacers values (from pacers_averages)
  pacers = c(
    pacers_averages$avg_points,
    pacers_averages$avg_opp_points,
    pacers_averages$avg_fg_pct,
    pacers_averages$avg_three_pct,
    pacers_averages$avg_assists,
    pacers_averages$avg_rebounds,
    pacers_averages$avg_turnovers,
    pacers_averages$avg_efg_pct,
    pacers_averages$avg_tov_pct,
    pacers_averages$avg_orb_pct,
    pacers_averages$avg_drb_pct,
    pacers_averages$avg_ft_pct,
    pacers_averages$avg_blocks,
    pacers_averages$avg_steals,
    pacers_averages$avg_defensive_rebounds,
    pacers_averages$avg_offensive_rebounds,
    pacers_averages$avg_rebounds,
    pacers_averages$avg_points_in_paint,
    pacers_averages$avg_total_turnovers,
    pacers_averages$avg_turnover_points
  ),
  
  # League values (from all_team_averages)
  league = c(
    all_team_averages$avg_points,
    all_team_averages$avg_opp_points,
    all_team_averages$avg_fg_pct,
    all_team_averages$avg_three_pct,
    all_team_averages$avg_assists,
    all_team_averages$avg_rebounds,
    all_team_averages$avg_turnovers,
    all_team_averages$avg_efg_pct,
    all_team_averages$avg_tov_pct,
    all_team_averages$avg_orb_pct,
    all_team_averages$avg_drb_pct,
    all_team_averages$avg_ft_pct,
    all_team_averages$avg_blocks,
    all_team_averages$avg_steals,
    all_team_averages$avg_defensive_rebounds,
    all_team_averages$avg_offensive_rebounds,
    all_team_averages$avg_rebounds,
    all_team_averages$avg_points_in_paint,
    all_team_averages$avg_total_turnovers,
    all_team_averages$avg_turnover_points
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


# === 1.4: Creating/Processing the Strengths and Weakness in order to plot ===

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


# === 1.5 Creating the Plots === 

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

# --- Weaknesses plot ---
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


# Print both plots
print(strengths_plot)
print(weakness_plot)




