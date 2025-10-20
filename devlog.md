Slide 1: Title: Indiana Pacers Offseason Improvements

Slide 2: Strengths

Slide 3: Weaknesses

Slide 4: Trade Targets

Slide 5: Undervalued Player based on Analytics



# Package error
- nbastatR package removed from Cran
- Pivoted to hoopR library which is actively maintained and provides the NBA player/team box score data needed

# Data Type error
- plus_minus column was stored as a character instead of numeric
- used as.numeric to cast it to numeric from character

# Filter was too restrictive 
- Original filter provided too little qualified targets 

# Visual label cutoff
- Zach Edey player name was covered by other player names in trade analysis plot
- Added conditional logic in geom_text()



- Almost made mistake with De'Andre Hunter being the most undervalued player. Got weird warning saying Hunter was appearing twice due to my debugging print statement warnings
in the comparison function. Found out it was because it was only keeping track of half his season stats because of a mid season trade. Fixed code in the pipeline to now 
account for midseason trades.

Debug code: 

nba_analysis %>% 
  filter(athlete_display_name == "De'Andre Hunter") %>%
  select(athlete_display_name, team_abbreviation, games, avg_minutes)