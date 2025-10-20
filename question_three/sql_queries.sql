-- QUESTION ONE:
SELECT
	p.full_name AS player_name,
	ROUND(SUM(ps.pts) * 1.0 / NULLIF(SUM(ps.games_played), 0), 1) AS points_per_game
FROM dbo.player_stats ps
INNER JOIN dbo.players p 
	ON ps.player_id = p.player_id
INNER JOIN dbo.teams t
	ON t.team_id = p.team_id
WHERE t.conference = 'Western'
	AND ps.season = 2022
GROUP BY p.full_name
ORDER BY points_per_game DESC;

-- QUESTION TWO
WITH playoff_games AS (
	SELECT 
		home_team_id AS team_id,
		game_id
	FROM dbo.games
	WHERE season = 2022
		AND game_type = 'playoffs' 

	UNION ALL 

	SELECT 
		away_team_id AS team_id,
		game_id
	FROM dbo.games
	WHERE season = 2022
		AND game_type = 'playoffs'
)
SELECT
	t.team_full_name AS team_name,
	COUNT(pg.game_id) AS total_playoff_games
FROM playoff_games pg
INNER JOIN dbo.teams t
	ON pg.team_id = t.team_id
GROUP BY t.team_full_name
ORDER BY total_playoff_games DESC;


-- QUESTION THREE
WITH RegularSeasonStarters AS (
    SELECT 
        player_id,
        ROUND(SUM(games_started) * 100.0 / NULLIF(SUM(games_played), 0), 1) AS pct_games_started
    FROM dbo.player_stats
    WHERE game_type = 'regular'
        AND season = 2022
    GROUP BY player_id
    HAVING SUM(games_started) * 100.0 / NULLIF(SUM(games_played), 0) >= 50
)
SELECT TOP 1
    p.full_name AS player_name,
    ROUND(SUM(ps.minutes) * 1.0 / NULLIF(SUM(ps.games_played), 0), 1) AS minutes_per_game,
    rss.pct_games_started AS reg_season_start_pct
FROM dbo.player_stats ps
    INNER JOIN dbo.players p
        ON ps.player_id = p.player_id
    INNER JOIN RegularSeasonStarters rss
        ON ps.player_id = rss.player_id
WHERE ps.game_type = 'playoffs'
    AND ps.season = 2022
GROUP BY p.full_name, rss.pct_games_started
ORDER BY minutes_per_game DESC;


-- QUESTION FOUR
WITH PlayoffTeams AS (
    SELECT DISTINCT home_team_id AS team_id
    FROM dbo.games
    WHERE season = 2022 
        AND game_type = 'playoffs'
    UNION
    SELECT DISTINCT away_team_id
    FROM dbo.games
    WHERE season = 2022 
        AND game_type = 'playoffs'
)
SELECT TOP 5
	p.full_name AS PlayerName,
    ROUND(SUM(ps.pts) * 1.0 / NULLIF(SUM(ps.games_played), 0), 1) AS points_per_game,
    t.team_full_name AS current_team
FROM dbo.player_stats ps
    INNER JOIN dbo.players p
        ON ps.player_id = p.player_id
    INNER JOIN dbo.teams t
        ON p.team_id = t.team_id
    INNER JOIN PlayoffTeams pt
        ON p.team_id = pt.team_id
WHERE ps.season = 2022
    AND ps.game_type = 'regular'
GROUP BY p.full_name, t.team_full_name
ORDER BY points_per_game DESC;