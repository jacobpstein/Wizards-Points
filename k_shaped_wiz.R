###########################################################
# Wiz K shaped economy
# Empirical test: elite-tier persistence vs. championship parity, 2010-11 through 2024-25
# Session info
# R version 4.5.3 (2026-03-11) -- "Reassured Reassurer"
# Copyright (C) 2026 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20
##########################################################

# set seed
set.seed(202)

# load libriares
library(tidyverse)
library(rvest)
library(patchwork)
library(cmdstanr)
library(tidybayes)
library(bayesplot)
library(ggridges)
library(ggtext)
library(ggrepel)
library(usaidplot)
library(extrafont)

extrafont::loadfonts(quiet = TRUE)

wiz_blue     <- "#172869FF"
wiz_red      <- "#D9565CFF"
teal         <- "#3B9AB2"
caption_text <- "Data: basketball-reference.com\nwizardspoints.substack.com"

# BBRef uses ending-year convention: 2025 = 2024-25 season
bbref_years <- 2011:2025


# create some helper functions--------

# Parse one conference/division standings table from a BBRef standings page
# Older pages (pre-2016) use divs_standings_* not confs_standings_*
parse_conf_table <- function(page, conf_suffix, year) {
  # Prefer confs_ table; fall back to divs_ for older seasons
  table_id <- if (length(page |> html_nodes(paste0('#confs_standings_', conf_suffix))) > 0)
    paste0("confs_standings_", conf_suffix)
  else
    paste0("divs_standings_", conf_suffix)
  rows <- page |> html_nodes(paste0('#', table_id, ' tbody tr'))
  map_dfr(rows, function(r) {
    link <- r |> html_node('[data-stat="team_name"] a')
    if (is.null(link) || is.na(html_attr(link, 'href'))) return(NULL)
    abbr      <- str_extract(html_attr(link, 'href'), '(?<=/teams/)[A-Z]+(?=/)')
    team_name <- html_text(link)
    w <- r |> html_node('[data-stat="wins"]')  |> html_text() |> as.integer()
    l <- r |> html_node('[data-stat="losses"]') |> html_text() |> as.integer()
    if (is.na(abbr) || is.na(w)) return(NULL)
    tibble(season_end = year, abbreviation = abbr, team_name = team_name, w = w, l = l)
  })
}

# Parse one playoff series table → winner/loser
parse_series_winner <- function(tbl) {
  games <- tbl |> filter(grepl('^Game', X1))
  if (nrow(games) < 4) return(NULL)
  g <- games |>
    mutate(
      away   = str_trim(X3)
      , home = str_remove(str_trim(X5), '^@ ')
      , ascore = suppressWarnings(as.integer(X4))
      , hscore = suppressWarnings(as.integer(X6))
    ) |>
    filter(!is.na(ascore), !is.na(hscore))
  if (nrow(g) < 4) return(NULL)
  teams <- unique(c(g$away, g$home))
  wins  <- sapply(teams, function(t)
    sum(g$away == t & g$ascore > g$hscore) +
    sum(g$home == t & g$hscore > g$ascore)
  )
  tibble(winner = names(which.max(wins)), loser = names(which.min(wins)))
}

# Parse full playoff bracket → per-team exit round
# BBRef table ordering: Finals (row 1), Conf Finals (2-3), Semis (4-7), 1st Round (8-15)
parse_playoff_rounds <- function(page) {
  tbls <- html_table(page, fill = TRUE)
  # Series tables: exactly 6 cols, 4-7 rows (one row per game in a best-of-7)
  series_tbls <- tbls[sapply(tbls, function(t) ncol(t) == 6 && nrow(t) >= 4 && nrow(t) <= 7)]
  series <- map_dfr(series_tbls, parse_series_winner)
  if (nrow(series) < 15) {
    message("  Warning: only ", nrow(series), " series found (expected 15)")
  }
  bind_rows(
    tibble(team_name = series$winner[1],          playoff_round = "champion")
    , tibble(team_name = series$loser[1],          playoff_round = "finals")
    , tibble(team_name = series$loser[2:3],        playoff_round = "conf_finals")
    , tibble(team_name = series$loser[4:7],        playoff_round = "second_round")
    , tibble(team_name = series$loser[8:nrow(series)], playoff_round = "first_round")
  )
}


# get data-------
if (file.exists("k_shaped_data.rds")) {
  panel_raw <- readRDS("k_shaped_data.rds")
} else {
  panel_raw <- map_dfr(bbref_years, function(yr) {
    tryCatch({
      message("Fetching ", yr - 1, "-", yr, "...")
      Sys.sleep(3)
      page_s <- read_html(
        paste0("https://www.basketball-reference.com/leagues/NBA_", yr, "_standings.html")
      )
      east <- parse_conf_table(page_s, "E", yr)
      west <- parse_conf_table(page_s, "W", yr)
      standings_yr <- bind_rows(east, west)

      Sys.sleep(3)
      page_p <- read_html(
        paste0("https://www.basketball-reference.com/playoffs/NBA_", yr, ".html")
      )
      rounds_yr <- parse_playoff_rounds(page_p)

      left_join(standings_yr, rounds_yr, by = "team_name")
    }, error = function(e) {
      message("Error for ", yr, ": ", e$message)
      NULL
    })
  })
  saveRDS(panel_raw, "k_shaped_data.rds")
}

# clean it up
# Map historical abbreviations to canonical franchise abbreviations
# (handles Charlotte Bobcats → Hornets, New Orleans Hornets → Pelicans)
franchise_map <- c("CHB" = "CHO", "NOH" = "NOP")

round_levels <- c("missed", "first_round", "second_round", "conf_finals", "finals", "champion")
round_labels <- c("Missed", "First Round", "Second Round", "Conf Finals", "Finals", "Champion")

panel <- panel_raw |>
  mutate(
    season_start = season_end - 1
    , games_played = w + l
    # Normalize to 82-game pace for 2020 (bubble ~72 games) and 2021 (72-game season)
    , wins_82      = w * 82 / games_played
    , playoff_round = if_else(is.na(playoff_round), "missed", playoff_round)
    , playoff_round = factor(playoff_round, levels = round_levels, labels = round_labels)
    , franchise    = if_else(abbreviation %in% names(franchise_map)
                             , franchise_map[abbreviation]
                             , abbreviation)
    , tier = case_when(
        wins_82 >= 50 ~ "Elite"
        , wins_82 >= 40 ~ "Contender"
        , wins_82 >= 27 ~ "Middle"
        , TRUE          ~ "Lottery"
      )
    , tier              = factor(tier, levels = c("Elite", "Contender", "Middle", "Lottery"))
    , made_second_round = as.integer(playoff_round %in% c("Second Round", "Conf Finals", "Finals", "Champion"))
    , made_conf_finals  = as.integer(playoff_round %in% c("Conf Finals", "Finals", "Champion"))
    , won_championship  = as.integer(playoff_round == "Champion")
    , is_wizards        = franchise == "WAS"
    , season_label      = paste0("'", substr(season_start + 1, 3, 4))
  ) |>
  arrange(franchise, season_start) |>
  group_by(franchise) |>
  mutate(
    prior_wins_82  = lag(wins_82)
    , prior_tier   = lag(tier)
  ) |>
  ungroup()

# who is getting deep runs?------

round_rank_map <- c(
  "Missed" = 0, "First Round" = 1, "Second Round" = 2
  , "Conf Finals" = 3, "Finals" = 4, "Champion" = 5
)

team_runs <- panel |>
  mutate(round_rank = round_rank_map[as.character(playoff_round)]) |>
  group_by(franchise) |>
  summarise(
    n_second_round = sum(made_second_round)
    , n_conf_finals  = sum(made_conf_finals)
    , n_champs       = sum(won_championship)
    , best_result    = max(round_rank)
    , .groups        = "drop"
  ) |>
  mutate(
    best_label = factor(best_result, levels = 0:5, labels = round_labels)
    , franchise  = fct_reorder(franchise, n_second_round)
    , is_wizards = franchise == "WAS"
  )

p1 <- ggplot(team_runs, aes(y = franchise, x = n_second_round)) +
  geom_segment(
    aes(yend = franchise, x = 0, xend = n_second_round, color = best_label)
    , linewidth = 0.8
  ) +
  geom_point(aes(color = best_label), size = 3) +
  geom_point(
    data = filter(team_runs, is_wizards)
    , aes(y = franchise, x = n_second_round)
    , color = wiz_blue, size = 5.5, shape = 21, stroke = 1.8, fill = NA
  ) +
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  annotate(
    "text", x = 6.8, y = 5
    , label = "Deep runs in \u22656 of 15 seasons"
    , hjust = 0, size = 3.2, color = "grey40"
  ) +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = c(
    "Missed"        = "#CCCCCC"
    , "First Round"  = "#AAAAAA"
    , "Second Round" = teal
    , "Conf Finals"  = "#0D6E8A"
    , "Finals"       = wiz_blue
    , "Champion"     = wiz_red
  )) +
  scale_x_continuous(breaks = 0:13) +
  labs(
    title = "The upper branch is small and stable"
    , subtitle = paste0(
        "Second-round-or-better appearances per team, 2010\u201311 through 2024\u201325 (15 seasons).\n"
        , "Dot color = best playoff result. Washington (WAS) circled."
      )
    , x = "Second Round or Better Appearances"
    , y = NULL
    , color = "Best result"
    , caption = caption_text
  ) +
  theme(legend.position = "right")

ggsave(
  "k_shaped_elite_concentration.png", p1
  , height = 12, width = 12, dpi = 300, device = ragg::agg_png
)

# heat map?

team_order <- panel |>
  group_by(franchise) |>
  summarise(n_deep = sum(made_second_round), n_cf = sum(made_conf_finals), .groups = "drop") |>
  arrange(desc(n_deep), desc(n_cf)) |>
  pull(franchise)

round_colors <- c(
  "Missed"        = "#EEEEEE"
  , "First Round"  = "#C8DCF0"
  , "Second Round" = "#7FB3E0"
  , "Conf Finals"  = teal
  , "Finals"       = wiz_blue
  , "Champion"     = wiz_red
)

p2_data <- panel |>
  mutate(
    team_fct   = factor(franchise, levels = rev(team_order))
    , season_fct = factor(season_label, levels = unique(season_label[order(season_start)]))
  )

p2 <- ggplot(p2_data, aes(x = season_fct, y = team_fct, fill = playoff_round)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_tile(
    data = filter(p2_data, is_wizards)
    , color = wiz_red, linewidth = 1.2, fill = NA
  ) +
  scale_fill_manual(values = round_colors) +
  usaid_plot(data_type = "discrete") +
  labs(
    title = "The same franchises run deep, season after season"
    , subtitle = "Playoff round by team, 2010\u201311 through 2024\u201325. Teams sorted by second-round appearances. WAS outlined."
    , x = NULL
    , y = NULL
    , fill = NULL
    , caption = caption_text
  ) +
  theme(
    axis.text.x    = element_text(size = 9)
    , panel.grid   = element_blank()
    , legend.position = "bottom"
  )

ggsave(
  "k_shaped_persistence_heatmap.png", p2
  , height = 14, width = 14, dpi = 300, device = ragg::agg_png
)


# rank corrs
ranked <- panel |>
  filter(!is.na(prior_wins_82)) |>
  mutate(
    tier_change = case_when(
      as.integer(tier) < as.integer(prior_tier) ~ "Rose"
      , as.integer(tier) > as.integer(prior_tier) ~ "Fell"
      , TRUE ~ "Stayed"
    )
    , tier_change_plot = tier_change
    , tier_change_plot = factor(tier_change_plot, levels = c("Rose", "Stayed", "Fell"))
  )

overall_rho <- cor(ranked$wins_82, ranked$prior_wins_82, method = "spearman") |> round(2)

tier_colors <- c("Rose" = teal, "Stayed" = "grey55", "Fell" = wiz_red)

# Labels for narrative teams; label ALL Wizards seasons
label_other <- ranked |>
  filter(
    (franchise == "OKC" & season_start %in% c(2019, 2020, 2021, 2024))
    | (franchise == "GSW" & season_start %in% c(2015, 2018))
    | (franchise == "PHI" & season_start %in% c(2015, 2016, 2018))
  )

label_wiz <- ranked |> filter(is_wizards)

p3 <- ggplot(ranked, aes(x = prior_wins_82, y = wins_82)) +
  geom_point(aes(color = tier_change_plot), alpha = 0.6, size = 3) +
  # Wizards points: larger with dark blue outline so they stand out
  geom_point(
    data = label_wiz
    , shape = 21, size = 4.2, stroke = 1.4
    , fill = NA, color = wiz_blue
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", fill = "grey80", linewidth = 0.8, alpha = 0.4) +
  # Labels for narrative teams (their tier color)
  geom_text_repel(
    data = label_other
    , aes(label = paste0(franchise, " '", substr(season_start + 1, 3, 4)), color = tier_change_plot)
    , size = 3, fontface = "bold", max.overlaps = 30, seed = 202
  ) +
  # Wizards labels always in wiz_blue, all seasons labeled
  geom_text_repel(
    data = label_wiz
    , aes(label = paste0("WAS '", substr(season_start + 1, 3, 4)))
    , color = wiz_blue, size = 3, fontface = "bold", max.overlaps = 30, seed = 202
  ) +
  annotate(
    "text", x = 15, y = 70
    , label = paste0("Spearman \u03c1 = ", overall_rho, "\n(all team-seasons)")
    , hjust = 0, size = 4, color = "grey25"
  ) +
  geom_hline(yintercept = c(27, 50), linetype = "dotted", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = c(27, 50), linetype = "dotted", color = "grey60", linewidth = 0.4) +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = tier_colors) +
  scale_x_continuous(limits = c(10, 75), breaks = seq(10, 70, 10)) +
  scale_y_continuous(limits = c(10, 75), breaks = seq(10, 70, 10)) +
  labs(
    title = "Where you are now predicts where you'll be next year"
    , subtitle = paste0(
        "Prior-season vs. current-season wins (82-game pace), 2011\u201312 through 2024\u201325.<br>"
        , "<span style='color:", teal, "'>**Rose**</span>, "
        , "<span style='color:grey55'>stayed</span>, or "
        , "<span style='color:", wiz_red, "'>**fell**</span> in win tier year over year. "
      )
    , x = "Prior Season Wins (82-game pace)"
    , y = "Current Season Wins (82-game pace)"
    , caption = caption_text
  ) +
  theme(
    plot.subtitle  = element_markdown()
    , legend.position = "none"
  )

ggsave(
  "k_shaped_rank_correlation.png", p3
  , height = 10, width = 12, dpi = 300, device = ragg::agg_png
)


tier_transitions <- panel |>
  filter(!is.na(prior_tier)) |>
  group_by(prior_tier, tier) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(prior_tier) |>
  mutate(
    pct       = n / sum(n)
    , pct_label = paste0(round(pct * 100), "%")
  ) |>
  ungroup() |>
  complete(prior_tier, tier, fill = list(n = 0, pct = 0, pct_label = "0%"))

p4 <- ggplot(
    tier_transitions
    , aes(x = tier, y = fct_rev(prior_tier), fill = pct)
  ) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = pct_label)
    , size = 5.5, fontface = "bold"
    , color = ifelse(tier_transitions$pct > 0.45, "white", "grey20")
  ) +
  usaid_plot(data_type = "continuous") +
  scale_fill_gradient2(
    low = "#F0F0F0", mid = teal, high = wiz_blue
    , midpoint = 0.40, labels = scales::percent, limits = c(0, 1)
  ) +
  scale_x_discrete(position = "top", labels = function(x) paste0("\u2192 ", x)) +
  labs(
    title = "Elite teams stay elite. Lottery teams stay in the lottery."
    , subtitle = "Year-to-year tier transition rates, 2010\u201311 through 2024\u201325. Row \u2192 column."
    , x = "Next Season Tier"
    , y = "Current Season Tier"
    , fill = "Transition rate"
    , caption = caption_text
  ) +
  theme(
    axis.text.x   = element_text(face = "bold", size = 11)
    , axis.text.y = element_text(face = "bold", size = 11)
    , panel.grid  = element_blank()
    , legend.position = "right"
  )

ggsave(
  "k_shaped_tier_transitions.png", p4
  , height = 8, width = 10, dpi = 300, device = ragg::agg_png
)


franchise_profiles <- panel |>
  group_by(franchise) |>
  summarise(
    n_deep      = sum(made_second_round)
    , n_lottery = sum(tier == "Lottery")
    , n_elite   = sum(tier == "Elite")
    , mean_wins = mean(wins_82)
    , sd_wins   = sd(wins_82)
    , n_seasons = n()
    , .groups   = "drop"
  ) |>
  mutate(
    profile = case_when(
      n_deep >= 6                        ~ "Perennial Contender"
      , n_deep >= 3 & n_lottery >= 3     ~ "Rebuilt Through Lottery"
      , n_deep >= 1 & n_lottery < 3      ~ "Occasional Contender"
      , TRUE                             ~ "Never Broken Through"
    )
    , profile = factor(profile, levels = c(
        "Perennial Contender"
        , "Rebuilt Through Lottery"
        , "Occasional Contender"
        , "Never Broken Through"
      ))
  )

franchise_profiles |> group_by(profile) |>
  summarise(
    median_lottery_seasons = median(n_lottery)
    , median_deep_runs     = median(n_deep)
    , median_wins          = median(mean_wins) |> round(1)
    , median_win_sd        = median(sd_wins) |> round(1)
    , .groups = "drop"
  )

# Two panels:
#   Left:  lottery seasons vs deep runs — shows whether lottery time was "converted"
#   Right: win volatility (SD) vs deep runs — shows the rebuild signature

profile_colors <- c(
  "Perennial Contender"      = wiz_blue
  , "Rebuilt Through Lottery" = teal
  , "Occasional Contender"    = "#B0B0B0"
  , "Never Broken Through"    = wiz_red
)

scatter_left <- franchise_profiles |>
  mutate(is_wiz = franchise == "WAS")

# Left panel
p_left <- ggplot(scatter_left, aes(x = n_lottery, y = n_deep)) +
  geom_hline(yintercept = c(3, 6), linetype = "dotted", color = "grey70", linewidth = 0.35) +
  geom_vline(xintercept = 3, linetype = "dotted", color = "grey70", linewidth = 0.35) +
  geom_point(
    data = filter(scatter_left, !is_wiz)
    , aes(color = profile, size = mean_wins)
    , alpha = 0.8
  ) +
  geom_point(
    data = filter(scatter_left, is_wiz)
    , aes(size = mean_wins)
    , color = wiz_red, shape = 21, stroke = 1.6, fill = wiz_red, alpha = 0.9
  ) +
  geom_text_repel(
    aes(label = franchise, color = profile)
    , size = 2.9, fontface = "bold"
    , max.overlaps = 40, seed = 202
    , segment.size = 0.3, segment.color = "grey70"
  ) +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = profile_colors, guide = "none") +
  scale_size_continuous(range = c(2.5, 8), guide = "none") +
  scale_x_continuous(breaks = 0:7) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  labs(
    title = "Lottery time is necessary, not sufficient"
    , subtitle = "Lottery seasons vs. second-round appearances, 2010\u201311\u20132024\u201325.\nPoint size = mean wins. Dotted lines at 3 lottery seasons, 3 and 6 deep runs."
    , x = "Seasons in Lottery tier (<27W)"
    , y = "Second-round-or-better appearances"
  ) +
  theme(plot.subtitle = element_text(size = 9))

# Right panel: win volatility vs deep runs
p_right <- ggplot(scatter_left, aes(x = sd_wins, y = n_deep)) +
  geom_hline(yintercept = c(3, 6), linetype = "dotted", color = "grey70", linewidth = 0.35) +
  geom_vline(xintercept = 12, linetype = "dotted", color = "grey70", linewidth = 0.35) +
  geom_point(
    data = filter(scatter_left, !is_wiz)
    , aes(color = profile, size = mean_wins)
    , alpha = 0.8
  ) +
  geom_point(
    data = filter(scatter_left, is_wiz)
    , aes(size = mean_wins)
    , color = wiz_red, shape = 21, stroke = 1.6, fill = wiz_red, alpha = 0.9
  ) +
  geom_text_repel(
    aes(label = franchise, color = profile)
    , size = 2.9, fontface = "bold"
    , max.overlaps = 40, seed = 202
    , segment.size = 0.3, segment.color = "grey70"
  ) +
  annotate(
    "text", x = 12.3, y = 11.5
    , label = "High volatility\n= rebuild signature"
    , hjust = 0, size = 3, color = "grey40", lineheight = 0.9
  ) +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = profile_colors) +
  scale_size_continuous(range = c(2.5, 8), guide = "none") +
  scale_x_continuous(breaks = seq(4, 18, 2)) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  labs(
    title = "Successful rebuilds require high volatility"
    , subtitle = "Win SD vs. second-round appearances. High SD = genuine trough\u2192rise cycle.\nPoint size = mean wins. Dotted line at SD = 12."
    , x = "Win volatility (SD of wins, 82-game pace)"
    , y = "Second-round-or-better appearances"
    , color = NULL
  ) +
  theme(
    plot.subtitle     = element_text(size = 9)
    , legend.position = "bottom"
    , legend.text     = element_text(size = 9)
  )

p4b_scatter <- p_left + p_right +
  plot_annotation(
    caption = caption_text
    , theme = theme(plot.caption = element_text(size = 8, color = "grey50"))
  )

ggsave(
  "k_shaped_characteristics.png", p4b_scatter
  , height = 9, width = 16, dpi = 300, device = ragg::agg_png
)

profile_traj <- panel |>
  left_join(franchise_profiles |> select(franchise, profile), by = "franchise") |>
  mutate(is_wizards = franchise == "WAS")

# Key franchises to label in each group
label_franchises <- c("WAS", "GSW", "SAS", "PHI", "OKC", "BOS", "CLE", "MIA", "SAC", "ORL", "DEN", "MIL")
label_df <- profile_traj |>
  filter(franchise %in% label_franchises) |>
  group_by(franchise, profile) |>
  slice_max(season_start, n = 1) |>
  ungroup()

p4b <- ggplot(
    profile_traj
    , aes(x = season_start, y = wins_82, group = franchise)
  ) +
  geom_hline(yintercept = c(27, 40, 50), linetype = "dotted", color = "grey70", linewidth = 0.35) +
  geom_line(
    data = filter(profile_traj, !is_wizards)
    , aes(color = profile), alpha = 0.45, linewidth = 0.6
  ) +
  geom_line(
    data = filter(profile_traj, is_wizards)
    , color = wiz_red, linewidth = 1.4, alpha = 0.9
  ) +
  geom_text_repel(
    data = filter(label_df, !is_wizards)
    , aes(label = franchise, color = profile)
    , size = 2.8, fontface = "bold"
    , nudge_x = 0.5, direction = "y", hjust = 0
    , segment.size = 0.3, segment.color = "grey70"
    , max.overlaps = 20, seed = 202
  ) +
  geom_text_repel(
    data = filter(label_df, is_wizards)
    , aes(label = franchise)
    , color = wiz_red, size = 3.2, fontface = "bold"
    , nudge_x = 0.5, direction = "y", hjust = 0
    , segment.size = 0.3, segment.color = wiz_red
    , max.overlaps = 20, seed = 202
  ) +
  facet_wrap(~ profile, ncol = 2) +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = profile_colors) +
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2022), labels = c("'10", "'13", "'16", "'19", "'22")) +
  scale_y_continuous(limits = c(5, 75), breaks = c(27, 40, 50)) +
  labs(
    title = "Every team that broke through first went through the lottery"
    , subtitle = paste0(
        "Win trajectories (82-game pace) by franchise profile, 2010\u201311 through 2024\u201325.\n"
        , "Dotted lines = tier thresholds (27, 40, 50 wins). "
        , "<span style='color:", wiz_red, "'>**Washington**</span> highlighted in each panel."
      )
    , x = NULL
    , y = "Wins (82-game pace)"
    , color = NULL
    , caption = caption_text
  ) +
  theme(
    plot.subtitle    = element_markdown()
    , legend.position = "none"
    , strip.text      = element_text(face = "bold", size = 11)
    , panel.spacing   = unit(1.2, "lines")
  )

ggsave(
  "k_shaped_team_profiles.png", p4b
  , height = 12, width = 14, dpi = 300, device = ragg::agg_png
)


# Pre-reform lottery odds (14-team lottery, official NBA weights)
# Source: NBA official odds used through 2018-19, then adjusted slightly
# Worst team = 14.0%, down to 14th seed = 0.5%
old_odds_top3 <- c(
  0.140, 0.134, 0.127, 0.119, 0.109, 0.099, 0.088, 0.076
  , 0.064, 0.051, 0.036, 0.020, 0.010, 0.005
) # 14 teams, P(#1 pick); #2 and #3 derived below

# For a rough expected top-3 pick probability, use ~3x the #1 odds
# (This is approximate — exact combinatorics depend on full draw structure)
# Under old system, expected pick # is roughly inversely proportional to rank
old_df <- tibble(
  wins = c(seq(10, 35, length.out = 14))  # ~14 lottery teams
  , p_top3_old = old_odds_top3 * 2.5       # approx P(top-3 pick)
)

# Build pick equity curves for each proposal as a function of win total
# Using the structural logic of each proposal rather than exact simulation
wins_seq <- seq(10, 65, by = 1)

# Old system: only bottom 14 teams, steeply declining odds
p_top3_old <- case_when(
  wins_seq <= 13 ~ 0.32    # ~10-13 wins: very high chance (worst 1-2 teams)
  , wins_seq <= 18 ~ 0.27
  , wins_seq <= 23 ~ 0.22
  , wins_seq <= 28 ~ 0.16
  , wins_seq <= 32 ~ 0.10
  , wins_seq <= 38 ~ 0.04  # lottery fringe
  , TRUE ~ 0.0             # no lottery access
)

# Every pick drawn in lottery — so all 18 teams have some access
# Play-in teams ~4% each for top pick; playoff first-round losers have tiny slice
p_top3_optA <- case_when(
  wins_seq <= 30 ~ 0.19    # bottom 10: flat 8% * ~2.4 for top-3 (all equal)
  , wins_seq <= 38 ~ 0.10  # play-in zone: 20% split among ~5 teams for top pick
  , wins_seq <= 48 ~ 0.03  # first-round playoff exit: small lottery slice
  , TRUE ~ 0.0
)

# Worst teams lose value because (a) their worst season is averaged down by prior year
# and (b) 14-win floor means extreme tanking yields only 20-win lottery equity
# Better coverage of middle (play-in + first-round exits) dilutes bottom teams further
p_top3_optB <- case_when(
  wins_seq <= 13 ~ 0.20    # floor effect: credited as 20W; loses top-of-bottom edge
  , wins_seq <= 18 ~ 0.19  # averaging smooths out the worst season
  , wins_seq <= 23 ~ 0.17
  , wins_seq <= 28 ~ 0.14
  , wins_seq <= 33 ~ 0.10
  , wins_seq <= 40 ~ 0.06  # more middle teams in lottery dilute
  , wins_seq <= 50 ~ 0.02
  , TRUE ~ 0.0
)

# Teams 6-18 in lottery get randomized second-tier draw for picks 6+
# Strong for true bottom 5; worst outcome for teams at 6th-10th worst position
p_top3_optC <- case_when(
  wins_seq <= 17 ~ 0.28    # likely bottom-5: high equal odds for top picks
  , wins_seq <= 24 ~ 0.09  # near bottom but outside top-5: 2nd-tier lottery only
  , wins_seq <= 30 ~ 0.07
  , wins_seq <= 38 ~ 0.05
  , wins_seq <= 48 ~ 0.01
  , TRUE ~ 0.0
)

equity_df <- tibble(
  wins         = wins_seq
  , `Pre-Reform`  = p_top3_old
  , `Option A`    = p_top3_optA
  , `Option B`    = p_top3_optB
  , `Option C`    = p_top3_optC
) |>
  pivot_longer(-wins, names_to = "system", values_to = "p_top3") |>
  mutate(system = factor(system, levels = c("Pre-Reform", "Option A", "Option B", "Option C")))

# Annotate the Process seasons (PHI wins during rebuild)
process_seasons <- tibble(
  wins   = c(19, 18, 10, 28)
  , year = c("'14", "'15", "'16", "'17")
  , pick = c("Noel\n#6", "Okafor\n#3", "Simmons\n#1", "Fultz\n#1")
)

system_colors <- c(
  "Pre-Reform" = "grey40"
  , "Option A"  = teal
  , "Option B"  = wiz_blue
  , "Option C"  = wiz_red
)

p4c <- ggplot(equity_df, aes(x = wins, y = p_top3, color = system)) +
  geom_line(linewidth = 1.1) +
  geom_vline(
    data = process_seasons
    , aes(xintercept = wins)
    , linetype = "dashed", color = "grey60", linewidth = 0.4
  ) +
  geom_text(
    data = process_seasons
    , aes(x = wins, y = 0.38, label = paste0("PHI ", year, "\n", pick))
    , inherit.aes = FALSE
    , size = 2.7, color = "grey35", hjust = 0.5, lineheight = 0.9
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.42)) +
  scale_x_continuous(limits = c(10, 65), breaks = seq(10, 60, 10)) +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = system_colors) +
  labs(
    title = "The reforms flatten the value of losing — and compress the escape route"
    , subtitle = paste0(
        "Approximate P(top-3 pick) by regular-season wins under each lottery structure.\n"
        , "Vertical lines show Philadelphia's Process seasons (2014\u201317) with picks received."
      )
    , x = "Team Wins (regular season)"
    , y = "P(top-3 pick)"
    , color = NULL
    , caption = caption_text
  ) +
  theme(legend.position = "bottom")

ggsave(
  "k_shaped_lottery_equity.png", p4c
  , height = 9, width = 12, dpi = 300, device = ragg::agg_png
)


# Bimodality coefficient: BC = (skewness^2 + 1) / kurtosis
# BC > 0.555 suggests bimodal distribution
bimodality_df <- panel |>
  group_by(season_start, season_label) |>
  summarise(
    mn = mean(wins_82)
    , s  = sd(wins_82)
    , sk = mean(((wins_82 - mn) / s)^3)
    , ku = mean(((wins_82 - mn) / s)^4)
    , bc = (sk^2 + 1) / ku
    , .groups = "drop"
  )


p5 <- panel |>
  mutate(
    season_fct = fct_rev(factor(season_label, levels = unique(season_label[order(season_start)])))
  ) |>
  ggplot(aes(x = wins_82, y = season_fct, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 1.6, rel_min_height = 0.01, bandwidth = 4.5
    , color = "white", linewidth = 0.4
  ) +
  geom_vline(xintercept = c(27, 40, 50), linetype = "dashed", color = "grey50", linewidth = 0.45) +
  annotate("text", x = 27.5, y = 0.6, label = "27W", hjust = 0, size = 3, color = "grey40") +
  annotate("text", x = 40.5, y = 0.6, label = "40W", hjust = 0, size = 3, color = "grey40") +
  annotate("text", x = 50.5, y = 0.6, label = "50W", hjust = 0, size = 3, color = "grey40") +
  geom_text(
    data = bimodality_df |>
      mutate(
        season_fct = fct_rev(factor(season_label, levels = unique(season_label[order(season_start)])))
      )
    , aes(x = 72, y = season_fct, label = paste0("BC=", round(bc, 2)), fill = NULL)
    , inherit.aes = FALSE, hjust = 1, size = 2.8, color = "grey40"
  ) +
  usaid_plot(data_type = "continuous") +
  scale_fill_gradient2(
    low = wiz_red, mid = "#DDDDDD", high = wiz_blue, midpoint = 41
  ) +
  scale_x_continuous(limits = c(5, 73), breaks = seq(10, 70, 10)) +
  labs(
    title = "Is the win distribution hollowing out?"
    , subtitle = "Team wins (82-game pace) by season. BC = bimodality coefficient (>0.555 = bimodal)."
    , x = "Wins (82-game pace)"
    , y = NULL
    , fill = "Wins"
    , caption = caption_text
  )

ggsave(
  "k_shaped_win_distribution.png", p5
  , height = 12, width = 11, dpi = 300, device = ragg::agg_png
)


model_df <- panel |>
  filter(!is.na(prior_wins_82)) |>
  arrange(franchise, season_start) |>
  group_by(franchise) |>
  mutate(delta_wins = prior_wins_82 - lag(prior_wins_82)) |>
  ungroup() |>
  filter(!is.na(delta_wins)) |>
  mutate(
    prior_wins_z  = as.numeric(scale(prior_wins_82))
    , delta_wins_z = as.numeric(scale(delta_wins))
    , team_idx    = as.integer(factor(franchise))
    , season_idx  = as.integer(factor(season_start))
  )

stan_mod <- cmdstan_model("k_shaped_persistence.stan")

# Cache draws matrices (not the full fit object, which references temp CSV files)
fit_model <- function(outcome_col, cache_file) {
  if (file.exists(cache_file)) {
    message("Loading cached draws: ", cache_file)
    return(readRDS(cache_file))
  }
  sdata <- list(
    N              = nrow(model_df)
    , N_teams      = max(model_df$team_idx)
    , N_seasons    = max(model_df$season_idx)
    , y            = model_df[[outcome_col]]
    , prior_wins_z = model_df$prior_wins_z
    , delta_wins_z = model_df$delta_wins_z
    , team_id      = model_df$team_idx
    , season_id    = model_df$season_idx
  )
  fit <- stan_mod$sample(
    data             = sdata
    , seed           = 202
    , chains         = 4
    , parallel_chains = 4
    , iter_warmup    = 1000
    , iter_sampling  = 1000
    , adapt_delta    = 0.99
    , refresh        = 200
  )
  cat("\n--- Diagnostics:", outcome_col, "---\n")
  print(fit$summary(c("alpha", "beta", "beta_delta", "sigma_team", "sigma_season")))
  draws <- fit$draws(c("alpha", "beta", "beta_delta"), format = "matrix")
  saveRDS(draws, cache_file)
  draws
}

draws_sr    <- fit_model("made_second_round", "k_shaped_bayes_fit_sr.rds")
draws_champ <- fit_model("won_championship",  "k_shaped_bayes_fit_champ.rds")

# Posterior marginal effects (hold delta_wins_z = 0, i.e. average trajectory)
wins_seq   <- seq(10, 70, by = 1)
wins_mu    <- mean(model_df$prior_wins_82)
wins_sd    <- sd(model_df$prior_wins_82)
wins_z_seq <- (wins_seq - wins_mu) / wins_sd

build_marginal <- function(draws, wins_z_seq, wins_seq, model_label) {
  map_dfr(seq_along(wins_seq), function(i) {
    # delta_wins_z held at 0 (average trajectory)
    lp <- draws[, "alpha"] + draws[, "beta"] * wins_z_seq[i]
    p  <- plogis(lp)
    tibble(
      prior_wins = wins_seq[i]
      , median   = median(p)
      , lo80     = quantile(p, 0.10)
      , hi80     = quantile(p, 0.90)
      , lo95     = quantile(p, 0.025)
      , hi95     = quantile(p, 0.975)
      , model    = model_label
    )
  })
}

marginal_all <- bind_rows(
  build_marginal(draws_sr,    wins_z_seq, wins_seq, "2nd Round or Better")
  , build_marginal(draws_champ, wins_z_seq, wins_seq, "Win Championship")
) |>
  mutate(model = factor(model, levels = c("2nd Round or Better", "Win Championship")))

p6 <- ggplot(marginal_all, aes(x = prior_wins, y = median, color = model, fill = model)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.12, color = NA) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = 0.22, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_rug(
    data = filter(panel, !is.na(prior_wins_82), made_second_round == 1)
    , aes(x = prior_wins_82, y = NULL, color = "2nd Round or Better")
    , sides = "t", alpha = 0.3, inherit.aes = FALSE, length = unit(0.03, "npc")
  ) +
  geom_rug(
    data = filter(panel, !is.na(prior_wins_82), won_championship == 1)
    , aes(x = prior_wins_82, y = NULL, color = "Win Championship")
    , sides = "t", alpha = 0.5, inherit.aes = FALSE, length = unit(0.05, "npc")
  ) +
  geom_vline(xintercept = c(27, 40, 50), linetype = "dashed", color = "grey60", linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(limits = c(10, 70), breaks = seq(10, 70, 10)) +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = c("2nd Round or Better" = wiz_blue, "Win Championship" = wiz_red)) +
  scale_fill_manual(values  = c("2nd Round or Better" = wiz_blue, "Win Championship" = wiz_red)) +
  labs(
    title = "**<span style='color:#172869FF'>Reaching the second round</span> is predictable. <span style='color:#D9565CFF'>Winning it all</span> is not.**"
    , subtitle = paste0(
        "Posterior P(outcome) as a function of prior-season wins. Hierarchical Bayesian logistic model.\n"
        , "Bands = 80% and 95% credible intervals. Rug marks = teams that achieved each outcome."
      )
    , x = "Prior Season Wins (82-game pace)"
    , y = "Posterior Probability"
    , color = NULL
    , fill  = NULL
    , caption = caption_text
  ) +
  theme(
    plot.title = element_markdown()
    , legend.position = "bottom"
  )

ggsave(
  "k_shaped_bayesian_persistence.png", p6
  , height = 9, width = 13, dpi = 300, device = ragg::agg_png
)

# Report momentum effect
beta_delta_sr    <- draws_sr[, "beta_delta"]
beta_delta_champ <- draws_champ[, "beta_delta"]
message(sprintf(
  "\nbeta_delta (2nd round): median=%.2f, 90%% CI [%.2f, %.2f], P(>0)=%.0f%%"
  , median(beta_delta_sr), quantile(beta_delta_sr, 0.05), quantile(beta_delta_sr, 0.95)
  , mean(beta_delta_sr > 0) * 100
))
message(sprintf(
  "beta_delta (champion):  median=%.2f, 90%% CI [%.2f, %.2f], P(>0)=%.0f%%"
  , median(beta_delta_champ), quantile(beta_delta_champ, 0.05), quantile(beta_delta_champ, 0.95)
  , mean(beta_delta_champ > 0) * 100
))


future_lookup <- panel |>
  select(franchise, season_start, tier, wins_82)

# For each lottery team-season, track tier and wins over next 5 seasons
lottery_instances <- panel |>
  filter(tier == "Lottery") |>
  select(franchise, lottery_year = season_start)

escape_paths <- crossing(lottery_instances, lead_yr = 0:5) |>
  mutate(target_season = lottery_year + lead_yr) |>
  left_join(
    future_lookup |> rename(future_tier = tier, future_wins = wins_82)
    , by = c("franchise", "target_season" = "season_start")
  ) |>
  filter(!is.na(future_tier))

# Classify each lottery stint by best tier reached within 5 seasons
escape_outcome <- escape_paths |>
  group_by(franchise, lottery_year) |>
  summarise(
    reached_elite      = any(future_tier == "Elite")
    , reached_contender = any(future_tier %in% c("Elite", "Contender"))
    , .groups = "drop"
  ) |>
  mutate(outcome = case_when(
    reached_elite      ~ "Reached Elite (50+ W)"
    , reached_contender ~ "Peaked at Contender (40-49 W)"
    , TRUE              ~ "Stayed Lottery / Middle"
  ) |> factor(levels = c("Reached Elite (50+ W)", "Peaked at Contender (40-49 W)", "Stayed Lottery / Middle")))


# LEFT PANEL: tier distribution at each year post-lottery
tier_dist <- escape_paths |>
  group_by(lead_yr, future_tier) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(lead_yr) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  mutate(
    future_tier = factor(future_tier, levels = c("Elite", "Contender", "Middle", "Lottery"))
    , lead_yr_label = paste0("Year +", lead_yr)
  )

magma4 <- scales::viridis_pal(option = "magma", begin = 0.15, end = 0.82)(4)
tier_fill_colors <- c(
  "Elite"     = magma4[4]
  , "Contender" = magma4[3]
  , "Middle"    = magma4[2]
  , "Lottery"   = magma4[1]
)

p_left_escape <- ggplot(
    tier_dist
    , aes(x = factor(lead_yr), y = pct, fill = fct_rev(future_tier))
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = ifelse(pct >= 0.08, paste0(round(pct * 100), "%"), ""))
    , position = position_stack(vjust = 0.5)
    , size = 3.5, fontface = "bold", color = "white"
  ) +
  usaid_plot(data_type = "discrete") +
  scale_fill_manual(values = tier_fill_colors, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("0" = "Lottery\nyear", "1" = "+1", "2" = "+2", "3" = "+3", "5" = "+5")) +
  labs(
    title = "The basement is not permanent\u2026"
    , subtitle = "Tier distribution of lottery teams at 0\u20135 seasons post-lottery.\n44% still lottery after 1 year; 11% after 3 years; 9% after 5 years."
    , x = "Seasons after lottery year"
    , y = "Share of lottery instances"
    , fill = NULL
  ) +
  theme(legend.position = "bottom")

# RIGHT PANEL: win trajectories by escape outcome
traj_by_outcome <- escape_paths |>
  left_join(escape_outcome, by = c("franchise", "lottery_year")) |>
  filter(!is.na(future_wins)) |>
  group_by(lead_yr, outcome) |>
  summarise(
    mean_wins = mean(future_wins)
    , se = sd(future_wins) / sqrt(n())
    , .groups = "drop"
  )

magma3 <- scales::viridis_pal(option = "magma", begin = 0.15, end = 0.82)(3)
outcome_colors <- c(
  "Reached Elite (50+ W)"          = magma3[3]
  , "Peaked at Contender (40-49 W)" = magma3[2]
  , "Stayed Lottery / Middle"       = magma3[1]
)

# Labels at the rightmost point of each line (year 5)
line_labels <- traj_by_outcome |>
  group_by(outcome) |>
  slice_max(lead_yr, n = 1) |>
  ungroup()

# Shorten labels for inline display
line_labels <- line_labels |>
  mutate(label_short = case_when(
    outcome == "Reached Elite (50+ W)"          ~ "Reached Elite"
    , outcome == "Peaked at Contender (40-49 W)" ~ "Peaked at\nContender"
    , TRUE                                        ~ "Stayed\nLottery/Middle"
  ))

p_right_escape <- ggplot(
    traj_by_outcome
    , aes(x = lead_yr, y = mean_wins, color = outcome, fill = outcome)
  ) +
  geom_ribbon(aes(ymin = mean_wins - se, ymax = mean_wins + se), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    data = line_labels
    , aes(label = label_short, color = outcome)
    , hjust = 0, nudge_x = 0.1, size = 3, fontface = "bold", lineheight = 0.9
    , show.legend = FALSE
  ) +
  geom_hline(yintercept = c(27, 40, 50), linetype = "dotted", color = "grey60", linewidth = 0.4) +
  annotate("text", x = 0.1, y = 51.5, label = "Elite (50W)", hjust = 0, size = 3, color = "grey40") +
  annotate("text", x = 0.1, y = 41.5, label = "Contender (40W)", hjust = 0, size = 3, color = "grey40") +
  annotate("text", x = 0.1, y = 28.5, label = "Middle (27W)", hjust = 0, size = 3, color = "grey40") +
  usaid_plot(data_type = "discrete") +
  scale_color_manual(values = outcome_colors) +
  scale_fill_manual(values = outcome_colors) +
  scale_x_continuous(breaks = 0:5, labels = c("Lottery\nyear", "+1", "+2", "+3", "+4", "+5")
                     , expand = expansion(mult = c(0.05, 0.22))) +
  scale_y_continuous(limits = c(15, 58), breaks = seq(20, 55, 10)) +
  labs(
    title = "\u2026but trajectory speed determines who escapes"
    , subtitle = "Elite escapers climb steeply; others plateau in Middle or Contender."
    , x = "Seasons after lottery year"
    , y = "Mean wins (82-game pace)"
    , color = NULL
    , fill  = NULL
  ) +
  theme(legend.position = "none")

p_escape <- p_left_escape + p_right_escape +
  plot_annotation(
    caption = caption_text
    , theme = theme(plot.caption = element_text(size = 8, color = "grey50"))
  )

ggsave(
  "k_shaped_lottery_escape.png", p_escape
  , height = 9, width = 16, dpi = 300, device = ragg::agg_png
)
