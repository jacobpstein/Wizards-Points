# =============================================================================
# Jamir Watkins RAPM Analysis
# Regularized Adjusted Plus-Minus using stint-level play-by-play data
# =============================================================================

library(tidyverse)
library(janitor)
library(cmdstanr)
library(tidybayes)
library(ggtext)
library(usaidplot)

set.seed(202)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 3)

wizards_id <- 1610612764
jamir_id <- 1642364

# bring data in from python----

all_pbp <- read_csv("jamir_pbp.csv", show_col_types = FALSE) |>
  rename(
    player1_id   = personId
    , order      = actionNumber
    , player_name = playerNameI
    , team_tricode = teamTricode
    , action_type = actionType
    , sub_type    = subType
    , shot_result = shotResult
    , shot_value  = shotValue
  ) |>
  mutate(game_id = str_pad(as.character(game_id), 10, pad = "0"))

game_dates <- read_csv("jamir_game_dates.csv", show_col_types = FALSE) |>
  clean_names() |>
  mutate(game_id = str_pad(as.character(game_id), 10, pad = "0"))

game_ids <- game_dates$game_id


# Preprocess v3 PBP → hoopR-compatible format ----
# v3 differences vs hoopR:
#   1. Substitutions: actionType="Substitution", subType=NA, personId=exiting player
#      description="SUB: [entering_last] FOR [exiting_last]"
#   2. Scoring: actionType="Made Shot"/"Missed Shot" (2pt/3pt via shotValue),
#               "Free Throw" (points via shotResult=="Made")
#   hoopR uses: action_type="substitution", sub_type="in"/"out";
#               action_type in c("2pt","3pt","freethrow"), shot_result=="Made"

# Build player lookup for entering player resolution
player_id_lookup <- all_pbp |>
  filter(player1_id > 0, !is.na(player_name), player_name != "") |>
  mutate(last_name = str_extract(player_name, "[^. ]+$") |> str_to_lower()) |>
  distinct(player1_id, team_tricode, last_name)

# Resolve ambiguous last names within same team by keeping most-common
player_id_lookup <- player_id_lookup |>
  group_by(team_tricode, last_name) |>
  slice_head(n = 1) |>
  ungroup()

# Non-substitution events: normalise action_type for scoring
all_pbp_non_sub <- all_pbp |>
  filter(action_type != "Substitution") |>
  mutate(
    # Map v3 shot types to hoopR-style
    action_type = case_when(
      action_type == "Made Shot"  & shot_value == 3 ~ "3pt"
      , action_type == "Made Shot"  & shot_value == 2 ~ "2pt"
      , action_type == "Missed Shot" & shot_value == 3 ~ "3pt"
      , action_type == "Missed Shot" & shot_value == 2 ~ "2pt"
      , action_type == "Free Throw" ~ "freethrow"
      , TRUE ~ action_type
    )
    , shot_result = case_when(
      str_detect(action_type, "pt") & !is.na(shot_result) ~ shot_result
      , action_type == "freethrow" ~ ifelse(!is.na(shot_result), shot_result, "Missed")
      , TRUE ~ shot_result
    )
    , sub_type = NA_character_
  )

# Substitution OUT rows (personId = exiting player)
subs_out <- all_pbp |>
  filter(action_type == "Substitution") |>
  mutate(action_type = "substitution", sub_type = "out")

# Substitution IN rows: parse entering player from description
subs_in <- all_pbp |>
  filter(action_type == "Substitution") |>
  mutate(
    entering_name = str_extract(description, "(?<=SUB: )[^F]+(?= FOR )") |> str_trim()
    , entering_last = str_extract(entering_name, "[^. ]+$") |> str_to_lower()
  ) |>
  left_join(
    player_id_lookup |> rename(entering_id = player1_id)
    , by = c("team_tricode", "entering_last" = "last_name")
  ) |>
  filter(!is.na(entering_id)) |>
  mutate(
    player1_id = entering_id
    , player_name = entering_name
    , action_type = "substitution"
    , sub_type = "in"
  ) |>
  dplyr::select(-entering_name, -entering_last, -entering_id)

all_pbp <- bind_rows(all_pbp_non_sub, subs_out, subs_in) |>
  arrange(game_id, order)

n_subs_in  <- nrow(subs_in)
n_subs_out <- nrow(subs_out)

# Derive player-team mapping from PBP (both teams included)
player_names <- all_pbp |>
  filter(!is.na(team_tricode), team_tricode != "", player1_id > 0) |>
  select(person_id = player1_id, player_name, team_tricode, game_id) |>
  distinct() |>
  group_by(person_id) |>
  slice_head(n = 1) |>
  ungroup()


#Stint Reconstruction ----

# Parse clock: "PT07M25.00S" -> seconds remaining in period
parse_clock <- function(clock) {
  mins <- as.numeric(str_extract(clock, "(?<=PT)\\d+(?=M)"))
  secs <- as.numeric(str_extract(clock, "(?<=M)[\\d.]+(?=S)"))
  mins * 60 + secs
}

# Convert period + clock to elapsed game seconds
clock_to_elapsed <- function(period, clock_remaining) {
  period_length <- ifelse(period <= 4, 720, 300)  # 12 min or 5 min OT
  period_start <- ifelse(
    period <= 4
    , (period - 1) * 720
    , 2880 + (period - 5) * 300
  )
  period_start + (period_length - clock_remaining)
}

# Identify starters for each period in a game
# Strategy: collect all candidate starters (pre-sub events + subbed-out players),
# then cap at 5 per team using earliest event order to break ties.
# This handles cases where bench players appear in pre-sub events (e.g., drawn fouls).
identify_period_starters <- function(pbp_game, period_num, prev_on_court = NULL) {
  p_events <- pbp_game |> filter(period == period_num)

  if (nrow(p_events) == 0) return(prev_on_court)

  sub_events <- p_events |> filter(action_type == "substitution")

  if (nrow(sub_events) == 0) {
    # No subs this period — starters are whoever was on court
    if (!is.null(prev_on_court)) return(prev_on_court)
    # Fall back to first 5 players per team by event order
    players <- p_events |>
      filter(!is.na(team_tricode), team_tricode != "", player1_id > 0) |>
      arrange(order) |>
      group_by(team_tricode) |>
      distinct(player1_id, .keep_all = TRUE) |>
      slice_head(n = 5) |>
      ungroup() |>
      select(player1_id, team_tricode)
    return(players)
  }

  first_sub_order <- min(sub_events$order)

  # Tier 1 (certain starters): players subbed OUT before being subbed IN
  # Their first action in this period is "out", meaning they started on court
  first_sub_action <- sub_events |>
    arrange(order) |>
    group_by(player1_id, team_tricode) |>
    slice_head(n = 1) |>
    ungroup()

  certain_starters <- first_sub_action |>
    filter(sub_type == "out") |>
    mutate(first_order = order) |>
    select(player1_id, team_tricode, first_order)

  # Tier 2 (likely starters): players in events before first sub
  # Exclude anyone whose first sub action is "in" (they came off the bench)
  bench_players <- first_sub_action |>
    filter(sub_type == "in") |>
    pull(player1_id)

  pre_sub <- p_events |>
    filter(order < first_sub_order, !is.na(team_tricode), team_tricode != "", player1_id > 0) |>
    filter(!(player1_id %in% bench_players)) |>
    group_by(player1_id, team_tricode) |>
    summarise(first_order = min(order), .groups = "drop")

  # Combine: certain starters first, then fill from pre-sub by event order
  candidates <- bind_rows(certain_starters, pre_sub) |>
    distinct(player1_id, team_tricode, .keep_all = TRUE) |>
    arrange(first_order)

  # Cap at 5 per team
  starters <- candidates |>
    group_by(team_tricode) |>
    slice_head(n = 5) |>
    ungroup() |>
    select(player1_id, team_tricode)

  starters
}

# Build stints for a single game
build_game_stints <- function(pbp_game, game_id) {
  # Derive teams from PBP
  teams <- pbp_game |>
    filter(!is.na(team_tricode), team_tricode != "") |>
    pull(team_tricode) |>
    unique()
  if (length(teams) != 2) return(NULL)

  wiz_tricode <- "WAS"
  opp_tricode <- setdiff(teams, wiz_tricode)
  if (length(opp_tricode) != 1) return(NULL)

  periods <- sort(unique(pbp_game$period))
  all_stints <- list()
  stint_counter <- 0
  prev_wiz_on <- NULL
  prev_opp_on <- NULL

  for (p in periods) {
    p_events <- pbp_game |> filter(period == p)
    if (nrow(p_events) == 0) next

    # Get starters for this period
    if (is.null(prev_wiz_on)) {
      # Period 1: identify from pre-sub events + sub-out players
      starters <- identify_period_starters(pbp_game, p)
      wiz_on <- starters |> filter(team_tricode == wiz_tricode) |> pull(player1_id)
      opp_on <- starters |> filter(team_tricode == opp_tricode) |> pull(player1_id)
    } else {
      # Periods 2+: start with previous period's ending lineup
      # The stint loop below will process subs (including those at period start)
      wiz_on <- prev_wiz_on
      opp_on <- prev_opp_on
    }

    # If still not 5v5, skip this period
    if (length(wiz_on) != 5 || length(opp_on) != 5) next

    # Get all substitution times in this period
    subs <- p_events |>
      filter(action_type == "substitution") |>
      mutate(clock_remaining = parse_clock(clock)) |>
      mutate(elapsed = clock_to_elapsed(period, clock_remaining)) |>
      arrange(order)

    # Period boundaries
    period_length <- ifelse(p <= 4, 720, 300)
    period_start_elapsed <- ifelse(p <= 4, (p - 1) * 720, 2880 + (p - 5) * 300)
    period_end_elapsed <- period_start_elapsed + period_length

    # Get scoring events for this period (made FGs and FTs)
    scoring <- p_events |>
      filter(
        action_type %in% c("2pt", "3pt", "freethrow")
        , shot_result == "Made"
      ) |>
      mutate(
        clock_remaining = parse_clock(clock)
        , elapsed = clock_to_elapsed(period, clock_remaining)
        , pts = case_when(
            action_type == "3pt" ~ 3L
            , action_type == "2pt" ~ 2L
            , action_type == "freethrow" ~ 1L
            , TRUE ~ 0L
          )
      )

    # Build stints by walking through substitutions
    sub_times <- unique(subs$elapsed)
    boundaries <- sort(unique(c(period_start_elapsed, sub_times, period_end_elapsed)))

    for (b in seq_len(length(boundaries) - 1)) {
      t_start <- boundaries[b]
      t_end <- boundaries[b + 1]
      duration_min <- (t_end - t_start) / 60

      if (duration_min < 0.05) next  # skip < 3 second stints

      # Process substitutions AT t_start
      subs_at_boundary <- subs |> filter(abs(elapsed - t_start) < 0.5)
      if (nrow(subs_at_boundary) > 0) {
        for (r in seq_len(nrow(subs_at_boundary))) {
          sub_row <- subs_at_boundary[r, ]
          pid <- sub_row$player1_id
          tc <- sub_row$team_tricode

          if (sub_row$sub_type == "out") {
            if (tc == wiz_tricode) wiz_on <- setdiff(wiz_on, pid)
            else opp_on <- setdiff(opp_on, pid)
          } else if (sub_row$sub_type == "in") {
            if (tc == wiz_tricode) wiz_on <- union(wiz_on, pid)
            else opp_on <- union(opp_on, pid)
          }
        }
      }

      # Skip if not 5v5
      if (length(wiz_on) != 5 || length(opp_on) != 5) next

      # Compute scoring in this stint
      stint_scoring <- scoring |>
        filter(elapsed > t_start - 0.5, elapsed <= t_end + 0.5)

      wiz_pts <- stint_scoring |>
        filter(team_tricode == wiz_tricode) |>
        pull(pts) |>
        sum(na.rm = TRUE)

      opp_pts <- stint_scoring |>
        filter(team_tricode == opp_tricode) |>
        pull(pts) |>
        sum(na.rm = TRUE)

      stint_counter <- stint_counter + 1
      all_stints[[stint_counter]] <- tibble(
        game_id = game_id
        , stint_id = stint_counter
        , period = p
        , t_start = t_start
        , t_end = t_end
        , duration_min = duration_min
        , wiz_pts = wiz_pts
        , opp_pts = opp_pts
        , margin = wiz_pts - opp_pts
        , margin_per_min = (wiz_pts - opp_pts) / duration_min
        , wiz_p1 = wiz_on[1], wiz_p2 = wiz_on[2], wiz_p3 = wiz_on[3]
        , wiz_p4 = wiz_on[4], wiz_p5 = wiz_on[5]
        , opp_p1 = opp_on[1], opp_p2 = opp_on[2], opp_p3 = opp_on[3]
        , opp_p4 = opp_on[4], opp_p5 = opp_on[5]
      )
    }

    # Save end-of-period lineup for next period
    prev_wiz_on <- wiz_on
    prev_opp_on <- opp_on
  }

  if (length(all_stints) == 0) return(NULL)
  bind_rows(all_stints)
}

# Process all games
all_stints <- list()
games_processed <- 0
games_skipped <- 0

for (gid in unique(all_pbp$game_id)) {
  pbp_game <- all_pbp |> filter(game_id == gid)

  if (nrow(pbp_game) < 50) {
    games_skipped <- games_skipped + 1
    next
  }

  stints <- tryCatch(
    build_game_stints(pbp_game, gid)
    , error = function(e) { message("  Stint error for ", gid, ": ", e$message); NULL }
  )

  if (!is.null(stints) && nrow(stints) > 0) {
    # Determine home/away: in nba_live_pbp, the first team listed in events is typically away
    # Use a simple heuristic: check if WAS is the home team from PBP description
    # The jumpball or period start events sometimes indicate this
    # Fallback: assume home = 1 (this is a minor covariate)
    stints$home <- 1  # will be overridden below if we can determine
    all_stints[[gid]] <- stints
    games_processed <- games_processed + 1
  } else {
    games_skipped <- games_skipped + 1
  }
}

# Determine home/away from game_dates (matchup column: "WAS vs. X" = home, "WAS @ X" = away)
if ("matchup" %in% names(game_dates)) {
  home_games <- game_dates |>
    mutate(wiz_home = !str_detect(matchup, "@")) |>
    select(game_id, wiz_home)
}

stints_df <- bind_rows(all_stints)

# Filter out very short stints and extreme margin_per_min
stints_df <- stints_df |>
  filter(duration_min >= 0.15)  # at least ~10 seconds



# Rim Protection Analysis ----
# Opponent shots at the restricted area with Jamir on vs off court
# Uses preprocessed all_pbp (area column preserved through rename pipeline)

has_area         <- "area" %in% names(all_pbp)
has_shot_distance <- "shotDistance" %in% names(all_pbp)

if (has_area) {
  opp_rim_shots <- all_pbp |>
    filter(action_type == "2pt", team_tricode != "WAS", area == "Restricted Area")
} else if (has_shot_distance) {
  message("NOTE: 'area' column not found — using shotDistance <= 4 as fallback")
  opp_rim_shots <- all_pbp |>
    filter(
      action_type == "2pt"
      , team_tricode != "WAS"
      , !is.na(as.numeric(shotDistance))
      , as.numeric(shotDistance) <= 4
    )
} else {
  message("WARNING: No area or shotDistance column in PBP — skipping rim protection")
  opp_rim_shots <- NULL
}

if (!is.null(opp_rim_shots) && nrow(opp_rim_shots) > 0) {
  opp_rim_shots <- opp_rim_shots |>
    mutate(
      clock_remaining = parse_clock(clock)
      , elapsed = clock_to_elapsed(period, clock_remaining)
      , made = shot_result == "Made"
    ) |>
    dplyr::select(game_id, order, elapsed, period, made)

  message("\nOpponent RA shot attempts: ", nrow(opp_rim_shots))

  # Join shots to stints by game_id, then filter to the matching elapsed window
  rim_with_stint <- opp_rim_shots |>
    left_join(
      stints_df |> dplyr::select(game_id, t_start, t_end, wiz_p1:wiz_p5)
      , by = "game_id"
      , relationship = "many-to-many"
    ) |>
    filter(elapsed >= t_start, elapsed <= t_end)

  # Deduplicate shots that fall exactly on a stint boundary
  rim_with_stint <- rim_with_stint |>
    group_by(game_id, order) |>
    slice_head(n = 1) |>
    ungroup()

  # Flag Jamir on/off for each shot
  rim_with_stint$jamir_on <- apply(rim_with_stint, 1, function(r) {
    as.numeric(jamir_id) %in% as.numeric(c(r["wiz_p1"], r["wiz_p2"], r["wiz_p3"],
                                             r["wiz_p4"], r["wiz_p5"]))
  })

  rim_summary <- rim_with_stint |>
    group_by(jamir_on) |>
    summarise(
      fga     = n()
      , fgm   = sum(made, na.rm = TRUE)
      , fg_pct = fgm / fga
      , .groups = "drop"
    )


  # Rim FG% with/without Watkins ----
  p_rim <- rim_summary |>
    mutate(label = if_else(jamir_on, "Watkins ON", "Watkins OFF")) |>
    ggplot(aes(x = label, y = fg_pct, fill = jamir_on)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_text(
      aes(label = paste0(round(fg_pct * 100, 1), "%\n(", fga, " att)"))
      , vjust = -0.3, size = 4
    ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.85)) +
    usaidplot::usaid_plot() +
    scale_fill_manual(values = c("TRUE" = "#172869FF", "FALSE" = "#D9565CFF")) +
    labs(
      title   = "Opponent restricted area FG% with and without Watkins on court"
      , subtitle = "2025-26 regular season · all opponent 2-point attempts in restricted area"
      , x = NULL, y = "FG%"
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    )

  ggsave("jamir_rim_protection.png", p_rim, width = 8, height = 7, dpi = 600, device = ragg::agg_png)
} else {
  rim_summary <- NULL
}


# Player Lookups ----

# player_names already built from PBP above

# Identify all Wizards and opponent players in stints
wiz_player_ids <- stints_df |>
  select(starts_with("wiz_p")) |>
  unlist() |>
  unique() |>
  sort()

opp_player_ids <- stints_df |>
  select(starts_with("opp_p")) |>
  unlist() |>
  unique() |>
  sort()

wiz_lookup <- tibble(person_id = wiz_player_ids) |>
  mutate(wiz_idx = row_number()) |>
  left_join(player_names |> select(person_id, player_name), by = "person_id")

opp_lookup <- tibble(person_id = opp_player_ids) |>
  mutate(opp_idx = row_number()) |>
  left_join(player_names |> select(person_id, player_name), by = "person_id")

jamir_wiz_idx <- wiz_lookup |> filter(person_id == jamir_id) |> pull(wiz_idx)

# let's take a look----

# Helper: check if Jamir is on court in a stint
jamir_on <- function(stint_row) {
  as.numeric(jamir_id) %in% c(stint_row$wiz_p1, stint_row$wiz_p2, stint_row$wiz_p3,
                                stint_row$wiz_p4, stint_row$wiz_p5)
}

stints_df$jamir_on <- apply(stints_df, 1, function(r) {
  as.numeric(jamir_id) %in% as.numeric(c(r["wiz_p1"], r["wiz_p2"], r["wiz_p3"],
                                           r["wiz_p4"], r["wiz_p5"]))
})

message("Jamir on court: ", sum(stints_df$jamir_on), " stints (",
        round(sum(stints_df$duration_min[stints_df$jamir_on]), 1), " min)")

# 4a: Most common lineup partners by shared minutes
# Pre-compute per-row on-court flags for each Wizards player
player_on_court <- map(wiz_lookup$person_id, function(pid) {
  apply(stints_df, 1, function(r) {
    pid %in% as.numeric(c(r["wiz_p1"], r["wiz_p2"], r["wiz_p3"],
                           r["wiz_p4"], r["wiz_p5"]))
  })
}) |> setNames(wiz_lookup$person_id)

shared_minutes <- map_dfr(wiz_lookup$person_id, function(pid) {
  if (pid == as.numeric(jamir_id)) return(NULL)

  p_on <- player_on_court[[as.character(pid)]]
  shared <- stints_df |> filter(jamir_on & p_on)

  tibble(
    person_id = pid
    , shared_min = sum(shared$duration_min)
    , shared_stints = nrow(shared)
    , raw_margin_per_min = if (nrow(shared) > 0) {
        weighted.mean(shared$margin_per_min, shared$duration_min)
      } else NA_real_
  )
})

shared_minutes <- shared_minutes |>
  left_join(wiz_lookup |> select(person_id, player_name), by = "person_id") |>
  filter(shared_min > 0) |>
  arrange(desc(shared_min))

# pairwise synergy
synergy_df <- map_dfr(wiz_lookup$person_id, function(pid) {
  if (pid == as.numeric(jamir_id)) return(NULL)

  p_on <- player_on_court[[as.character(pid)]]

  both_on <- stints_df |> filter(jamir_on & p_on)
  jamir_only <- stints_df |> filter(jamir_on & !p_on)
  p_only <- stints_df |> filter(!jamir_on & p_on)
  neither <- stints_df |> filter(!jamir_on & !p_on)

  wm <- function(df) {
    if (nrow(df) == 0 || sum(df$duration_min) < 1) return(NA_real_)
    weighted.mean(df$margin_per_min, df$duration_min)
  }

  both_net <- wm(both_on)
  jamir_net <- wm(jamir_only)
  p_net <- wm(p_only)
  neither_net <- wm(neither)

  # Synergy = actual together - expected additive
  expected_additive <- if (!is.na(jamir_net) && !is.na(p_net) && !is.na(neither_net)) {
    jamir_net + p_net - neither_net
  } else NA_real_

  synergy <- if (!is.na(both_net) && !is.na(expected_additive)) {
    both_net - expected_additive
  } else NA_real_

  tibble(
    person_id = pid
    , shared_min = sum(both_on$duration_min)
    , both_on_net = both_net
    , jamir_only_net = jamir_net
    , p_only_net = p_net
    , neither_net = neither_net
    , synergy = synergy
  )
})

synergy_df <- synergy_df |>
  left_join(wiz_lookup |> select(person_id, player_name), by = "person_id") |>
  filter(!is.na(synergy), shared_min >= 20) |>
  arrange(desc(synergy))

synergy_df |> select(player_name, shared_min, both_on_net, synergy) |> head(10)

# plot it

p_synergy <- synergy_df |>
  mutate(
    player_name = fct_reorder(player_name, synergy)
    , positive = synergy > 0
  ) |>
  ggplot(aes(x = synergy, y = player_name)) +
  geom_segment(
    aes(x = 0, xend = synergy, y = player_name, yend = player_name, color = positive)
    , linewidth = 1.2
  ) +
  geom_point(aes(color = positive, size = shared_min)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("TRUE" = "#3B9AB2", "FALSE" = "maroon")
    , guide = "none"
  ) +
  scale_size_continuous(
    range = c(2, 7)
    , name = "Shared\nMinutes"
  ) +
  labs(
    title = "Jamir Watkins' <span style='color:#3B9AB2;'>positive</span> and <span style='color:maroon;'>negative</span> pairwise synergy"
    , subtitle = "Net rating together vs expected from individual effects\nDot size = shared minutes; minimum 20 shared minutes"
    , x = "Synergy (points per minute above expected)"
    , y = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaidplot::usaid_plot() +
  theme(
    plot.title = element_markdown()
    , plot.subtitle = element_text(size = 11, color = "grey30")
  )

ggsave("jamir_rapm_synergy.png", p_synergy, width = 14, height = 10, dpi = 600, device = ragg::agg_png)


# Top positive synergy partners ----
top_synergy_rec2 <- synergy_df |>
  filter(synergy > 0) |>
  arrange(desc(synergy)) |>
  head(8) |>
  mutate(player_name = fct_reorder(player_name, synergy))

if (nrow(top_synergy_rec2) > 0) {
  p_rec2 <- ggplot(top_synergy_rec2, aes(x = synergy, y = player_name)) +
    geom_col(fill = "#20b2aa", width = 0.65) +
    geom_text(
      aes(label = paste0("+", round(synergy, 2), "  (", round(shared_min, 0), " min)"))
      , hjust = -0.05, size = 5, color = "grey30"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
    labs(
      title   = "Watkins' most productive lineup partners"
      , subtitle = "Positive synergy: points/min above additive expectation · minimum 20 shared minutes"
      , x = "Synergy (pts per minute above expected)"
      , y = NULL
      , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
    ) +
    usaidplot::usaid_plot() +
    theme(panel.grid.major.y = element_blank())

  ggsave("jamir_rec2_synergy.png", p_rec2, width = 12, height = 8, dpi = 600, device = ragg::agg_png)
  message("Saved jamir_rec2_synergy.png")
} else {
  message("No positive synergy partners found — skipping jamir_rec2_synergy.png")
}


# Bayesian RAPM ----

# Build sparse index matrices
wiz_on_matrix <- stints_df |>
  select(wiz_p1:wiz_p5) |>
  as.matrix()

opp_on_matrix <- stints_df |>
  select(opp_p1:opp_p5) |>
  as.matrix()

# Map person_ids to indices
wiz_id_to_idx <- setNames(wiz_lookup$wiz_idx, wiz_lookup$person_id)
opp_id_to_idx <- setNames(opp_lookup$opp_idx, opp_lookup$person_id)

wiz_on_idx <- matrix(
  wiz_id_to_idx[as.character(wiz_on_matrix)]
  , nrow = nrow(wiz_on_matrix)
  , ncol = 5
)

opp_on_idx <- matrix(
  opp_id_to_idx[as.character(opp_on_matrix)]
  , nrow = nrow(opp_on_matrix)
  , ncol = 5
)

# Check for NAs (players not in lookup)
wiz_na <- sum(is.na(wiz_on_idx))
opp_na <- sum(is.na(opp_on_idx))
message("\nNA in wiz index: ", wiz_na, " | NA in opp index: ", opp_na)

# Remove stints with NA indices
valid_rows <- complete.cases(wiz_on_idx) & complete.cases(opp_on_idx)
stints_model <- stints_df[valid_rows, ]
wiz_on_idx <- wiz_on_idx[valid_rows, ]
opp_on_idx <- opp_on_idx[valid_rows, ]


stan_data <- list(
  N = nrow(stints_model)
  , N_wiz = nrow(wiz_lookup)
  , N_opp = nrow(opp_lookup)
  , wiz_on = wiz_on_idx
  , opp_on = opp_on_idx
  , y = stints_model$margin_per_min
  , weights = stints_model$duration_min
  , home = stints_model$home
)

# Compile and fit
rapm_mod <- cmdstan_model("jamir_watkins_rapm.stan")

fit <- rapm_mod$sample(
  data = stan_data
  , seed = 202
  , chains = 4
  , parallel_chains = 4
  , iter_warmup = 2000
  , iter_sampling = 2000
  , adapt_delta = 0.95
  , max_treedepth = 15
)

# Convergence checks
fit$diagnostic_summary()

convergence_issues <- fit$summary() |>
  filter(rhat > 1.01 | ess_bulk < 400)

if (nrow(convergence_issues) > 0) {
  message("\nWARNING: Some parameters have convergence issues:")
  print(convergence_issues |> head(10))
} else {
  message("\nAll parameters converged: rhat < 1.01, ess_bulk > 400")
}

# LOO-CV
loo_result <- fit$loo()

# Extract RAPM Posteriors ----

# Wizards player RAPM
wiz_draws <- fit$draws("beta_wiz", format = "draws_df") |>
  pivot_longer(
    cols = starts_with("beta_wiz")
    , names_to = "variable"
    , values_to = "rapm"
  ) |>
  mutate(wiz_idx = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(wiz_lookup, by = "wiz_idx")

wiz_summary <- fit$summary("beta_wiz") |>
  mutate(wiz_idx = as.integer(str_extract(variable, "\\d+"))) |>
  left_join(wiz_lookup, by = "wiz_idx") |>
  arrange(desc(mean))

wiz_summary |>
  select(player_name, mean, median, q5, q95) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) 

# Jamir's RAPM
jamir_rapm <- wiz_summary |> filter(wiz_idx == jamir_wiz_idx)
jamir_rank <- which(wiz_summary$wiz_idx == jamir_wiz_idx)

prob_positive <- mean(wiz_draws$rapm[wiz_draws$wiz_idx == jamir_wiz_idx] > 0)

# RAPM Estimates ----

# Compute total stint-minutes per Wizards player
wiz_stint_minutes <- map_dfr(wiz_lookup$person_id, function(pid) {
  p_on <- player_on_court[[as.character(pid)]]
  tibble(person_id = pid, total_min = sum(stints_df$duration_min[p_on]))
})

# Filter to players with >= 50 stint-minutes and non-NA names
wiz_draws <- wiz_draws |>
  left_join(wiz_stint_minutes, by = "person_id") |>
  mutate(
    player_name = coalesce(player_name, paste0("Player ", person_id))
    , is_jamir = person_id == as.numeric(jamir_id)
  )

# Filter to meaningful players (50+ min) — always include Jamir
min_minutes <- 50
keep_players <- wiz_stint_minutes |>
  left_join(wiz_lookup |> select(person_id, player_name), by = "person_id") |>
  filter(total_min >= min_minutes | person_id == as.numeric(jamir_id)) |>
  filter(!is.na(player_name))

wiz_draws_filtered <- wiz_draws |>
  filter(person_id %in% keep_players$person_id)

# Compute medians for ordering
wiz_medians <- wiz_draws_filtered |>
  group_by(player_name) |>
  summarise(med_rapm = median(rapm), .groups = "drop")

# Create ordered factor for y-axis
ordered_names <- wiz_medians |> arrange(med_rapm) |> pull(player_name)
wiz_draws_filtered <- wiz_draws_filtered |>
  mutate(player_name = factor(player_name, levels = ordered_names))

# Build axis label styling
y_face <- ifelse(ordered_names == "Watkins", "bold", "plain")
y_color <- ifelse(ordered_names == "Watkins", "maroon", "grey30")

p_rapm <- wiz_draws_filtered |>
  ggplot(aes(
    x = rapm
    , y = player_name
    , fill = is_jamir
  )) +
  stat_halfeye(
    .width = c(0.5, 0.8, 0.95)
    , point_interval = "median_qi"
    , slab_alpha = 0.7
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(
    values = c("TRUE" = "maroon", "FALSE" = "#3B9AB2")
    , guide = "none"
  ) +
  labs(
    title = "<span style='color:maroon;'>Watkins</span>' Bayesian RAPM among <span style='color:#3B9AB2;'>Wizards teammates</span>"
    , subtitle = "Points per minute impact with 50/80/95% credible intervals\nHierarchical priors regularize players with fewer minutes toward the group mean"
    , x = "RAPM (points per minute)"
    , y = NULL
    , caption = "Data: nba.com/stats\nwizardspoints.substack.com"
  ) +
  usaidplot::usaid_plot() +
  theme(
    plot.title = element_markdown()
    , axis.text.y = element_text(face = y_face, color = y_color)
  )

ggsave("jamir_rapm_estimates.png", p_rapm, width = 16, height = 10, dpi = 600, device = ragg::agg_png)


