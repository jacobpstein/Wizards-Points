########################################################
# This file scrapes data from basketball reference to investigate
# the importance of the first ten games for overall win percentage
# Session Info:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
#######################################################

# load packages
library(htmltab)
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(ggpmisc)

# set seed
set.seed(11122021)

# create a vector of years

year <- c(2000:2010, 2012:2019)


# loops to get data
get_urls <- function(year) {
  url <- paste0("https://www.basketball-reference.com/friv/standings.fcgi?month=11&day=15&year=", year)
}

urls <- get_urls(year)

get_data <- function(urls) {
  t1 <- htmltab(doc = urls, which = 1) %>% rename("Team" = 1) %>% filter(!str_detect(Team, "Division"))
  t2 <- htmltab(doc = urls, which = 2) %>% rename("Team" = 1) %>% filter(!str_detect(Team, "Division"))

  t1 %>% bind_rows(t2)

}

nba_list <- NULL

for(i in 1:length(year)){
  tmp <- get_data(urls[i]) %>% mutate(Year = year[i])
  nba_list[[i]] <- tmp
}

# collapse tables
nbastart_dat <- do.call(rbind, nba_list)

# let's just pick the columns we want right now
nbastart_dat_short <- nbastart_dat %>% select(Team, "Start" = `W/L%`, Year) %>%
  mutate(Period = "Start"
         # , Start = `W/L%`
         , Team = gsub("\\*","", Team)
)


# full season win/loss percentage
url2 <- "https://www.basketball-reference.com/leagues/NBA_2016_standings.html"

t3 <- htmltab(doc = url2, which = 1) %>% rename("Team" = 1) %>% filter(!str_detect(Team, "Division"))
t4 <- htmltab(doc = url2, which = 2) %>% rename("Team" = 1) %>% filter(!str_detect(Team, "Division"))

t3 %>% bind_rows(t4)


get_urls <- function(year) {
  url <- paste0( "https://www.basketball-reference.com/leagues/NBA_", year, "_standings.html")
}

urls <- get_urls(year)

get_data <- function(urls) {
  t1 <- htmltab(doc = urls, which = 1) %>% rename("Team" = 1) %>% filter(!str_detect(Team, "Division"))
  t2 <- htmltab(doc = urls, which = 2) %>% rename("Team" = 1) %>% filter(!str_detect(Team, "Division"))

  t1 %>% bind_rows(t2)

}

nba_list <- NULL

for(i in 1:length(year)){
  tmp <- get_data(urls[i]) %>% mutate(Year = year[i])
  nba_list[[i]] <- tmp

}

nbaseasons_dat <- do.call(rbind, nba_list)

nbaseasons_dat_short <- nbaseasons_dat %>%
  select(Team, "Season" = `W/L%`, Year) %>%
  mutate(Period = "Full Season"
         # , Season = `W/L%`
         , Team = gsub("\\*","", Team)
  )

# combine our data
nba_start_season <- nbaseasons_dat_short %>%
  full_join(nbastart_dat_short, by = c("Team", "Year")) %>%
  mutate(Season = as.numeric(Season)
        , Start = as.numeric(Start)) %>%
  group_by(Team) %>%
  mutate(lag_seasonwin_loss = lag(Season, n = 1)
         , lag_startwin_loss = lag(Start, n = 1)
         ) %>%
  ungroup()


# take a look
nba_start_season %>%
  drop_na() %>%
  mutate(Wizards = ifelse(Team == "Washington Wizards", "Wizards", "Not Wizards")
  ) %>%
  ggplot(aes(Start, Season)) +
  geom_point(aes(col = Wizards), size = 3) +
  geom_smooth(method= "lm", se = F, col = "#002B5C") +
  scale_color_manual(values = c("dark grey", "#E31837")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.ticks=element_blank()
        , panel.background=element_blank()
        , panel.grid.major = element_line()
        , legend.position="top"
        , legend.title = element_blank()
        , text=element_text(size = 22)

  ) +
  labs(y = "Regular Season Win %", x = "First 10 Games Win %"
       , caption = "Data scraped from basketball-reference.com\nusing R and the htmltab package"
  ) +  stat_poly_eq(formula = y~x,
                    eq.with.lhs = "italic(hat(y))~`=`~",
                    aes(label = paste(..eq.label.., sep = "*plain(\",\")~")),
                    parse = TRUE)


# model relationship
m1 <- stan_glmer(Season ~ Start + (1|Team), data = nba_start_season)

summary(m1)

# view model output
m1_out <- m1 %>%
  spread_draws(`(Intercept)`, b[,Team]) %>%
  mutate(overall = `(Intercept)` + b)

m1_out %>%
  mutate(Team = gsub("\\Team:","", Team)
         , Team = gsub("\\_"," ", Team)
         ) %>%
ggplot(aes(y=fct_rev(Team), x=overall)) +
  stat_pointinterval() +
  geom_vline(xintercept = 0.311) +
  scale_x_continuous(labels = scales::percent) +
  theme(axis.ticks=element_blank()
        , panel.background=element_blank()
        , panel.grid.major = element_line()
        , legend.position="top"
        , legend.title = element_blank()
        , text=element_text(size = 22)
  ) +
  labs(y = "", x = "Relationship of First 10 Games and Overall Performance"
  )

tidy_summary_1 <- tidy(m1, effects = "fixed",
                       conf.int = TRUE, conf.level = 0.80)
tidy_summary_1

# what about last season's win% to start win %?
m2 <- stan_glmer(Start~lag_seasonwin_loss + (1|Team), data = nba_start_season)

tidy_summary_2 <- tidy(m2,  effects = "fixed",
                       conf.int = TRUE, conf.level = 0.80)
tidy_summary_2

