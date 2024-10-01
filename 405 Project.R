library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(ggrepel)
library(plotly)

long_pbp <- load_pbp(1999:2024)


long_pbp %>%
  group_by(season) %>%
  summarise(n = n())


long_pbp %>%
  group_by(play_type) %>%
  summarise(n = n())


qbs <- long_pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  dplyr::summarise(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 10 & n_plays > 100)

qbs <- qbs %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

offense <- long_pbp %>%
  group_by(posteam) %>%
  summarise(off_epa = mean(epa, na.rm = TRUE))


defense <- long_pbp %>%
  group_by(defteam) %>%
  summarise(def_epa = mean(epa, na.rm = TRUE))

logos <- teams_colors_logos %>% dplyr::select(team_abbr, team_logo_espn)

epa_chart <- offense %>%
  dplyr::inner_join(defense, by = c("posteam" = "defteam")) %>%
  dplyr::inner_join(logos, by = c("posteam" = "team_abbr")) %>%
  ggplot2::ggplot(aes(x = off_epa, y = def_epa, alpha = 0.2)) +
  ggplot2::geom_abline(slope = -1.5, intercept = c(.4, .3, .2, .1, 0, -.1, -.2, -.3), alpha = .2) +
  ggplot2::geom_hline(aes(yintercept = mean(off_epa)), color = "red", linetype = "dashed") +
  ggplot2::geom_vline(aes(xintercept = mean(def_epa)), color = "red", linetype = "dashed") +
  ggimage::geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9, alpha = .1) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "NFL Offensive and Defensive EPA per Play 1999-2024"
  ) +
  ggplot2::theme(
    aspect.ratio = 9 / 16,
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  ggplot2::scale_y_reverse()

epa_chart





library(vip)


# Load in play-by-play data
first_pbp <- load_pbp(1999:2010)
second_pbp <- load_pbp(2011:2024)

# Getting just 4th downs
fourth_downs_first <- first_pbp |>
  filter(down == 4, !is.na(play_type))

fourth_downs_first <- fourth_downs_first %>%
  filter(!is.na(yardline_100) & !is.na(ydstogo) & !is.na(wp))

# See what usually happens on 4th down
fourth_downs_first |>
  group_by(play_type) |>
  tally(sort = T) |>
  print(n = 10)

# Creating an indicator variable
fourth_downs_first <- fourth_downs_first |>
  mutate(went_for_it = ifelse(play_type %in% c("pass", "run"), 1, 0))

# Seeing which variables correlate with going for it
fourth_downs_first |>
  group_by(ydstogo) |>
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it)) |>
  filter(count >= 5) |>
  ggplot(aes(x = ydstogo, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()

fourth_downs_first |>
  group_by(yardline_100) |>
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it)) |>
  filter(count >= 5) |>
  ggplot(aes(x = yardline_100, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()

fourth_downs_first |>
  group_by(ydstogo) |>
  summarize(went_for_it_rate = mean(went_for_it)) |>
  ggplot(aes(x = ydstogo, y = went_for_it_rate)) +
  geom_bar(stat = "identity") +
  theme_minimal()

fourth_downs_first |>
  mutate(wp_rounded = round(wp, 2)) |>
  group_by(wp_rounded) |>
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it)) |>
  ggplot(aes(x = wp_rounded, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()


# Checking the prediction probabilities
 fourth_downs_first |> 
   mutate(pred_prob = log_fourth$fitted.values) |> 
   ggplot(aes(x = ydstogo)) +
   geom_line(aes(y = pred_prob), color = "black", size = 2) +
   geom_point(aes(y = went_for_it, color = ifelse(went_for_it == 1, "darkgreen", "darkred")), 
              alpha = 0.3) +
   scale_color_identity() +
   theme_minimal() +
   labs(x = "Yards to Go",
        y = "Chance Offense Will Go For It (0-1)")

# Getting fourth down go's over expected
fourth_downs_first <- fourth_downs_first |> 
  mutate(pred_prob = log_fourth$fitted.values) |> 
  mutate(fourth_oe = went_for_it - pred_prob)


team_fourth_first <- fourth_downs_first |> 
  filter(season >= 1999 & season <= 2010) |> 
  group_by(posteam) |> 
  summarize(count = n(),
            exp_fourths = sum(pred_prob),
            actual_fourths = sum(went_for_it),
            fourths_oe = sum(fourth_oe)) |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

#########USING GGPLOT ###########

# Reshape the data for ggplot
team_fourth_long <- team_fourth_first |> 
  select(posteam, actual_fourths, exp_fourths, team_logo_espn) |> 
  pivot_longer(cols = c(actual_fourths, exp_fourths),
               names_to = "type",
               values_to = "value")

# # Create the bar chart with ggplot
# ggplot(team_fourth_long, aes(x = posteam, y = value, fill = type)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black") +
#   geom_image(aes(image = team_logo_espn), position = position_dodge(width = 0.9), 
#              size = 0.05, by = "width", asp = 16/9) +
#   theme_minimal() +
#   labs(x = "Teams",
#        y = "Number of 4th Downs",
#        title = "Team 4th Down Actual vs Expected Go's",
#        subtitle = "1999-2010") +
#   scale_fill_manual(values = c("actual_fourths" = "skyblue", "exp_fourths" = "lightgreen"),
#                     name = "Type",
#                     labels = c("Actual 4th Downs", "Expected 4th Downs")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate team names for readability
#         legend.position = "top")


ggplot(team_fourth_long, aes(x = posteam, y = value, color = type)) + 
  geom_point(position = position_jitter(width = 0.2), size = 4) +
  geom_image(aes(image = team_logo_espn), position = position_jitter(width = 0.2), size = 0.05, asp = 16/9) +
  theme_minimal() +
  labs(x = "Teams", 
       y = "Number of 4th Downs", 
       title = "Team 4th Down Actual vs Expected Go's", 
       subtitle = "1999-2010") + 
  scale_color_manual(values = c("actual_fourths" = "skyblue", "exp_fourths" = "lightgreen"), 
                     name = "Type", 
                     labels = c("Actual 4th Downs", "Expected 4th Downs")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")





###################### Making Linear Regression Model ########################

pass_plays <- pbp |> 
  filter(pass == 1) |> 
  filter(!is.na(air_yards), !is.na(down), !is.na(wp),
         !is.na(ydstogo), !is.na(half_seconds_remaining)) 

# Select just the columns we want
pass_play_model <- pass_plays |> 
  select(air_yards, down, wp, ydstogo, half_seconds_remaining, season) |> 
  mutate(down = as.factor(down))

str(pass_play_model)
colSums(is.na(pass_play_model))

# Make the linear model
air_yards_lm <- lm(air_yards ~ down + wp + ydstogo + 
                     half_seconds_remaining + as.factor(season), 
                   data = pass_play_model)

summary(air_yards_lm)

vip(air_yards_lm, num_features = 12)

# Getting predictions
air_yard_preds <- data.frame(predict.lm(air_yards_lm, newdata = pass_play_model)) |> 
  rename(exp_air_yards = predict.lm.air_yards_lm..newdata...pass_play_model.)

# Binding it to data frame
air_yards_projs <- cbind(pass_plays, air_yard_preds)

# Leaders in air yards over expected in 2021
ayoe_24 <- air_yards_projs |> 
  mutate(ayoe = air_yards - exp_air_yards) |> 
  filter(season == 2024) |> 
  group_by(passer) |> 
  summarize(passes = n(),
            exp_air_yards = mean(exp_air_yards),
            adot = mean(air_yards),
            avg_ayoe = mean(ayoe),
            team = last(posteam)) |> 
  #filter(passes >= 140) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# Making a bar graph
ayoe_22 |> 
  ggplot(aes(x = avg_ayoe, y = fct_reorder(passer, avg_ayoe))) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.6) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  geom_text(aes(label = paste0("n=", passes)), x = min(ayoe_22$avg_ayoe)-0.05, size = 3.5) +
  labs(x = "Average Air Yards Over Expected",
       y = "",
       title = "Average Air Yards Over Expected, 2024",
       subtitle = "Through Week 3") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) 

