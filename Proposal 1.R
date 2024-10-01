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


####### Without GGPLOT EPA Chart #########

# Sample offense and defense data (replace with your actual data)
off_epa <- epa_data$off_epa
def_epa <- epa_data$def_epa
team_names <- epa_data$posteam  # Assuming this column contains team abbreviations

# Set up the plotting area (this initializes the plot)
plot(off_epa, def_epa, 
     xlab = "Offense EPA/play", 
     ylab = "Defense EPA/play", 
     main = "NFL Offensive and Defensive EPA per Play 1999-2024",
     pch = 16,  # Use solid circle to plot points
     col = "blue",  # Color of points
     asp = 16/9,  # Set aspect ratio
     xlim = range(off_epa, na.rm = TRUE),
     ylim = range(def_epa, na.rm = TRUE)
)

# Now add diagonal lines (after plot is initialized)
slopes <- c(.4, .3, .2, .1, 0, -.1, -.2, -.3)
for (i in seq_along(slopes)) {
  abline(a = slopes[i], b = -1.5, col = "gray", lty = "dotted", lwd = 0.5)
}

# Add horizontal and vertical reference lines (after plot is initialized)
abline(h = mean(off_epa, na.rm = TRUE), col = "red", lty = "dashed")
abline(v = mean(def_epa, na.rm = TRUE), col = "red", lty = "dashed")

# Label the points with team names
text(off_epa, def_epa, labels = team_names, pos = 4, cex = 0.8, col = "black")

# Reverse y-axis (optional: similar to scale_y_reverse in ggplot)
axis(2, at = rev(pretty(def_epa)))  # Flip the y-axis ticks and labels

# Add additional text and captions
mtext("Data: @nflfastR", side = 1, line = 3)
title("NFL Offensive and Defensive EPA per Play 1999-2024", line = 0.5, font = 2, cex.main = 1.2)

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
ydstogo_summary <- fourth_downs_first %>%
  group_by(ydstogo) %>%
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it)) %>%
  filter(count >= 5)


barplot(ydstogo_summary$went_for_it_rate,
        names.arg = ydstogo_summary$ydstogo,
        col = heat.colors(length(ydstogo_summary$went_for_it_rate)), # Color fill
        xlab = "Yards to Go",
        ylab = "Went For It Rate",
        main = "Went For It Rate by Yards to Go",
        border = "white")

# Summarize the data
yardline_summary <- fourth_downs_first %>%
  group_by(yardline_100) %>%
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it)) %>%
  filter(count >= 5)

# Create the barplot
barplot(yardline_summary$went_for_it_rate,
        names.arg = yardline_summary$yardline_100,
        col = heat.colors(length(yardline_summary$went_for_it_rate)), # Color fill
        xlab = "Yardline",
        ylab = "Went For It Rate",
        main = "Went For It Rate by Yardline",
        border = "white")

# Summarize the data
ydstogo_simple <- fourth_downs_first %>%
  group_by(ydstogo) %>%
  summarize(went_for_it_rate = mean(went_for_it))

# Create the barplot
barplot(ydstogo_simple$went_for_it_rate,
        names.arg = ydstogo_simple$ydstogo,
        col = "skyblue",
        xlab = "Yards to Go",
        ylab = "Went For It Rate",
        main = "Went For It Rate by Yards to Go (Simple)",
        border = "white")

# Summarize the data
wp_summary <- fourth_downs_first %>%
  mutate(wp_rounded = round(wp, 2)) %>%
  group_by(wp_rounded) %>%
  summarize(count = n(),
            went_for_it_rate = mean(went_for_it))

# Create the barplot
barplot(wp_summary$went_for_it_rate,
        names.arg = wp_summary$wp_rounded,
        col = heat.colors(length(wp_summary$went_for_it_rate)), # Color fill
        xlab = "Win Probability (Rounded)",
        ylab = "Went For It Rate",
        main = "Went For It Rate by Win Probability",
        border = "white")


# Making a logistic regression model
log_fourth <- glm(went_for_it ~ yardline_100 + ydstogo + wp, 
                  data = fourth_downs_first)

# Getting the summary
summary(log_fourth)

# Getting the variable importance
vip(log_fourth)

# Accounting for interaction effects
log_fourth_co <- glm(went_for_it ~ (yardline_100 + ydstogo + wp)^2, 
                     data = fourth_downs_first)

summary(log_fourth_co)


fourth_downs_first$pred_prob <- log_fourth$fitted.values

# Create the initial plot (line plot of pred_prob vs. ydstogo)
plot(fourth_downs_first$ydstogo, fourth_downs_first$pred_prob, 
     type = "l",  # Line plot
     col = "black", 
     lwd = 2,  # Line thickness
     xlab = "Yards to Go", 
     ylab = "Chance Offense Will Go For It (0-1)",
     main = "Predicted Probability of Going for It vs. Yards to Go")

# Overlay points: different colors for went_for_it == 1 and 0
points(fourth_downs_first$ydstogo[fourth_downs_first$went_for_it == 1], 
       fourth_downs_first$went_for_it[fourth_downs_first$went_for_it == 1], 
       col = "darkgreen", 
       pch = 19, 
       cex = 0.7  # Point size
) 

points(fourth_downs_first$ydstogo[fourth_downs_first$went_for_it == 0], 
       fourth_downs_first$went_for_it[fourth_downs_first$went_for_it == 0], 
       col = "darkred", 
       pch = 19, 
       cex = 0.7)

# Optional: Add a legend to indicate the color coding
legend("topright", legend = c("Went for it", "Did not go for it"), 
       col = c("darkgreen", "darkred"), pch = 19, bty = "n")


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



######### Wihtout ggplot Bar Chart ######
team_names <- team_fourth_first$posteam  # Get team names
actual_fourths <- team_fourth_first$actual_fourths
exp_fourths <- team_fourth_first$exp_fourths

# Combine the actual and expected fourth downs into a matrix for the bar plot
bar_data <- rbind(actual_fourths, exp_fourths)

# Create the bar chart
barplot(
  bar_data,
  beside = TRUE,                          # Place bars side by side
  names.arg = team_names,                 # Use team names for the x-axis
  col = c("skyblue", "lightgreen"),       # Set colors for the two categories
  legend.text = c("Actual 4th Downs", "Expected 4th Downs"),
  args.legend = list(x = "topright"),     # Position the legend
  main = "Team 4th Down Actual vs Expected Go's (1999-2010)",  # Main title
  xlab = "Teams",                         # X-axis label
  ylab = "Number of 4th Downs",           # Y-axis label
  las = 2,                                # Rotate x-axis labels for readability
  ylim = c(0, max(bar_data) + 5)          # Adjust y-axis limits to fit the data
)

# Add gridlines for visual clarity
abline(h = seq(0, max(bar_data), by = 5), col = "gray", lty = "dotted")



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


