## START ####
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(caret)
library(glmnet)


## LOAD DATA ####
lahman_people <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/Red Hackathon 2025/lahman_people.csv")
savant_data_2021_2023 <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/Red Hackathon 2025/savant_data_2021_2023.csv")

savant_data_2021_2023 <- savant_data_2021_2023 %>%
  mutate(
    right_throws = ifelse(p_throws == "R", 1, 0),
    left_throws = ifelse(p_throws == "L", 1, 0),
    right_stand = ifelse(stand == "R", 1, 0),
    left_stand = ifelse(stand == "L", 1, 0)
  )


batter_process_year_data <- function(data, year) {
  savant_update <- data %>%
    filter(game_year == year) %>%
    left_join(lahman_people, by = c("batter" = "player_mlb_id")) %>%
    mutate(
      birthDate = mdy(birthDate),
      age = year - year(birthDate),
      debutDate = mdy(debut),
      years_since_debut = year - year(debutDate)
    )
  
  ## POST PLATE APPEARANCE ####
  post_plate_appearance <- data %>%
    filter(game_year == year + 1) %>%
    group_by(batter) %>%
    summarise(
      post_PA = n_distinct(paste(game_pk, at_bat_number)),
      .groups = "drop"
    )
  
  ## GAMES PER MONTH ####
  batter_num_games_per_month <- savant_update %>%
    mutate(month = month(game_date)) %>%
    group_by(batter, month) %>%
    summarise(Games = n_distinct(game_pk), 
              .groups = "drop") %>%
    pivot_wider(
      names_from = month, 
      values_from = Games, 
      names_prefix = "Games_", 
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(
      Mean_Games = round(mean(c_across(starts_with("Games_")), na.rm = TRUE), 2),
      SD_Games = round(sd(c_across(starts_with("Games_")), na.rm = TRUE), 2),
      zero_months = sum(c_across(starts_with("Games_")) == 0)
    ) %>%
    ungroup() %>%
    select(batter, Mean_Games, zero_months)
  
  ## AGE AND EXPERIENCE ####
  batter_age_experience <- savant_update %>% 
    select(batter, age, years_since_debut) %>% 
    distinct()
  
  ## BIRTH COUNTRY ####
  birth_country <- savant_update %>% 
    select(batter, birthCountry) %>% 
    distinct()
  
  ## BATTER STATS ####
  batter_stats <- savant_update %>%
    group_by(batter) %>%
    summarise(
      current_PA = n_distinct(paste(game_pk, at_bat_number)),
      games_played = n_distinct(game_pk),
      PA_per_game = round((current_PA/games_played), 2),
      avg_launch_speed = mean(launch_speed, na.rm = TRUE),
      avg_xBA = mean(estimated_ba_using_speedangle, na.rm = TRUE),
      avg_xWOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
      sin_launch_angle = mean(sin(launch_angle), na.rm = TRUE),
      cos_launch_angle = mean(cos(launch_angle), na.rm = TRUE),
      BB = sum(events == "walk", na.rm = TRUE),
      K = sum(events %in% c("strikeout", "strikeout_double_play"), na.rm = TRUE),
      HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
      SF = sum(events == "sac_fly" | events == "sac_fly_double_play", na.rm = TRUE),
      .groups = "drop"
    )
  
  
  ## RUNS AND RBI ####
  runs_rbi_data <- savant_update %>%
    arrange(batter, game_pk, bat_score) %>%
    mutate(
      runs_scored = post_bat_score - bat_score,
      rbi = case_when(
        events == "home_run" ~ 1 + (!is.na(on_3b) + !is.na(on_2b) + !is.na(on_1b)),
        events %in% c("single", "double", "triple") & (runs_scored > 0) ~ runs_scored,
        events == "sac_fly" & runs_scored > 0 ~ 1,
        events == "sac_bunt" & runs_scored > 0 ~ 1,
        events %in% c("walk", "hit_by_pitch") & (!is.na(on_3b)) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    group_by(batter) %>%
    summarise(
      total_runs = sum(runs_scored, na.rm = TRUE),
      total_rbi = sum(rbi, na.rm = TRUE)
    )
  
  ## HITS AND AT BATS AGAINST ####
  hit_events <- c("single", "double", "triple", "home_run")
  error_events <- c("field_error", "fielders_choice", "catcher_interf")
  at_bat_events <- c("strikeout", "field_out", "single", "double", "triple",
                     "home_run", "grounded_into_double_play", "force_out",
                     "field_error", "strikeout_double_play", "fielders_choice",
                     "fielders_choice_out", "double_play", "triple_play")
  
  hits_data <- savant_update %>%
    mutate(
      hit = ifelse(events %in% hit_events & !events %in% error_events, 1, 0),
      at_bat = ifelse(events %in% at_bat_events, 1, 0),
      total_bases = case_when(
        events == "single" ~ 1,
        events == "double" ~ 2,
        events == "triple" ~ 3,
        events == "home_run" ~ 4,
        TRUE ~ 0
      )
    ) %>%
    group_by(batter) %>%
    summarise(
      H = sum(hit, na.rm = TRUE),
      AB = sum(at_bat, na.rm = TRUE),
      TB = sum(total_bases, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## BATTER HEIGHT AND HANDEDNESS ####
  batter_height_hand <- savant_update %>%
    group_by(batter) %>%
    reframe(
      height = height,
      right_hand = ifelse(bats == 'R', 1, 0),
      both_hand = ifelse(bats == 'B', 1, 0)
    ) %>%
    distinct()
  
  ## BATTER PICKEDOFF WHILE ON BASE ####
  pickedoff_events <- c("pickoff_1b", "pickoff_2b", "pickoff_3b", 
                        "pickoff_caught_stealing_2b", "pickoff_caught_stealing_3b", 
                        "pickoff_caught_stealing_home", "pickoff_error_3b")
  
  pickedoff_data <- savant_update %>%
    group_by(batter) %>%
    summarise(
      pickoff_count = sum(events %in% pickedoff_events),
      .groups = "drop")
  
  ## HITS AGAINST PITCH TYPES ####
  hit_vs_pitch <- savant_update %>% 
    group_by(batter) %>% 
    summarise(
      current_PA = n_distinct(paste(game_pk, at_bat_number)),
      hits_count = sum(events %in% c("single", "double", "triple", "home_run")),
      fastball_hits = sum((pitch_type == "FF") & (events %in% c("single", 
                                                                "double", "triple", "home_run"))),
      breakingball_hits = sum((pitch_name %in% c("Curveball", "Knuckle Curve", 
                                                 "Slurve", "Slider", "Sweeper")) &
                                (events %in% c("single", "double", "triple", 
                                               "home_run"))),
      pct_FF_hits = round((fastball_hits/hits_count)*100, 2),
      pct_breakingball_hits = round((breakingball_hits/hits_count)*100, 2),
      .groups = "drop"
    ) %>% 
    mutate(
      pct_FF_hits = replace_na(pct_FF_hits, 0),
      pct_breakingball_hits = replace_na(pct_breakingball_hits, 0)
    ) %>% 
    select(batter, pct_FF_hits, pct_breakingball_hits)
  
  ## LINE DRIVE PERCENTAGE ####
  pct_line_drive <- savant_update %>% 
    group_by(batter) %>% 
    summarise(
      in_play = sum(type == "X", na.rm = TRUE),
      line_drive = sum(bb_type == "line_drive", na.rm = TRUE),
      pct_LD = round((line_drive/in_play)*100,2),
      .groups = "drop"
    ) %>% 
    mutate(
      pct_LD = ifelse(is.na(pct_LD), min(pct_LD[!is.na(pct_LD)], na.rm = TRUE), pct_LD)
    ) %>% 
    select(batter,pct_LD)
  
  
  ## FINAL DATASET ####
  final_dataset <- batter_stats %>%
    left_join(runs_rbi_data, by = "batter") %>%
    left_join(hits_data, by = "batter") %>%
    left_join(post_plate_appearance, by = "batter") %>%
    left_join(batter_num_games_per_month, by = "batter") %>%
    left_join(batter_age_experience, by = "batter") %>%
    left_join(batter_height_hand, by = "batter") %>%
    left_join(pickedoff_data, by = "batter") %>% 
    left_join(hit_vs_pitch, by = "batter") %>% 
    left_join(pct_line_drive, by = "batter") %>% 
    mutate(
      BA = round(H / AB, 3),
      OBP = round((H + BB + HBP) / (AB + BB + HBP + SF), 3),
      SLG = round(TB / AB, 3),
      OPS = round(OBP + SLG, 3),
      BB_rate = round(BB / current_PA, 3),
      K_rate = round(K / current_PA, 3),
      runs_per_PA = round(total_runs/current_PA,3),
      RBI_per_PA = round(total_rbi/current_PA,3)
    )
  
  final_dataset <- final_dataset %>%
    mutate(year = year) %>% 
    select(batter, year, post_PA, everything())
  
  
  ## REPLACE NA'S WITH MEAN ####
  final_dataset <- final_dataset %>%
    mutate(
      across(where(is.numeric) & !post_PA, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))
    )
  
  return(final_dataset)
}

years <- unique(savant_data_2021_2023$game_year)
b_panel_data <- lapply(years, function(year) {
  batter_process_year_data(savant_data_2021_2023, year)
})

## COMBINE ALL YEARS ####
batter_panel_data <- bind_rows(b_panel_data)


## PLOTS TO CHECK RELATIONSHIP ####
ggplot(batter_panel_data, aes(x = games_played, y = post_PA)) + 
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "loess", na.rm = TRUE)

## TEST DIFFERENT VARIABLE MODIFICATIONS ####
lin_test_model <- lm(post_PA ~ runs_per_PA, 
                 data = batter_panel_data)

poly2_test_model <- lm(post_PA ~ poly(runs_per_PA,2), 
                   data = batter_panel_data)

poly3_test_model <- lm(post_PA ~ poly(runs_per_PA,3), 
                       data = batter_panel_data)

log_test_model <- lm(post_PA ~ log(runs_per_PA + 0.001), 
                 data = batter_panel_data)

sqrt_test_model <- lm(post_PA ~ sqrt(runs_per_PA), 
                     data = batter_panel_data)

inv_test_model <- lm(post_PA ~ I(1 / (runs_per_PA + 0.001)), 
                     data = batter_panel_data)

exp_test_model <- lm(post_PA ~ exp(runs_per_PA),
                     data = batter_panel_data)

AIC(lin_test_model, poly2_test_model, poly3_test_model, log_test_model,sqrt_test_model, inv_test_model, exp_test_model)
BIC(lin_test_model, poly2_test_model, poly3_test_model, log_test_model, sqrt_test_model, inv_test_model, exp_test_model)


## CHECK CORRELATION BETWEEN VARIABLES TO AVOID COLLINEARITY ####
b_cor_matrix <- cor(batter_panel_data %>% 
                      select_if(is.numeric), use = "pairwise.complete.obs")
b_cor_matrix <- ifelse(abs(b_cor_matrix) < 0.7, "x", round(b_cor_matrix, 2))

head(b_cor_matrix)


## ELASTIC NET REG. ####
filtered_batter_data <- batter_panel_data %>% 
  filter(!is.na(post_PA))

b_training.samples <- filtered_batter_data$post_PA %>%
  createDataPartition(p = 0.8, list = FALSE)

b_train.data  <- filtered_batter_data[b_training.samples, ]
b_test.data <- filtered_batter_data[-b_training.samples, ]

set.seed(123)
b_elastic <- train(
  post_PA ~ exp(PA_per_game) + poly(pct_FF_hits,2)
  + poly(pct_breakingball_hits,2) + poly(avg_launch_speed,3) + poly(pct_LD,3)
  + I(1 / (BB_rate + 0.001)) + poly(K_rate,3) + poly(OBP,2) + poly(games_played,3) 
  + poly(years_since_debut,2) + I(1 / (height + 0.001)) + sqrt(HBP)
  + I(1 / (pickoff_count + 0.001)) + right_hand + both_hand + poly(runs_per_PA,3),
  data = b_train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

## COEFFICIENTS ####
coef(b_elastic$finalModel, b_elastic$bestTune$lambda)

## MODEL PREDICTIONS ####
b_predictions <- b_elastic %>% predict(b_test.data)

## CHECKING RMSE AND RSQUARE OF MODEL PREDICTIONS ####
data.frame(
  RMSE = RMSE(b_predictions, b_test.data$post_PA),
  Rsquare = R2(b_predictions, b_test.data$post_PA)
)

## COMPARE ACTUAL VS. PREDICTED ####
b_results <- data.frame(
  Actual = b_test.data$post_PA,
  Predicted = b_predictions)

head(b_results)


## 2024 PREDICTIONS ####
batter_2023_data <- batter_panel_data %>% 
  filter(is.na(post_PA))

batter_2023_data$predicted_post_PA <- predict(b_elastic, batter_2023_data)
batter_2024_predictions <-batter_2023_data %>% 
  select(batter, predicted_post_PA) %>% 
  mutate(
    predicted_post_PA = round(ifelse(predicted_post_PA < 0, 1, predicted_post_PA),0) # Replacing negative values with 1
  )

head(batter_2024_predictions)