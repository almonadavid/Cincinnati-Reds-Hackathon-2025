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

pitcher_process_year_data <- function(data, year) {
  savant_update <- data %>%
    filter(game_year == year) %>%
    left_join(lahman_people, by = c("pitcher" = "player_mlb_id")) %>%
    mutate(
      birthDate = mdy(birthDate),
      age = year - year(birthDate),
      debutDate = mdy(debut),
      years_since_debut = year - year(debutDate)
    )
  
  ## PITCHER DATA ####
  pitcher_data <- savant_update %>%
    group_by(pitcher) %>%
    summarise(
      current_BF = n_distinct(paste(game_pk, batter, times_faced)),
      games_played = n_distinct(game_pk),
      BF_per_game = round((current_BF/games_played), 2),
      pitch_count = n(),
      pitch_count_per_game = round((pitch_count/games_played), 2),
      age = first(age),
      years_since_debut = first(years_since_debut),
      .groups = "drop"
    )
  
  ## HEIGHT ####
  pitcher_height <- savant_update %>%
    select(pitcher, height) %>%
    distinct()
  
  ## POST BATTERS FACED ####
  next_year <- year + 1
  post_batters_faced <- data %>%
    filter(game_year == next_year) %>%
    group_by(pitcher) %>%
    summarise(
      post_BF = round(n_distinct(paste(game_pk, batter, times_faced)),2),
      .groups = "drop"
    )
  
  ## PERCENTAGE PITCH TYPES ####
  pct_pitch_type <- savant_update %>%
    group_by(pitcher, pitch_type) %>%
    summarise(count = n(), 
              .groups = "drop") %>%
    group_by(pitcher) %>%
    mutate(
      total_pitches = sum(count),
      pct = (count / total_pitches) * 100
    ) %>%
    ungroup()
  
  ## TOP 3 PITCHES ####
  ranked_pitch_types <- pct_pitch_type %>%
    group_by(pitcher) %>%
    arrange(desc(pct), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    filter(rank <= 3) %>%
    select(pitcher, pitch_type, rank) %>%
    pivot_wider(
      names_from = rank,
      values_from = pitch_type,
      names_glue = "top{rank}_pitch"
    )
  
  top3_pitch_type <- ranked_pitch_types %>%
    group_by(pitcher) %>%
    summarise(
      top3_FF = ifelse("FF" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_SL = ifelse("SL" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_SI = ifelse("SI" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_CH = ifelse("CH" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_CU = ifelse("CU" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_FC = ifelse("FC" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_ST = ifelse("ST" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_KC = ifelse("KC" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      top3_FS = ifelse("FS" %in% c(top1_pitch, top2_pitch, top3_pitch), 1, 0),
      .groups = "drop"
    )
  
  ## EACH PITCH STAT ####
  each_pitch_stat <- savant_update %>%
    group_by(pitcher, pitch_type) %>%
    summarise(
      avg_release_speed = mean(release_speed, na.rm = TRUE),
      avg_effective_speed = mean(effective_speed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(pitch_type %in% c("FF", "SI", "SL")) %>%
    mutate(across(
      c(avg_release_speed, avg_effective_speed),
      ~ round(., 1)
    )) %>%
    pivot_wider(
      names_from = pitch_type,
      values_from = c(avg_release_speed, avg_effective_speed),
      names_sep = "_"
    ) %>%
    select(pitcher, avg_release_speed_FF)
  
  ## BREAKING BALLS ####
  breakingball_per_pitcher <- savant_update %>%
    group_by(pitcher) %>%
    summarise(
      SP_prob = round(mean(sp_indicator, na.rm = TRUE), 2),
      total_pitches = n(),
      total_breakingball = sum(pitch_name %in% c("Curveball", "Knuckle Curve", "Slurve",
                                                 "Slider", "Sweeper"), na.rm = TRUE),
      breakingball_pct = round((total_breakingball / total_pitches) * 100, 2),
      .groups = "drop"
    ) %>%
    select(pitcher, breakingball_pct)
  
  ## WHIP ####
  savant_whip <- savant_update %>%
    group_by(pitcher) %>%
    summarise(
      total_hits = sum(events %in% c("single", "double", "triple", "home_run"), na.rm = TRUE),
      total_walks = sum(events == "walk", na.rm = TRUE),
      total_outs = sum(events %in% c(
        "strikeout", "field_out", "force_out", "sac_fly",
        "fielders_choice_out", "other_out"), na.rm = TRUE) +
        2 * sum(events %in% c(
          "grounded_into_double_play", "double_play", "sac_fly_double_play",
          "strikeout_double_play", "sac_bunt_double_play"), na.rm = TRUE) +
        3 * sum(events == "triple_play", na.rm = TRUE),
      innings_pitched = round(total_outs / 3, 2),
      whip = round((total_hits + total_walks) / innings_pitched, 2),
      .groups = "drop"
    ) %>%
    filter(is.finite(whip))
  
  ## K-RATE ####
  savant_krate <- savant_update %>%
    group_by(pitcher) %>%
    summarise(
      total_strikeout = sum(events %in% c("strikeout", "strikeout_play"), na.rm = TRUE),
      BF = n_distinct(paste(batter, times_faced)),
      krate = round(total_strikeout / BF, 2),
      .groups = "drop"
    ) %>%
    select(pitcher, krate)
  
  ## GAMES PER MONTH ####
  num_games_per_month <- savant_update %>%
    mutate(month = month(game_date)) %>%
    group_by(pitcher, month) %>%
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
    select(pitcher, Mean_Games, zero_months)
  
  
  ## PITCHES PER MONTH ####
  pitches_per_month <- savant_update %>%
    mutate(month = month(game_date)) %>%
    group_by(pitcher, month) %>%
    summarise(Pitches = n(), 
              .groups = "drop") %>%
    pivot_wider(
      names_from = month,
      values_from = Pitches,
      names_prefix = "Pitches_",
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(
      Mean_Pitches = round(mean(c_across(starts_with("Pitches_")), na.rm = TRUE), 2)
    ) %>%
    ungroup() %>%
    select(pitcher, Mean_Pitches)
  
  ## REST BETWEEN GAMES ####
  rest <- savant_update %>%
    distinct(game_year, pitcher, game_date, game_pk, .keep_all = FALSE) %>%
    arrange(pitcher, game_date) %>%
    group_by(pitcher) %>%
    mutate(
      rest_days = as.numeric(difftime(game_date, dplyr::lag(game_date), units = "days")),
      .groups = "drop"
    ) %>%
    summarise(
      Mean_rest = round(mean(rest_days, na.rm = TRUE),2),
      .groups = "drop"
    )
  
  ## PITCHER HAND ####
  pitcher_hand <- savant_update %>%
    select(pitcher, right_throws) %>%
    distinct()
  
  ## NUMBER OF PITCH TYPES IN ARSENAL ####
  pitcher_arsenal <- savant_update %>%
    filter(!pitch_name %in% c("Other", "Pitch Out", "Eephus", "Knuckleball",
                              "Screwball", "Forkball", "Slow Curve")) %>%
    group_by(pitcher) %>%
    summarise(
      pitch_arsenal = n_distinct(pitch_type, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(pitcher, pitch_arsenal)
  
  ## PICKOFFS ####
  pickoff_events <- c("pickoff_1b", "pickoff_2b", "pickoff_3b", 
                      "pickoff_caught_stealing_2b", "pickoff_caught_stealing_3b", 
                      "pickoff_caught_stealing_home", "pickoff_error_3b")
  
  pickoff_data <- savant_update %>%
    group_by(pitcher) %>%
    summarise(
      pickoff_count = sum(events %in% pickoff_events),
      .groups = "drop")
  
  
  ## ERA ####
  era_data <- savant_update %>% 
    group_by(pitcher) %>% 
    summarise(
      total_outs = sum(events %in% c(
        "strikeout", "field_out", "force_out", "sac_fly",
        "fielders_choice_out", "other_out"), na.rm = TRUE) +
        2 * sum(events %in% c(
          "grounded_into_double_play", "double_play", "sac_fly_double_play",
          "strikeout_double_play", "sac_bunt_double_play"), na.rm = TRUE) +
        3 * sum(events == "triple_play", na.rm = TRUE),
      innings_pitched = round(total_outs / 3, 2),
      earned_runs = sum(events %in% c("single", "double", "triple", "home_run",
                                      "walk", "hit_by_pitch", "sac_fly", "sac_bunt",
                                      "stolen_base_2b", "stolen_base_3b", "wild_pitch")),
      ERA = round((earned_runs / innings_pitched) * 9,2),
      .groups = "drop"
    ) %>% 
    mutate(
      ERA = ifelse(is.infinite(ERA), min(ERA[is.finite(ERA)], na.rm = TRUE), ERA),
      ERA = ifelse(is.na(ERA), min(ERA[!is.na(ERA)], na.rm = TRUE), ERA)
    ) %>% 
    select(pitcher,ERA)
  
  
  ## FIP ####
  fip_data <- savant_update %>% 
    group_by(pitcher) %>% 
    summarise(
      HR = sum(events %in% "home_run", na.rm = TRUE),
      HBP = sum(events %in% "hit_by_pitch", na.rm = TRUE),
      BB = sum(events %in% "walk", na.rm = TRUE),
      K = sum(events %in% c("strikeout", "strikeout_play"), na.rm = TRUE), 
      total_outs = sum(events %in% c(
        "strikeout", "field_out", "force_out", "sac_fly",
        "fielders_choice_out", "other_out"), na.rm = TRUE) +
        2 * sum(events %in% c(
          "grounded_into_double_play", "double_play", "sac_fly_double_play",
          "strikeout_double_play", "sac_bunt_double_play"), na.rm = TRUE) +
        3 * sum(events == "triple_play", na.rm = TRUE),
      IP = round(total_outs / 3, 2),
      FIP = round((((13*HR)+(3*(HBP+BB))-(2*K))/IP) + 3.2,2),
      .groups = "drop"
    ) %>% 
    mutate(
      FIP = ifelse(is.infinite(FIP), min(FIP[is.finite(FIP)], na.rm = TRUE), FIP),  
      FIP = ifelse(is.na(FIP)|FIP<0, min(FIP[!is.na(FIP) & FIP >= 0], na.rm = TRUE), FIP)
    ) %>% 
    select(pitcher,FIP)
  
  ## LINE DRIVE PERCENTAGE ####
  pct_line_drive <- savant_update %>% 
    group_by(pitcher) %>% 
    summarise(
      in_play = sum(type == "X", na.rm = TRUE),
      line_drive = sum(bb_type == "line_drive", na.rm = TRUE),
      pct_LD = round((line_drive/in_play)*100,2),
      .groups = "drop"
    ) %>% 
    mutate(
      pct_LD = ifelse(is.na(pct_LD), min(pct_LD[!is.na(pct_LD)], na.rm = TRUE), pct_LD)
    ) %>% 
    select(pitcher,pct_LD)
  
  ## SIERA ####
  siera_data <- savant_update %>% 
    group_by(pitcher) %>% 
    summarise(
      SO = sum(events %in% c("strikeout", "strikeout_play"), na.rm = TRUE),
      BF = n_distinct(paste(game_pk, batter, times_faced)),
      BB = sum(events == "walk", na.rm = TRUE),
      GB = sum(bb_type %in% "ground_ball", na.rm = TRUE),
      FB = sum(bb_type %in% "fly_ball", na.rm = TRUE),
      PU = sum(bb_type %in% "popup", na.rm = TRUE),
      SIERA = round(ifelse(GB >= FB, 
                     6.145 - 16.986*(SO/BF) + 11.434*(BB/BF) - 1.858*((GB-FB-PU)/BF) + 7.653*((SO/BF)^2) 
                     - 6.664*(((GB-FB-PU)/BF)^2) + 10.130*(SO/BF)*((GB-FB-PU)/BF) - 5.195*(BB/BF)*((GB-FB-PU)/BF), 
                     6.145 - 16.986*(SO/BF) + 11.434*(BB/BF) - 1.858*((GB-FB-PU)/BF) + 7.653*((SO/BF)^2) 
                     + 6.664*(((GB-FB-PU)/BF)^2) + 10.130*(SO/BF)*((GB-FB-PU)/BF) - 5.195*(BB/BF)*((GB-FB-PU)/BF)),2), #formula from MLB website
      .groups = "drop"
    ) %>% 
    mutate(
      SIERA = ifelse(is.na(SIERA)|SIERA<0, min(SIERA[!is.na(SIERA) & SIERA >= 0], na.rm = TRUE), SIERA)
    ) %>% 
    select(pitcher,SIERA)
    
  
  ## FINAL DATASET ####
  final_dataset <- pitcher_arsenal %>%
    left_join(pitcher_data, by = "pitcher") %>% 
    left_join(post_batters_faced, by = "pitcher") %>%
    left_join(each_pitch_stat, by = "pitcher") %>%
    left_join(top3_pitch_type, by = "pitcher") %>%
    left_join(rest, by = "pitcher") %>%
    left_join(breakingball_per_pitcher, by = "pitcher") %>%
    left_join(savant_whip, by = "pitcher") %>%
    left_join(savant_krate, by = "pitcher") %>%
    left_join(num_games_per_month, by = "pitcher") %>%
    left_join(pitches_per_month, by = "pitcher") %>%
    left_join(pitcher_hand, by = "pitcher") %>%
    left_join(pickoff_data, by = "pitcher") %>% 
    left_join(siera_data, by = "pitcher") %>% 
    left_join(fip_data, by = "pitcher") %>% 
    left_join(era_data, by = "pitcher") %>% 
    left_join(pct_line_drive, by = "pitcher")
  
  ## REPLACE NA'S WITH MEAN ####
  final_dataset <- final_dataset %>%
    mutate(
      across(where(is.numeric) & !post_BF, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))
      )
  
  final_dataset <- final_dataset %>%
    mutate(year = year) %>% 
    select(pitcher, year, post_BF, everything())
  
  return(final_dataset)
}

# PROCESS DATA FOR EACH YEAR ####
years <- unique(savant_data_2021_2023$game_year)
p_panel_data <- lapply(years, function(year) {
  pitcher_process_year_data(savant_data_2021_2023, year)
})

## COMBINE ALL YEARS ####
pitcher_panel_data <- bind_rows(p_panel_data)


## TEST CORRELATION BETWEEN VARIABLES TO AVOID COLLINEARITY ####
p_cor_matrix <- cor(pitcher_panel_data %>% 
                      select_if(is.numeric), use = "pairwise.complete.obs")
p_cor_matrix <- ifelse(abs(p_cor_matrix) < 0.7, "x", round(p_cor_matrix, 2))

head(p_cor_matrix)

## PLOTS TO CHECK RELATIONSHIP ####
ggplot(pitcher_panel_data, aes(x = SIERA, y = post_BF)) + 
  geom_point(na.rm = TRUE) + 
  geom_smooth(method = "loess", na.rm = TRUE)


## TEST DIFFERENT VARIABLE MODIFICATIONS ####
lin_test_model <- lm(post_BF ~ SIERA, 
                     data = pitcher_panel_data)

poly2_test_model <- lm(post_BF ~ poly(SIERA,2), 
                       data = pitcher_panel_data)

poly3_test_model <- lm(post_BF ~ poly(SIERA,3), 
                       data = pitcher_panel_data)

log_test_model <- lm(post_BF ~ log(SIERA + 0.001), 
                     data = pitcher_panel_data)

sqrt_test_model <- lm(post_BF ~ sqrt(SIERA), 
                      data = pitcher_panel_data)

inv_test_model <- lm(post_BF ~ I(1 / (SIERA + 0.001)), 
                     data = pitcher_panel_data)

exp_test_model <- lm(post_BF ~ exp(SIERA),
                     data = pitcher_panel_data)

AIC(lin_test_model, poly2_test_model, poly3_test_model, log_test_model,sqrt_test_model, inv_test_model, exp_test_model)
BIC(lin_test_model, poly2_test_model, poly3_test_model, log_test_model, sqrt_test_model, inv_test_model, exp_test_model)


## ELASTIC NET REG. ####
filtered_pitcher_data <- pitcher_panel_data %>% 
  filter(!is.na(post_BF))

p_training.samples <- filtered_pitcher_data$post_BF %>%
  createDataPartition(p = 0.8, list = FALSE)

p_train.data  <- filtered_pitcher_data[p_training.samples, ]
p_test.data <- filtered_pitcher_data[-p_training.samples, ]


set.seed(123)
p_elastic <- train(
  post_BF ~ pitch_arsenal + poly(games_played,3) + poly(BF_per_game,3) 
  + poly(pitch_count,2) + years_since_debut + avg_release_speed_FF
  + top3_FF + top3_SL + top3_SI + top3_CH + top3_CU + top3_FC + top3_ST 
  + top3_KC + top3_FS + I(1 / (Mean_rest + 0.001)) + poly(breakingball_pct,2)
  + poly(whip,2) + right_throws + pickoff_count + poly(SIERA,3) + poly(FIP,2)
  + poly(pct_LD,3),
  data = p_train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
  ) 


## COEFFICIENTS ####
coef(p_elastic$finalModel, p_elastic$bestTune$lambda)

## MODEL PREDICTIONS ####
p_predictions <- p_elastic %>% predict(p_test.data)

## CHECKING RMSE AND RSQUARE OF MODEL PREDICTIONS ####
data.frame(
  RMSE = RMSE(p_predictions, p_test.data$post_BF),
  Rsquare = R2(p_predictions, p_test.data$post_BF)
)

## COMPARE ACTUAL VS. PREDICTED ####
p_results <- data.frame(
  Actual = p_test.data$post_BF,
  Predicted = p_predictions)

head(p_results)


## 2024 PREDICTIONS ####
pitcher_2023_data <- pitcher_panel_data %>% 
  filter(is.na(post_BF))

pitcher_2023_data$predicted_post_BF <- predict(p_elastic, pitcher_2023_data)
pitcher_2024_predictions <-pitcher_2023_data %>% 
  select(pitcher, predicted_post_BF) %>%  
  mutate(
    predicted_post_BF = round(ifelse(predicted_post_BF<0, 1, predicted_post_BF),0) # Replacing negative values with 1
  )

head(pitcher_2024_predictions)