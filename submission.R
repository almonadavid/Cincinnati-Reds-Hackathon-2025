
sample_submission <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/Red Hackathon 2025/sample_submission.csv")

combined_prediction <- pitcher_2024_predictions %>%
  rename(
    PLAYER_ID = pitcher,
    PLAYING_TIME = predicted_post_BF
  ) %>%
  bind_rows(
    batter_2024_predictions %>%
      rename(
        PLAYER_ID = batter,
        PLAYING_TIME = predicted_post_PA
      )
  ) %>%
  group_by(PLAYER_ID) %>%
  summarise(
    PLAYING_TIME = sum(PLAYING_TIME),
    .groups = "drop"
  )

submission <- sample_submission %>%
  dplyr::select(-PLAYING_TIME) %>%
  left_join(
    combined_prediction,
    by = 'PLAYER_ID'
  )

summary(submission)
