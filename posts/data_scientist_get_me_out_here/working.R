library(bushtucker)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Basic contestant stats
contestants_clean <- contestants |>
  mutate(
    occupation = case_when(
      str_detect(famous_for, regex("sing|music|record|rapper|bassist|vocal|guitar|opera|x factor", ignore_case = TRUE)) ~ "music",
      str_detect(famous_for, regex("football|rugby|cricket|sports|boxer|athlet|olympic|tennis|snooker|darts|javelin|jockey", ignore_case = TRUE)) ~ "sports",
      str_detect(famous_for, regex("made in chelsea|only way is|shore|love island", ignore_case = TRUE)) ~ "reality",
      str_detect(famous_for, regex("actor|actress|soap|film", ignore_case = TRUE)) ~ "acting",
      str_detect(famous_for, regex("politic|aristoc|socialite", ignore_case = TRUE)) ~ "politics",
      str_detect(famous_for, regex("radio", ignore_case = TRUE)) ~ "radio",
      str_detect(famous_for, regex("model|miss ", ignore_case = TRUE)) ~ "model",
      str_detect(famous_for, regex("chef", ignore_case = TRUE)) ~ "chef",
      str_detect(famous_for, regex("danc|choreo", ignore_case = TRUE)) ~ "dance",
      str_detect(famous_for, regex("comed", ignore_case = TRUE)) ~ "comedian",
      str_detect(famous_for, regex("journal|author|column", ignore_case = TRUE)) ~ "writing",
      str_detect(famous_for, regex("interior|fashion", ignore_case = TRUE)) ~ "designer",
      str_detect(famous_for, regex("presenter|newsreader", ignore_case = TRUE)) ~ "presenter",
      str_detect(famous_for, regex("youtube|social", ignore_case = TRUE)) ~ "social_media",
      .default = "other"
    ),

    occupation_group = case_when(
      occupation == "sports" ~ "sports",
      occupation %in% c("acting", "presenter") ~ "tv_film",
      occupation %in% c("radio", "comedian", "music", "dance") ~ "trad_entertainment",
      occupation %in% c("reality", "model", "social_media") ~ "mod_entertainment",
      occupation %in% c("politics", "writing", "designer", "chef") ~ "politics_industry",
      .default = occupation
    )
  )

contestants_clean |>
  select(season, occupation_group) |>
  ggplot() +
  geom_bar(aes(x = season, fill = occupation_group), position = "fill")











longer <- contestants_clean |>
  transmute(season, occupation_group) |>
  pivot_wider(names_from = occupation_group) |>
  group_by(season) |>
  summarise(across(-first_name, \(x) sum(x, na.rm = TRUE))) |>
  pivot_longer(-season) |>
