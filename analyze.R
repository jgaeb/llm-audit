library(groundhog)
groundhog.library("
  lme4
  lfe
  broom.mixed
  ggh4x
  ggstance
  xtable
  glmnet
  fs
  glue
  DBI
  RSQLite
  scales
  dbplyr
  broom
  tidymodels
  tidyverse
", "2024-07-15")

theme_set(theme_bw())

set.seed(7140466)

MODEL_TRANSLATOR <- c(
  "gpt-3.5-turbo-0125" = "GPT-3.5",
  "gpt-4-0125-preview" = "GPT-4",
  "gpt-4o-mini-2024-07-18" = "GPT-4o Mini",
  "gpt-4o-2024-05-13" = "GPT-4o",
  "mistral.mistral-7b-instruct-v0:2" = "Mistral 7B",
  "mistral.mixtral-8x7b-instruct-v0:1" = "Mixtral 8x7B",
  "anthropic.claude-v2:1" = "Claude 2",
  "anthropic.claude-instant-v1" = "Claude Instant",
  "anthropic.claude-3-haiku-20240307-v1:0" = "Claude 3 Haiku",
  "anthropic.claude-3-sonnet-20240229-v1:0" = "Claude 3 Sonnet",
  "anthropic.claude-3-5-sonnet-20240620-v1:0" = "Claude 3.5 Sonnet"
)

FRAMING_TRANSLATOR <- c(
  "base" = "Base",
  "no_scratch" = "No Scratch",
  "no_transcripts" = "No Transcripts",
  "other_district" = "Other District",
  "eeoc_guidance" = "EEOC Guidance"
)

# Load the data from the database
con <- dbConnect(RSQLite::SQLite(), "data.db")

interviews <- tbl(con, "interviews")
personas <- tbl(con, "personas")
prompts <- tbl(con, "prompts")
requests <- tbl(con, "requests")
ratings <- tbl(con, "ratings")
checks <- tbl(con, "checks")
embeddings <- tbl(con, "embeddings")

################################################################################
# Demographic breakdown

# Total number of interviews
interviews %>%
  collect() %>%
  tally() %>%
  glue_data("There are {n} interviews in the dataset.")

# Number of interviews in study
interviews %>%
  filter(in_study == 1) %>%
  collect() %>%
  tally() %>%
  glue_data("There are {n} interviews in the study.")

glue("Race breakdown of interviews in the study:")
interviews %>%
  filter(in_study == 1) %>%
  count(race) %>%
  mutate(prop = as.double(n) / sum(n, na.rm = TRUE))

glue("Gender breakdown of interviews in the study:")
interviews %>%
  filter(in_study == 1) %>%
  count(gender) %>%
  mutate(prop = as.double(n) / sum(n, na.rm = TRUE))

################################################################################
# Manipulation checks

# Join the personas to the checks
manipulation_checks <- personas %>%
  select(interview_id, persona_id, first_name, last_name, race, gender) %>%
  left_join(
    select(prompts, prompt_id, persona_id, experiment_type),
    by = "persona_id"
  ) %>%
  filter(experiment_type == "manipulation_check") %>%
  left_join(
    select(requests, request_id, prompt_id, model),
    by = "prompt_id"
  ) %>%
  left_join(
    select(checks, request_id, inferred_race = race, inferred_gender = gender),
    by = "request_id"
  ) %>%
  # Coerce any inferred race that is not "Asian", "Black", "Hispanic", or
  # "White" to NA. Similarly for inferred gender.
  mutate(
    inferred_race = if_else(
      inferred_race %in% c("Asian", "Black", "Hispanic", "White"),
      inferred_race,
      NA_character_
    ),
    inferred_gender = if_else(
      str_to_lower(inferred_gender) %in% c("female", "male"),
      str_to_lower(inferred_gender),
      NA_character_
    )
  ) %>%
  count(model, race, inferred_race, gender, inferred_gender) %>%
  collect() %>%
  drop_na(model) %>%
  mutate(model = MODEL_TRANSLATOR[model])

# To make things more ergonomic, aggregate over race and gender
race_checks <- manipulation_checks %>%
  group_by(model, race, inferred_race) %>%
  summarize(n = sum(n), .groups = "drop")

gender_checks <- manipulation_checks %>%
  group_by(model, gender, inferred_gender) %>%
  summarize(n = sum(n), .groups = "drop")

# Plot the agreement between the inferred and actual gender
p_race_check <- race_checks %>%
  mutate(
    inferred_race = if_else(
      is.na(inferred_race),
      "Refused or\nUnparseable",
      inferred_race
    ),
    inferred_race = factor(
      inferred_race,
      levels = c("Asian", "Black", "Hispanic", "White", "Refused or\nUnparseable"),
    ),
    race = str_extract(race, "^.")
  ) %>%
  ggplot(aes(x = race, y = n, fill = inferred_race)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c(hue_pal()(4), "black")) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Intended race",
    y = "Elicited race",
    fill = "Elicited\nrace"
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  facet_wrap(vars(model), ncol = 3) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "manipulation-checks-race.pdf")),
  plot = p_race_check,
  width = 5,
  height = 5
)

p_gender_check <- gender_checks %>%
  mutate(
    inferred_gender = if_else(
      is.na(inferred_gender),
      "na",
      inferred_gender
    ),
    inferred_gender = factor(
      inferred_gender,
      levels = c("female", "male", "na"),
      labels = c("Female", "Male", "Refused or\nUnparseable")
    ),
    gender = if_else(gender == "female", "F", "M")
  ) %>%
  ggplot(aes(x = gender, y = n, fill = inferred_gender)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c(hue_pal()(2), "black")) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Intended gender",
    y = "Elicited gender",
    fill = "Elicited\ngender"
  ) +
  facet_wrap(vars(model), ncol = 3) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "manipulation-checks-gender.pdf")),
  plot = p_gender_check,
  width = 5,
  height = 5
)

################################################################################
# Ratings

# Unadjusted differences in average ratings
unadjusted_diff <- ratings %>%
  left_join(
    select(requests, request_id, prompt_id, model),
    by = "request_id"
  ) %>%
  left_join(
    select(prompts, prompt_id, interview_id, experiment_type),
    by = "prompt_id"
  ) %>%
  left_join(interviews, by = "interview_id") %>%
  filter(experiment_type %in% c("redacted", "unredacted"), error == 0) %>%
  collect()

# Get the ground truth adverse impact ratios
# NOTE: There aren't enough Asian and Other candidates to get reliable estimates
adverse_impact_race_unblinded_gt <- unadjusted_diff %>%
  filter(
    model == "gpt-3.5-turbo-0125",
    experiment_type == "unredacted",
    race %in% c("Black", "Hispanic", "White")
  ) %>%
  count(race, hire) %>%
  arrange(race, desc(hire)) %>%
  group_by(race) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = hire,
    names_from = race,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
  select(-White) %>%
  pivot_longer(
    cols = c(Black, Hispanic),
    names_to = "race",
    values_to = "adverse_impact_ratio"
  )

# Bootstrap the adverse impact ratios for race
adverse_impact_race_unblinded <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(
        model == "gpt-3.5-turbo-0125",
        experiment_type == "unredacted",
        race %in% c("Black", "Hispanic", "White")
      ) %>%
      group_by(race) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(race, hire) %>%
      arrange(race, desc(hire)) %>%
      group_by(race) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = hire,
        names_from = race,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
      select(-White) %>%
      pivot_longer(
        cols = c(Black, Hispanic),
        names_to = "race",
        values_to = "adverse_impact_ratio"
      )
  }) %>%
  list_rbind() %>%
  group_by(hire, race) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84),
    .groups = "drop"
  ) %>%
  arrange(desc(hire), race) %>%
  transmute(
    characteristic = "Race",
    value = race,
    hire = hire,
    est = adverse_impact_race_unblinded_gt$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

# Bootstrap the adverse impact ratios for gender
adverse_impact_gender_unblinded_gt <- unadjusted_diff %>%
  filter(experiment_type == "unredacted", model == "gpt-3.5-turbo-0125") %>%
  count(gender, hire) %>%
  arrange(gender, desc(hire)) %>%
  group_by(gender) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = hire,
    names_from = gender,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(female = female / male) %>%
  select(-male) %>%
  rename(adverse_impact_ratio = female)


# Bootstrap the adverse impact ratios
adverse_impact_gender_unblinded <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(experiment_type == "unredacted", model == "gpt-3.5-turbo-0125") %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(gender, hire) %>%
      arrange(gender, desc(hire)) %>%
      group_by(gender) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = hire,
        names_from = gender,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(female = female / male) %>%
      select(-male) %>%
      rename(adverse_impact_ratio = female)
  }) %>%
  list_rbind() %>%
  group_by(hire) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84),
    .groups = "drop"
  ) %>%
  arrange(desc(hire)) %>%
  transmute(
    characteristic = "Gender",
    value = "Female",
    hire = hire,
    est = adverse_impact_gender_unblinded_gt$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

p_adverse_impact_unblinded <- bind_rows(
    adverse_impact_race_unblinded,
    adverse_impact_gender_unblinded
  ) %>%
  ggplot(aes(x = hire, y = est, color = value)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 1.25, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_linerange(
    aes(ymin = CI_lwr0, ymax = CI_upr0),
    linewidth = 0.5,
    position = position_dodge(width = 0.25)
  ) +
  geom_linerange(
    aes(ymin = CI_lwr1, ymax = CI_upr1),
    linewidth = 2,
    position = position_dodge(width = 0.25)
  ) +
  scale_x_continuous(breaks = 3:5) +
  scale_y_continuous(
    limits = c(0, 1.7),
    breaks = seq(0, 2, 0.5),
    sec.axis = sec_axis(
      identity,
      name = expr({phantom() %<-% phantom()} ~ "More minority hires — More White hires" ~ {phantom() %->% phantom()})
    )
  ) +
  scale_color_manual(values = set_names(hue_pal()(3)[-3], c("Black", "Hispanic"))) +
  labs(
    title = NULL,
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More male hires — More female hires" ~ {phantom() %->% phantom()}),
    color = "Race"
  ) +
  facet_wrap(vars(characteristic)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "adverse-impact-unblinded.pdf")),
  plot = p_adverse_impact_unblinded,
  width = 6,
  height = 4
)

# Print the proportion of candidates receiving each rating
unadjusted_diff %>%
  filter(experiment_type == "unredacted") %>%
  count(hire) %>%
  mutate(prop = n / sum(n)) %>%
  glue_data("Proportion of candidates receiving hire rating {hire}: {prop}")

# Repeat the analysis for the redacted experiment
adverse_impact_race_blinded_gt <- unadjusted_diff %>%
  filter(
    model == "gpt-3.5-turbo-0125",
    experiment_type == "redacted",
    race %in% c("Black", "Hispanic", "White")
  ) %>%
  count(race, hire) %>%
  arrange(race, desc(hire)) %>%
  group_by(race) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = hire,
    names_from = race,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
  select(-White) %>%
  pivot_longer(
    cols = c(Black, Hispanic),
    names_to = "race",
    values_to = "adverse_impact_ratio"
  )

adverse_impact_race_blinded <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(
        model == "gpt-3.5-turbo-0125",
        experiment_type == "redacted",
        race %in% c("Black", "Hispanic", "White")
      ) %>%
      group_by(race) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(race, hire) %>%
      arrange(race, desc(hire)) %>%
      group_by(race) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = hire,
        names_from = race,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
      select(-White) %>%
      pivot_longer(
        cols = c(Black, Hispanic),
        names_to = "race",
        values_to = "adverse_impact_ratio"
      )
  }) %>%
  list_rbind() %>%
  group_by(hire, race) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84),
    .groups = "drop"
  ) %>%
  arrange(desc(hire), race) %>%
  transmute(
    characteristic = "Race",
    value = race,
    hire = hire,
    est = adverse_impact_race_blinded_gt$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

adverse_impact_gender_blinded_gt <- unadjusted_diff %>%
  filter(experiment_type == "redacted", model == "gpt-3.5-turbo-0125") %>%
  count(gender, hire) %>%
  arrange(gender, desc(hire)) %>%
  group_by(gender) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = hire,
    names_from = gender,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(female = female / male) %>%
  select(-male) %>%
  rename(adverse_impact_ratio = female)

adverse_impact_gender_blinded <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(experiment_type == "redacted", model == "gpt-3.5-turbo-0125") %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(gender, hire) %>%
      arrange(gender, desc(hire)) %>%
      group_by(gender) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = hire,
        names_from = gender,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(female = female / male) %>%
      select(-male) %>%
      rename(adverse_impact_ratio = female)
  }) %>%
  list_rbind() %>%
  group_by(hire) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84),
    .groups = "drop"
  ) %>%
  arrange(desc(hire)) %>%
  transmute(
    characteristic = "Gender",
    value = "Female",
    hire = hire,
    est = adverse_impact_gender_blinded_gt$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

p_adverse_impact_blinded <- bind_rows(
    adverse_impact_race_blinded,
    adverse_impact_gender_blinded
  ) %>%
  ggplot(aes(x = hire, y = est, color = value)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 1.25, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_linerange(
    aes(ymin = CI_lwr0, ymax = CI_upr0),
    linewidth = 0.5,
    position = position_dodge(width = 0.25)
  ) +
  geom_linerange(
    aes(ymin = CI_lwr1, ymax = CI_upr1),
    linewidth = 2,
    position = position_dodge(width = 0.25)
  ) +
  scale_x_continuous(breaks = 3:5) +
  scale_y_continuous(
    limits = c(0, 1.7),
    breaks = seq(0, 2, 0.5),
    sec.axis = sec_axis(
      identity,
      name = expr({phantom() %<-% phantom()} ~ "More minority hires — More White hires" ~ {phantom() %->% phantom()})
    )
  ) +
  scale_color_manual(values = set_names(hue_pal()(3)[-3], c("Black", "Hispanic"))) +
  labs(
    title = "Adverse impact ratio",
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More male hires — More female hires" ~ {phantom() %->% phantom()}),
    color = "Race"
  ) +
  facet_wrap(vars(characteristic)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "adverse-impact-blinded.pdf")),
  plot = p_adverse_impact_blinded,
  width = 6,
  height = 4
)

# Repeat both analyses across models
adverse_impact_race_unblinded_gt_models <- unadjusted_diff %>%
  filter(
    experiment_type == "unredacted",
    race %in% c("Black", "Hispanic", "White")
  ) %>%
  count(model, race, hire) %>%
  ungroup() %>%
  complete(model, race, hire = 1:5, fill = list(n = 0)) %>%
  arrange(model, race, desc(hire)) %>%
  group_by(model, race) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = c(model, hire),
    names_from = race,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
  select(-White) %>%
  pivot_longer(
    cols = c(Black, Hispanic),
    names_to = "race",
    values_to = "adverse_impact_ratio"
  )

adverse_impact_race_unblinded_models <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(
        experiment_type == "unredacted",
        race %in% c("Black", "Hispanic", "White")
      ) %>%
      group_by(model, race) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(model, race, hire) %>%
      ungroup() %>%
      complete(model, race, hire = 1:5, fill = list(n = 0)) %>%
      arrange(model, race, desc(hire)) %>%
      group_by(model, race) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = c(model, hire),
        names_from = race,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
      select(-White) %>%
      pivot_longer(
        cols = c(Black, Hispanic),
        names_to = "race",
        values_to = "adverse_impact_ratio"
      )
  }) %>%
  list_rbind() %>%
  group_by(model, hire, race) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025, na.rm = TRUE),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975, na.rm = TRUE),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16, na.rm = TRUE),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(model, desc(hire), race) %>%
  transmute(
    model = MODEL_TRANSLATOR[model],
    characteristic = "Race",
    value = race,
    hire = hire,
    est = adverse_impact_race_unblinded_gt_models$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

adverse_impact_gender_unblinded_gt_models <- unadjusted_diff %>%
  filter(experiment_type == "unredacted") %>%
  count(model, gender, hire) %>%
  arrange(model, gender, desc(hire)) %>%
  group_by(model, gender) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = c(model, hire),
    names_from = gender,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(female = female / male) %>%
  select(-male) %>%
  rename(adverse_impact_ratio = female)


# Bootstrap the adverse impact ratios
adverse_impact_gender_unblinded_models <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(experiment_type == "unredacted") %>%
      group_by(model, gender) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(model, gender, hire) %>%
      arrange(model, gender, desc(hire)) %>%
      group_by(model, gender) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = c(model, hire),
        names_from = gender,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(female = female / male) %>%
      select(-male) %>%
      rename(adverse_impact_ratio = female)
  }) %>%
  list_rbind() %>%
  group_by(model, hire) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84),
    .groups = "drop"
  ) %>%
  arrange(model, desc(hire)) %>%
  transmute(
    model = MODEL_TRANSLATOR[model],
    characteristic = "Gender",
    value = "Female",
    hire = hire,
    est = adverse_impact_gender_unblinded_gt_models$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

p_adverse_impact_unblinded_models_race <- adverse_impact_race_unblinded_models %>%
  # Make sure none of the CIs clip out of bounds
  mutate(
    CI_lwr0 = pmax(CI_lwr0, 0),
    CI_upr0 = pmax(CI_upr0, 0),
    CI_lwr1 = pmax(CI_lwr1, 0),
    CI_upr1 = pmax(CI_upr1, 0)
  ) %>%
  ggplot(aes(x = hire, y = est, color = value)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 1.25, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_linerange(
    aes(ymin = CI_lwr0, ymax = CI_upr0),
    linewidth = 0.5,
    position = position_dodge(width = 0.25)
  ) +
  geom_linerange(
    aes(ymin = CI_lwr1, ymax = CI_upr1),
    linewidth = 2,
    position = position_dodge(width = 0.25)
  ) +
  scale_x_continuous(breaks = 3:5) +
  scale_y_continuous(
    limits = c(0, 1.7),
    breaks = seq(0, 2, 0.5),
    oob = oob_keep
  ) +
  scale_color_manual(values = set_names(hue_pal()(3)[-3], c("Black", "Hispanic"))) +
  labs(
    title = "Adverse impact ratio",
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More White hires — More minority hires" ~ {phantom() %->% phantom()}),
    color = "Race"
  ) +
  facet_wrap(vars(model)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "adverse-impact-unblinded-models-race.pdf")),
  plot = p_adverse_impact_unblinded_models_race,
  width = 6,
  height = 6
)

p_adverse_impact_unblinded_models_gender <- adverse_impact_gender_unblinded_models %>%
  # Make sure none of the CIs clip out of bounds
  mutate(
    CI_lwr0 = pmax(CI_lwr0, 0),
    CI_upr0 = pmax(CI_upr0, 0),
    CI_lwr1 = pmax(CI_lwr1, 0),
    CI_upr1 = pmax(CI_upr1, 0)
  ) %>%
  ggplot(aes(x = hire, y = est, color = value)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 1.25, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_linerange(
    aes(ymin = CI_lwr0, ymax = CI_upr0),
    linewidth = 0.5,
    position = position_dodge(width = 0.25)
  ) +
  geom_linerange(
    aes(ymin = CI_lwr1, ymax = CI_upr1),
    linewidth = 2,
    position = position_dodge(width = 0.25)
  ) +
  scale_x_continuous(breaks = 3:5) +
  scale_y_continuous(
    limits = c(0, 2.5),
    breaks = seq(0, 2.5, 0.5),
    oob = oob_keep,
  ) +
  scale_color_manual(values = c("Female" = "gray50")) +
  labs(
    title = "Adverse impact ratio",
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More male hires — More female hires" ~ {phantom() %->% phantom()}),
    color = "Race"
  ) +
  facet_wrap(vars(model)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "adverse-impact-unblinded-models-gender.pdf")),
  plot = p_adverse_impact_unblinded_models_gender,
  width = 6,
  height = 6
)

adverse_impact_race_blinded_gt_models <- unadjusted_diff %>%
  filter(
    experiment_type == "redacted",
    race %in% c("Black", "Hispanic", "White")
  ) %>%
  count(model, race, hire) %>%
  ungroup() %>%
  complete(model, race, hire = 1:5, fill = list(n = 0)) %>%
  arrange(model, race, desc(hire)) %>%
  group_by(model, race) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = c(model, hire),
    names_from = race,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
  select(-White) %>%
  pivot_longer(
    cols = c(Black, Hispanic),
    names_to = "race",
    values_to = "adverse_impact_ratio"
  )

adverse_impact_race_blinded_models <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(
        experiment_type == "redacted",
        race %in% c("Black", "Hispanic", "White")
      ) %>%
      group_by(model, race) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(model, race, hire) %>%
      ungroup() %>%
      complete(model, race, hire = 1:5, fill = list(n = 0)) %>%
      arrange(model, race, desc(hire)) %>%
      group_by(model, race) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = c(model, hire),
        names_from = race,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(across(c(Black, Hispanic), ~ .x / White)) %>%
      select(-White) %>%
      pivot_longer(
        cols = c(Black, Hispanic),
        names_to = "race",
        values_to = "adverse_impact_ratio"
      )
  }) %>%
  list_rbind() %>%
  group_by(model, hire, race) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025, na.rm = TRUE),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975, na.rm = TRUE),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16, na.rm = TRUE),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(model, desc(hire), race) %>%
  transmute(
    model = MODEL_TRANSLATOR[model],
    characteristic = "Race",
    value = race,
    hire = hire,
    est = adverse_impact_race_blinded_gt_models$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

adverse_impact_gender_blinded_gt_models <- unadjusted_diff %>%
  filter(experiment_type == "redacted") %>%
  count(model, gender, hire) %>%
  arrange(model, gender, desc(hire)) %>%
  group_by(model, gender) %>%
  mutate(N = sum(n), prop = cumsum(n) / N) %>%
  pivot_wider(
    id_cols = c(model, hire),
    names_from = gender,
    values_from = prop,
    values_fill = 0
  ) %>%
  # Everyone received at least a 2
  filter(hire > 2) %>%
  mutate(female = female / male) %>%
  select(-male) %>%
  rename(adverse_impact_ratio = female)


# Bootstrap the adverse impact ratios
adverse_impact_gender_blinded_models <- map(
  1:1000, ~ {
    unadjusted_diff %>%
      filter(experiment_type == "redacted") %>%
      group_by(model, gender) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      count(model, gender, hire) %>%
      arrange(model, gender, desc(hire)) %>%
      group_by(model, gender) %>%
      mutate(N = sum(n), prop = cumsum(n) / N) %>%
      pivot_wider(
        id_cols = c(model, hire),
        names_from = gender,
        values_from = prop,
        values_fill = 0
      ) %>%
      # Everyone received at least a 2
      filter(hire > 2) %>%
      mutate(female = female / male) %>%
      select(-male) %>%
      rename(adverse_impact_ratio = female)
  }) %>%
  list_rbind() %>%
  group_by(model, hire) %>%
  summarize(
    lwr_quant0 = quantile(adverse_impact_ratio, 0.025),
    upr_quant0 = quantile(adverse_impact_ratio, 0.975),
    lwr_quant1 = quantile(adverse_impact_ratio, 0.16),
    upr_quant1 = quantile(adverse_impact_ratio, 0.84),
    .groups = "drop"
  ) %>%
  arrange(model, desc(hire)) %>%
  transmute(
    model = MODEL_TRANSLATOR[model],
    characteristic = "Gender",
    value = "Female",
    hire = hire,
    est = adverse_impact_gender_blinded_gt_models$adverse_impact_ratio,
    CI_lwr0 = pmax(2 * est - upr_quant0, 0),
    CI_upr0 = pmax(2 * est - lwr_quant0, 0),
    CI_lwr1 = pmax(2 * est - upr_quant1, 0),
    CI_upr1 = pmax(2 * est - lwr_quant1, 0)
  )

p_adverse_impact_blinded_models_race <- adverse_impact_race_blinded_models %>%
  # Make sure none of the CIs clip out of bounds
  mutate(
    CI_lwr0 = pmax(CI_lwr0, 0),
    CI_upr0 = pmax(CI_upr0, 0),
    CI_lwr1 = pmax(CI_lwr1, 0),
    CI_upr1 = pmax(CI_upr1, 0)
  ) %>%
  ggplot(aes(x = hire, y = est, color = value)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 1.25, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_linerange(
    aes(ymin = CI_lwr0, ymax = CI_upr0),
    linewidth = 0.5,
    position = position_dodge(width = 0.25)
  ) +
  geom_linerange(
    aes(ymin = CI_lwr1, ymax = CI_upr1),
    linewidth = 2,
    position = position_dodge(width = 0.25)
  ) +
  scale_x_continuous(breaks = 3:5) +
  scale_y_continuous(
    limits = c(0, 1.7),
    breaks = seq(0, 2, 0.5),
    oob = oob_keep
  ) +
  scale_color_manual(values = set_names(hue_pal()(3)[-3], c("Black", "Hispanic"))) +
  labs(
    title = "Adverse impact ratio",
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More White hires — More minority hires" ~ {phantom() %->% phantom()}),
    color = "Race"
  ) +
  facet_wrap(vars(model)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "adverse-impact-blinded-models-race.pdf")),
  plot = p_adverse_impact_blinded_models_race,
  width = 6,
  height = 6
)

p_adverse_impact_blinded_models_gender <- adverse_impact_gender_blinded_models %>%
  # Make sure none of the CIs clip out of bounds
  mutate(
    CI_lwr0 = pmax(CI_lwr0, 0),
    CI_upr0 = pmax(CI_upr0, 0),
    CI_lwr1 = pmax(CI_lwr1, 0),
    CI_upr1 = pmax(CI_upr1, 0)
  ) %>%
  ggplot(aes(x = hire, y = est, color = value)) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 1.25, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.25), size = 3) +
  geom_line(position = position_dodge(width = 0.25)) +
  geom_linerange(
    aes(ymin = CI_lwr0, ymax = CI_upr0),
    linewidth = 0.5,
    position = position_dodge(width = 0.25)
  ) +
  geom_linerange(
    aes(ymin = CI_lwr1, ymax = CI_upr1),
    linewidth = 2,
    position = position_dodge(width = 0.25)
  ) +
  scale_x_continuous(breaks = 3:5) +
  scale_y_continuous(
    breaks = seq(0, 2.5, 0.5),
    limits = c(0, 2.5),
    oob = oob_keep
  ) +
  scale_color_manual(values = c("Female" = "gray50")) +
  labs(
    title = "Adverse impact ratio",
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More male hires — More female hires" ~ {phantom() %->% phantom()}),
    color = "Race"
  ) +
  facet_wrap(vars(model)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "adverse-impact-blinded-models-gender.pdf")),
  plot = p_adverse_impact_blinded_models_gender,
  width = 6,
  height = 6
)


################################################################################
# Audit study results (across models)

model_ratings <- ratings %>%
  left_join(
    select(requests, request_id, prompt_id, model),
    by = "request_id"
  ) %>%
  left_join(
    select(prompts, prompt_id, persona_id, experiment_type),
    by = "prompt_id"
  ) %>%
  left_join(
    select(personas, persona_id, interview_id, race, gender, college, name = first_name),
    by = "persona_id"
  ) %>%
  filter(experiment_type == "base", error == 0) %>%
  collect() %>%
  mutate(
    race = factor(race, levels = c("White", "Black", "Hispanic", "Asian")),
    gender = factor(gender, levels = c("male", "female"))
  )

# Vectorized convenience function that extracts the population standard deviation
# of the hire rating per model
sd_model <- function(models) {
  v <- model_ratings %>%
    group_by(model) %>%
    summarize(sd = sd(hire)) %>%
    with(set_names(sd, model))
  v[models]
}

p_models <- model_ratings %>%
  group_by(model) %>%
  group_modify(~ tidy(felm(hire ~ gender + race | 1 | 0 | interview_id, data = .))) %>%
  mutate(
    across(c(estimate, std.error), ~ .x / sd_model(model)),
    characteristic = case_when(
      str_detect(term, "^gender(?!_)") ~ "Gender",
      str_detect(term, "^race(?!_)")   ~ "Race",
      TRUE                        ~ "Intercept"
    ),
    value = str_to_title(str_remove(term, "^gender|race")),
    model = factor(
      model,
      levels = names(MODEL_TRANSLATOR),
      labels = MODEL_TRANSLATOR
    ),
    company = case_when(
      str_detect(model, "GPT") ~ "OpenAI",
      str_detect(model, "Mi.tral") ~ "Mistral",
      str_detect(model, "Claude") ~ "Anthropic"
    )
  ) %>%
  filter(characteristic != "Intercept") %>%
  ggplot(aes(x = estimate, y = model, color = value)) +
  geom_rect(
    aes(fill = company, alpha = characteristic),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    color = "transparent"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodgev(height = 0.8), size = 3) +
  geom_linerange(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error
    ),
    linewidth = 2,
    position = position_dodgev(height = 0.8)
  ) +
  geom_linerange(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error
    ),
    linewidth = 0.5,
    position = position_dodgev(height = 0.8)
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.05,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Gender",
      label = "{phantom() %<-% phantom()} * \"Higher male                                 Higher female \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.05,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Race",
      label = "{phantom() %<-% phantom()} * \"Higher White                                 Higher minority \" * {phantom() %->% phantom()}"
    )
  ) +
  scale_color_discrete(breaks = c("Asian", "Black", "Hispanic")) +
  scale_alpha_manual(values = c("Gender" = 0.06, "Race" = 0.02)) +
  guides(alpha = "none", fill = "none") +
  labs(
    x = "Difference in rating (pop. std. dev.)",
    y = "Model",
    color = "Race"
  ) +
  facet_grid(rows = vars(company), cols = vars(characteristic), scales = "free_y") +
  force_panelsizes(rows = c(2.5, 1, 2)) +
  theme(legend.position = "bottom", panel.spacing = unit(0.25, "lines"))

ggsave(
  path_join(c("plots", "models.pdf")),
  plot = p_models,
  width = 6,
  height = 4.5
)

# Audit study results (across models) with name random effects.
# NOTE: We also include applicant-level random effects to account for
#       clustering.
p_models_re <- model_ratings %>%
  group_by(model) %>%
  group_modify(~ tidy(lmer(hire ~ gender + race + (1 | name) + (1 | interview_id), data = .))) %>%
  filter(effect == "fixed") %>%
  select(-effect, -group) %>%
  mutate(
    across(c(estimate, std.error), ~ .x / sd_model(model)),
    characteristic = case_when(
      str_detect(term, "^gender(?!_)") ~ "Gender",
      str_detect(term, "^race(?!_)")   ~ "Race",
      TRUE                        ~ "Intercept"
    ),
    value = str_to_title(str_remove(term, "^gender|race")),
    model = factor(
      model,
      levels = names(MODEL_TRANSLATOR),
      labels = MODEL_TRANSLATOR
    ),
    company = case_when(
      str_detect(model, "GPT") ~ "OpenAI",
      str_detect(model, "Mi.tral") ~ "Mistral",
      str_detect(model, "Claude") ~ "Anthropic"
    )
  ) %>%
  filter(characteristic != "Intercept") %>%
  ggplot(aes(x = estimate, y = model, color = value)) +
  geom_rect(
    aes(fill = company, alpha = characteristic),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    color = "transparent"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodgev(height = 0.8), size = 3) +
  geom_linerange(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error
    ),
    linewidth = 2,
    position = position_dodgev(height = 0.8)
  ) +
  geom_linerange(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error
    ),
    linewidth = 0.5,
    position = position_dodgev(height = 0.8)
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.07,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Gender",
      label = "{phantom() %<-% phantom()} * \"Higher male                                 Higher female \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.07,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Race",
      label = "{phantom() %<-% phantom()} * \"Higher White                                 Higher minority \" * {phantom() %->% phantom()}"
    )
  ) +
  scale_color_discrete(breaks = c("Asian", "Black", "Hispanic")) +
  scale_alpha_manual(values = c("Gender" = 0.06, "Race" = 0.02)) +
  guides(alpha = "none", fill = "none") +
  labs(
    x = "Difference in rating (pop. std. dev.)",
    y = "Model",
    color = "Race"
  ) +
  facet_grid(rows = vars(company), cols = vars(characteristic), scales = "free_y") +
  force_panelsizes(rows = c(2.5, 1, 2)) +
  theme(legend.position = "bottom", panel.spacing = unit(0.25, "lines"))

ggsave(
  path_join(c("plots", "models-re.pdf")),
  plot = p_models_re,
  width = 6,
  height = 4.5
)

# Audit study results in terms of differences in hiring rates at different
# thresholds.
p_selection_model_gender <- model_ratings %>%
  count(model, gender, hire) %>%
  complete(model, gender, hire = 1:5, fill = list(n = 0)) %>%
  arrange(model, gender, desc(hire)) %>%
  group_by(model, gender) %>%
  reframe(
    hire = hire,
    N = sum(n),
    prop = cumsum(n) / N,
    std.err = sqrt(prop * (1 - prop) / N),
    .groups = "drop"
  ) %>%
  select(model, gender, hire, prop, std.err) %>%
  pivot_wider(
    id_cols = c(model, hire),
    names_from = gender,
    values_from = c(prop, std.err),
  ) %>%
  transmute(
    model = MODEL_TRANSLATOR[model],
    hire = hire,
    diff = prop_female - prop_male,
    std.err = sqrt(std.err_male^2 + std.err_female^2)
  ) %>%
  filter(hire != 1) %>%
  ggplot(aes(x = hire, y = diff)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +
  geom_linerange(
    aes(ymin = diff - 1 * std.err, ymax = diff + 1 * std.err),
    linewidth = 2
  ) +
  geom_linerange(
    aes(ymin = diff - 2 * std.err, ymax = diff + 2 * std.err),
    linewidth = 0.5
  ) +
  geom_line() +
  scale_x_continuous(limits = c(2, 5), breaks = 2:5, expand = c(0, 0.3)) +
  scale_y_continuous(limits = c(-0.04, 0.08), labels = scales::percent) +
  labs(
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More male hires - More female hires" ~ {phantom() %->% phantom()})
  ) +
  guides(color = guide_none()) +
  facet_wrap(vars(model), ncol = 4)

ggsave(
  path_join(c("plots", "models-diff-gender.pdf")),
  plot = p_selection_model_gender,
  width = 6,
  height = 6
)

# Audit study results in terms of differences in hiring rates at different
# thresholds.
p_selection_model_race <- model_ratings %>%
  count(model, race, hire) %>%
  complete(model, race, hire = 1:5, fill = list(n = 0)) %>%
  arrange(model, race, desc(hire)) %>%
  group_by(model, race) %>%
  reframe(
    hire = hire,
    N = sum(n),
    prop = cumsum(n) / N,
    std.err = sqrt(prop * (1 - prop) / N)
  ) %>%
  select(model, race, hire, prop, std.err) %>%
  pivot_wider(
    id_cols = c(model, hire),
    names_from = race,
    values_from = c(prop, std.err),
  ) %>%
  transmute(
    model = MODEL_TRANSLATOR[model],
    hire = hire,
    diff_Asian = prop_Asian - prop_White,
    diff_Black = prop_Black - prop_White,
    diff_Hispanic = prop_Hispanic - prop_White,
    std.err_Asian = sqrt(std.err_White^2 + std.err_Asian^2),
    std.err_Black = sqrt(std.err_White^2 + std.err_Black^2),
    std.err_Hispanic = sqrt(std.err_White^2 + std.err_Hispanic^2)
  ) %>%
  pivot_longer(
    cols = c(
      diff_Asian,
      diff_Black,
      diff_Hispanic,
      std.err_Asian,
      std.err_Black,
      std.err_Hispanic
    ),
    names_sep = "_",
    names_to = c("stat", "race"),
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  filter(hire != 1) %>%
  ggplot(aes(x = hire, y = diff, color = race)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_linerange(
    aes(ymin = diff - 1 * std.err, ymax = diff + 1 * std.err),
    linewidth = 2,
    position = position_dodge(width = 0.5)
  ) +
  geom_linerange(
    aes(ymin = diff - 2 * std.err, ymax = diff + 2 * std.err),
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_x_continuous(limits = c(1.5, 5.5), breaks = 2:5, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.05, 0.11), labels = scales::percent) +
  labs(
    x = "Hiring threshold",
    y = expr({phantom() %<-% phantom()} ~ "More White hires - More minority hires" ~ {phantom() %->% phantom()})
  ) +
  guides(color = guide_legend(title = "Race")) +
  facet_wrap(vars(model), ncol = 4) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "models-diff-race.pdf")),
  plot = p_selection_model_race,
  width = 6,
  height = 6
)


################################################################################
# Audit study results (across framings)

framing_ratings <- ratings %>%
  left_join(
    select(requests, request_id, prompt_id, model),
    by = "request_id"
  ) %>%
  left_join(
    select(prompts, prompt_id, persona_id, experiment_type),
    by = "prompt_id"
  ) %>%
  left_join(
    select(personas, persona_id, interview_id, race, gender, college, name = first_name),
    by = "persona_id"
  ) %>%
  filter(
    model == "gpt-3.5-turbo-0125",
    error == 0,
    experiment_type %in% c(
      "base",
      "no_scratch",
      "no_transcripts",
      "other_district",
      "eeoc_guidance"
    )
  ) %>%
  collect() %>%
  mutate(
    race = factor(race, levels = c("White", "Black", "Hispanic", "Asian")),
    gender = factor(gender, levels = c("male", "female"))
  )

# Vectorized convenience function that extracts the population standard deviation
# of the hire rating per framing
sd_framing <- function(framings) {
  v <- framing_ratings %>%
    group_by(experiment_type) %>%
    summarize(sd = sd(hire)) %>%
    with(set_names(sd, experiment_type))
  v[framings]
}

p_framing <- framing_ratings %>%
  group_by(experiment_type) %>%
  group_modify(~ tidy(felm(hire ~ gender + race | 1 | 0 | interview_id, data = .))) %>%
  mutate(
    across(c(estimate, std.error), ~ .x / sd_framing(experiment_type)),
    characteristic = case_when(
      str_detect(term, "^gender") ~ "Gender",
      str_detect(term, "^race")   ~ "Race",
      TRUE                        ~ "Intercept"
    ),
    value = str_to_title(str_remove(term, "^gender|race")),
    experiment_type = factor(
      experiment_type,
      levels = names(FRAMING_TRANSLATOR),
      labels = FRAMING_TRANSLATOR
    )
  ) %>%
  filter(characteristic != "Intercept") %>%
  ggplot(aes(x = estimate, y = experiment_type, color = value)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodgev(height = 0.8), size = 3) +
  geom_linerange(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error
    ),
    linewidth = 2,
    position = position_dodgev(height = 0.8)
  ) +
  geom_linerange(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error
    ),
    linewidth = 0.5,
    position = position_dodgev(height = 0.8)
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.075,
    y = 0.5,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Gender",
      label = "{phantom() %<-% phantom()} * \"Higher male                                 Higher female \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.075,
    y = 0.5,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Race",
      label = "{phantom() %<-% phantom()} * \"Higher White                                 Higher minority \" * {phantom() %->% phantom()}"
    )
  ) +
  scale_color_discrete(breaks = c("Asian", "Black", "Hispanic")) +
  labs(
    x = expr("Difference in rating (pop. std. dev.)"),
    y = "Framing",
    color = "Race"
  ) +
  facet_wrap(vars(characteristic)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "framing.pdf")),
  plot = p_framing,
  width = 6,
  height = 4
)

# Audit study results (across framings) with name random effects.
# NOTE: We also include applicant-level random effects to account for
#       clustering.
p_framing_re <- framing_ratings %>%
  group_by(experiment_type) %>%
  # NOTE: Because there is very little variance by name, the fit for
  #       `no_scratch` is singular.
  group_modify(~ tidy(lmer(hire ~ gender + race + (1 | name) + (1 | interview_id), data = .))) %>%
  filter(effect == "fixed") %>%
  select(-effect, -group) %>%
  mutate(
    across(c(estimate, std.error), ~ .x / sd_framing(experiment_type)),
    characteristic = case_when(
      str_detect(term, "^gender") ~ "Gender",
      str_detect(term, "^race")   ~ "Race",
      TRUE                        ~ "Intercept"
    ),
    value = str_to_title(str_remove(term, "^gender|race")),
    experiment_type = factor(
      experiment_type,
      levels = names(FRAMING_TRANSLATOR),
      labels = FRAMING_TRANSLATOR
    )
  ) %>%
  filter(characteristic != "Intercept") %>%
  ggplot(aes(x = estimate, y = experiment_type, color = value)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodgev(height = 0.8), size = 3) +
  geom_linerange(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error
    ),
    linewidth = 2,
    position = position_dodgev(height = 0.8)
  ) +
  geom_linerange(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error
    ),
    linewidth = 0.5,
    position = position_dodgev(height = 0.8)
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.075,
    y = 0.5,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Gender",
      label = "{phantom() %<-% phantom()} * \"Higher male                                 Higher female \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.075,
    y = 0.5,
    parse = TRUE,
    size = 2,
    data = tibble(
      company = "OpenAI",
      characteristic = "Race",
      label = "{phantom() %<-% phantom()} * \"Higher White                                 Higher minority \" * {phantom() %->% phantom()}"
    )
  ) +
  scale_color_discrete(breaks = c("Asian", "Black", "Hispanic")) +
  labs(
    x = expr("Difference in rating (pop. std. dev.)"),
    y = "Framing",
    color = "Race"
  ) +
  facet_wrap(vars(characteristic)) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "framing-re.pdf")),
  plot = p_framing_re,
  width = 6,
  height = 4
)

################################################################################
# Audit study results (across variants)

variant_ratings <- ratings %>%
  left_join(
    select(requests, request_id, prompt_id, model),
    by = "request_id"
  ) %>%
  left_join(
    select(prompts, prompt_id, persona_id, experiment_type),
    by = "prompt_id"
  ) %>%
  left_join(
    select(personas, persona_id, interview_id, race, gender, college, name = first_name),
    by = "persona_id"
  ) %>%
  filter(
    model == "gpt-3.5-turbo-0125",
    error == 0,
    experiment_type %in% c(
      "base",
      "variant_0",
      "variant_1",
      "variant_2",
      "variant_3"
    )
  ) %>%
  collect() %>%
  mutate(
    race = factor(race, levels = c("White", "Black", "Hispanic", "Asian")),
    gender = factor(gender, levels = c("male", "female"))
  )


# Vectorized convenience function that extracts the population standard deviation
# of the hire rating per variation
sd_variant <- function(variants) {
  v <- variant_ratings %>%
    group_by(experiment_type) %>%
    summarize(sd = sd(hire)) %>%
    with(set_names(sd, experiment_type))
  v[variants]
}


results_variant <- variant_ratings %>%
  group_by(experiment_type) %>%
  group_modify(~ tidy(felm(hire ~ race + gender | 1 | 0 | interview_id, data = .))) %>%
  mutate(
    across(c(estimate, std.error), ~ .x / sd_variant(experiment_type)),
    characteristic = case_when(
      str_detect(term, "^gender") ~ "Gender",
      str_detect(term, "^race")   ~ "Race",
      TRUE                        ~ "Intercept"
    ),
    value = str_to_title(str_remove(term, "^gender|race"))
  ) %>%
  filter(characteristic != "Intercept")

results_base <- results_variant %>%
  filter(experiment_type == "base") %>%
  mutate(value = factor(
    value,
    levels = c("Female", "Asian", "Black", "Hispanic")
  ))

p_variant <- results_variant %>%
  ungroup() %>%
  filter(experiment_type != "base") %>%
  mutate(
    experiment_type = factor(
      experiment_type,
      levels = str_c("variant_", 0:3),
      labels = str_c("Variant ", 0:3)
    ),
    value = factor(
      value,
      levels = c("Female", "Asian", "Black", "Hispanic")
    )
  ) %>%
  ggplot(aes(x = estimate, y = experiment_type)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(
    aes(xintercept = estimate),
    color = "blue",
    linewidth = 3,
    data = results_base
  ) +
  geom_rect(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error,
    ),
    ymin = -Inf,
    ymax = Inf,
    fill = "blue",
    alpha = 0.3,
    data = results_base,
    inherit.aes = FALSE
  ) +
  geom_rect(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error,
    ),
    ymin = -Inf,
    ymax = Inf,
    fill = "blue",
    alpha = 0.1,
    data = results_base,
    inherit.aes = FALSE
  ) +
  geom_point(position = position_dodgev(height = 0.8), size = 3) +
  geom_linerange(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error
    ),
    linewidth = 2,
    position = position_dodgev(height = 0.8)
  ) +
  geom_linerange(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error
    ),
    linewidth = 0.5,
    position = position_dodgev(height = 0.8)
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.055,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Female", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher male                                                     Higher female \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.055,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Asian", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher White                                                     Higher Asian \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.055,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Black", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher White                                                     Higher Black \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.055,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Hispanic", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher White                                                  Higher Hispanic \" * {phantom() %->% phantom()}"
    )
  ) +
  labs(
    x = expr("Difference in rating (pop. std. dev.)"),
    y = "Variant",
    color = "Race"
  ) +
  facet_wrap(vars(value), ncol = 2) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "variant.pdf")),
  plot = p_variant,
  width = 6,
  height = 4
)

# Audit study results (across variants) with name random effects.
# NOTE: We also include applicant-level random effects to account for
#       clustering.
results_variant_re <- variant_ratings %>%
  group_by(experiment_type) %>%
  # NOTE: Because there is very little variance by name, the fit for
  #       variants 2 and 3 are singular.
  group_modify(~ tidy(lmer(hire ~ gender + race + (1 | name) + (1 | interview_id), data = .))) %>%
  filter(effect == "fixed") %>%
  select(-effect, -group) %>%
  mutate(
    across(c(estimate, std.error), ~ .x / sd_variant(experiment_type)),
    characteristic = case_when(
      str_detect(term, "^gender") ~ "Gender",
      str_detect(term, "^race")   ~ "Race",
      TRUE                        ~ "Intercept"
    ),
    value = str_to_title(str_remove(term, "^gender|race"))
  ) %>%
  filter(characteristic != "Intercept")

results_base_re <- results_variant %>%
  filter(experiment_type == "base") %>%
  mutate(value = factor(
    value,
    levels = c("Female", "Asian", "Black", "Hispanic")
  ))

p_variant_re <- results_variant_re %>%
  ungroup() %>%
  filter(experiment_type != "base") %>%
  mutate(
    experiment_type = factor(
      experiment_type,
      levels = str_c("variant_", 0:3),
      labels = str_c("Variant ", 0:3)
    ),
    value = factor(
      value,
      levels = c("Female", "Asian", "Black", "Hispanic")
    )
  ) %>%
  ggplot(aes(x = estimate, y = experiment_type)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(
    aes(xintercept = estimate),
    color = "blue",
    linewidth = 3,
    data = results_base
  ) +
  geom_rect(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error,
    ),
    ymin = -Inf,
    ymax = Inf,
    fill = "blue",
    alpha = 0.3,
    data = results_base_re,
    inherit.aes = FALSE
  ) +
  geom_rect(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error,
    ),
    ymin = -Inf,
    ymax = Inf,
    fill = "blue",
    alpha = 0.1,
    data = results_base,
    inherit.aes = FALSE
  ) +
  geom_point(position = position_dodgev(height = 0.8), size = 3) +
  geom_linerange(
    aes(
      xmin = estimate - 1 * std.error,
      xmax = estimate + 1 * std.error
    ),
    linewidth = 2,
    position = position_dodgev(height = 0.8)
  ) +
  geom_linerange(
    aes(
      xmin = estimate - 2 * std.error,
      xmax = estimate + 2 * std.error
    ),
    linewidth = 0.5,
    position = position_dodgev(height = 0.8)
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.06,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Female", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher male                                                     Higher female \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.06,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Asian", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher White                                                     Higher Asian \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.06,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Black", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher White                                                     Higher Black \" * {phantom() %->% phantom()}"
    )
  ) +
  geom_text(
    aes(label = label),
    inherit.aes = FALSE,
    x = 0.06,
    y = 0.55,
    parse = TRUE,
    size = 2,
    data = tibble(
      value = factor("Hispanic", levels = c("Female", "Asian", "Black", "Hispanic")),
      label = "{phantom() %<-% phantom()} * \"Higher White                                                  Higher Hispanic \" * {phantom() %->% phantom()}"
    )
  ) +
  labs(
    x = expr("Difference in rating (pop. std. dev.)"),
    y = "Variant",
    color = "Race"
  ) +
  facet_wrap(vars(value), ncol = 2) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "variant-re.pdf")),
  plot = p_variant_re,
  width = 6,
  height = 4
)

################################################################################
# Analysis of parseability

errors <- ratings %>%
  left_join(
    select(requests, request_id, prompt_id, model),
    by = "request_id"
  ) %>%
  left_join(
    select(prompts, prompt_id, persona_id, experiment_type),
    by = "prompt_id"
  ) %>%
  left_join(
    select(personas, persona_id, interview_id, race, gender, college, name = first_name),
    by = "persona_id"
  ) %>%
  filter(experiment_type == "base") %>%
  collect() %>%
  mutate(
    race = factor(race, levels = c("White", "Black", "Hispanic", "Asian")),
    gender = factor(gender, levels = c("male", "female"))
  )

# Calculate error rates by model and format the output with xtable
errors %>%
  mutate(
    error = factor(error == 1, labels = c("Parseable", "Unparseable")),
    model = factor(
      model,
      levels = names(MODEL_TRANSLATOR),
      labels = MODEL_TRANSLATOR
    )
  ) %>%
  count(model, error) %>%
  spread(error, n, fill = 0) %>%
  mutate(
    total = Parseable + Unparseable,
    error_rate = 100 * Unparseable / total,
    error_rate = sprintf("%.1f%%", error_rate)
  ) %>%
  arrange(error_rate) %>%
  select(Model = model, `Error Rate` = error_rate) %>%
  xtable(caption = "Proportion of unparseable responses by model.") %>%
  print(include.rownames = FALSE, table.placement = "t")

# Calculate the proportion of unparseable responses by gender
errors %>%
  lm(error ~ -1 + model + model:gender, data = .) %>%
  tidy() %>%
  filter(p.value < 0.1, str_detect(term, "gender")) %>%
  mutate(
    model = str_extract(term, "(?<=model).*(?=:gender)"),
    model = factor(
      model,
      levels = names(MODEL_TRANSLATOR),
      labels = MODEL_TRANSLATOR
    ),
    gender = str_extract(term, "(?<=:gender).*"),
    estimate = sprintf("%.1f", 100 * estimate),
    std.error = sprintf("%.1f", 200 * std.error)
  ) %>%
  select(model, gender, estimate, std.error) %>%
  glue_data(
    "{model} is {estimate}\\% ± {std.error}\\% more likely to produce ",
    "unparseable responses for {gender} applicants."
  )

# Plot the proportion of unparseable responses by gender
p_error_gender <- errors %>%
  count(model, gender, error) %>%
  complete(model, gender, error, fill = list(n = 0)) %>%
  mutate(
    model = factor(
      model,
      levels = names(MODEL_TRANSLATOR),
      labels = MODEL_TRANSLATOR
    ),
    error = factor(error == 1, labels = c("Parseable", "Unparseable"))
  ) %>%
  ggplot(aes(x = gender, y = n, fill = error)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c(hue_pal()(2), "black")) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Gender",
    y = "Percentage of Prompts",
    fill = NULL
  ) +
  facet_wrap(vars(model), ncol = 3) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "errors-gender.pdf")),
  plot = p_error_gender,
  width = 5,
  height = 5
)

# Calculate the proportion of unparseable responses by race
errors %>%
  lm(error ~ -1 + model + model:race, data = .) %>%
  tidy() %>%
  filter(p.value < 0.1, str_detect(term, "race")) %>%
  mutate(
    model = str_extract(term, "(?<=model).*(?=:race)"),
    model = factor(
      model,
      levels = names(MODEL_TRANSLATOR),
      labels = MODEL_TRANSLATOR
    ),
    race = str_extract(term, "(?<=:race).*"),
    estimate = sprintf("%.1f", 100 * estimate),
    std.error = sprintf("%.1f", 200 * std.error)
  ) %>%
  select(model, race, estimate, std.error) %>%
  glue_data(
    "{model} is {estimate}\\% ± {std.error}\\% more likely to produce ",
    "unparseable responses for {race} applicants."
  )

# Plot the proportion of unparseable responses by race
p_error_race <- errors %>%
  count(model, race, error) %>%
  complete(model, race, error, fill = list(n = 0)) %>%
  mutate(
    model = factor(
      model,
      levels = names(MODEL_TRANSLATOR),
      labels = MODEL_TRANSLATOR
    ),
    race = factor(race, levels = levels(race), labels = c("A", "B", "H", "W")),
    error = factor(error == 1, labels = c("Parseable", "Unparseable"))
  ) %>%
  ggplot(aes(x = race, y = n, fill = error)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c(hue_pal()(2), "black")) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Race",
    y = "Percentage of Prompts",
    fill = NULL
  ) +
  facet_wrap(vars(model), ncol = 3) +
  theme(legend.position = "bottom")

ggsave(
  path_join(c("plots", "errors-race.pdf")),
  plot = p_error_race,
  width = 5,
  height = 5
)

################################################################################
# AUC analysis

# NOTE: Embeddings are not available in the public data.
# # Restructure the data to have one row per interview
# auc_data <- interviews %>%
#   filter(in_study) %>%
#   left_join(embeddings, by = "interview_id") %>%
#   mutate(
#     # Make `redacted` and `resume` more informative
#     redacted = if_else(redacted == 1, "R", "U"),
#     resume = if_else(resume == 1, "R", "T")
#   ) %>%
#   select(-in_study, -embedding_id) %>%
#   pivot_wider(
#     id_cols = c(interview_id, race, gender),
#     names_from = c(redacted, resume),
#     values_from = starts_with("X_"),
#     names_glue = "{redacted}{resume}.{.value}"
#   ) %>%
#   collect() %>%
#   mutate(
#     # Convert race and gender to the relevant outcomes
#     race = factor(
#       race == "White",
#       levels = c(FALSE, TRUE),
#       labels = c("Minority", "White")
#     ),
#     gender = factor(gender, levels = c("female", "male"))
#   )
# 
# # Define a convenience function to analyze the AUC
# perform_analysis <- function(formula_) {
#   # Split the data into training and testing sets
#   train_test_split <- initial_split(auc_data, prop = 1/2)
#   
#   # 10-fold cross-validation
#   train_cv <- vfold_cv(training(train_test_split), v = 10)
#   test_cv <- vfold_cv(testing(train_test_split), v = 10)
#   
#   # Model specification for outcome prediction
#   train_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
#     set_engine("glmnet") %>%
#     set_mode("classification")
#   
#   # Workflow for outcome prediction
#   train_wf <- workflow() %>%
#     add_model(train_spec) %>%
#     add_formula(formula_)
#   
#   # Fit and evaluate the model with cross-validation for outcome prediction on the training
#   # data.
#   tuning_grid <- grid_regular(
#     penalty(range = c(-5, 1)),
#     mixture(range = c(0, 1)),
#     levels = 10
#   )
#   
#   # Tune the outcome model
#   res <- tune_grid(
#     train_wf,
#     resamples = train_cv,
#     grid = tuning_grid,
#     metrics = metric_set(roc_auc)
#   )
#   
#   # Collect and summarize the tuning results
#   best <- select_best(res, metric = "roc_auc")
# 
#   # Fit and evaluate the model with cross-validation for outcome prediction using
#   # the optimal hyperparameters on the testing data
#   test_spec <- logistic_reg(
#       penalty = best$penalty,
#       mixture = best$mixture
#     ) %>%
#     set_engine("glmnet") %>%
#     set_mode("classification")
#   test_wf <- workflow() %>%
#     add_model(test_spec) %>%
#     add_formula(formula_)
#   fit_resamples(
#       test_wf,
#       test_cv,
#       metrics = metric_set(roc_auc)
#     ) %>%
#     collect_metrics() %>%
#     select(-starts_with(".config")) %>%
#     bind_cols(best) %>%
#     select(-starts_with(".config"), -.metric, -.estimator, -n)
# }
# 
# # Convenience function to generate formulas for the AUC analysis
# generate_formula <- function(outcome, status, features) {
#   if (features == "resume") {
#     reformulate(
#       str_c(if_else(status == "redacted", "R", "U"), "R.X_", 0:255),
#       response = outcome
#     )
#   } else if (features == "transcripts") {
#     reformulate(
#       str_c(if_else(status == "redacted", "R", "U"), "T.X_", 0:255),
#       response = outcome
#     )
#   } else {
#     reformulate(
#       c(
#         str_c(if_else(status == "redacted", "R", "U"), "R.X_", 0:255),
#         str_c(if_else(status == "redacted", "R", "U"), "T.X_", 0:255)
#       ),
#       response = outcome
#     )
#   }
# }
# 
# # Generate formulas for the AUC analysis
# p_auc <- expand_grid(
#     outcome = c("race", "gender"),
#     status = c("redacted", "unredacted"),
#     features = c("resume", "transcripts", "both")
#   ) %>%
#   rowwise() %>%
#   mutate(results = list(perform_analysis(generate_formula(outcome, status, features)))) %>%
#   unnest(results) %>%
#   mutate(
#     # Capitalize the features, outcomes, and statuses
#     features = factor(
#       features,
#       levels = c("resume", "transcripts", "both"),
#       labels = c("Resume", "Transcripts", "Both")
#     ),
#     outcome = str_to_title(outcome),
#     status = str_to_title(status)
#   ) %>%
p_auc <- tribble(
   ~outcome, ~status,      ~features,     ~mean,              ~std_err,             ~penalty,             ~mixture,
   "Race",   "Redacted",   "Resume",      0.8181976249244186, 0.029362309783269987, 0.1,                  0,
   "Race",   "Redacted",   "Transcripts", 0.9328436434978218, 0.0102126843939987,   0.1,                  0,
   "Race",   "Redacted",   "Both",        0.917247211364543,  0.017326371948840378, 0.021544346900318822, 0.2222222222222222,
   "Race",   "Unredacted", "Resume",      0.8231764790652418, 0.02503523200401656,  0.46415888336127725,  0,
   "Race",   "Unredacted", "Transcripts", 0.9119148086319139, 0.016500002926406746, 0.1,                  0,
   "Race",   "Unredacted", "Both",        0.8824810770889197, 0.014845147402024243, 0.021544346900318822, 0.2222222222222222,
   "Gender", "Redacted",   "Resume",      0.8071356680898996, 0.01795216999721258,  10,                   0,
   "Gender", "Redacted",   "Transcripts", 0.8612161314012277, 0.01428162663704173,  0.021544346900318822, 0.2222222222222222,
   "Gender", "Redacted",   "Both",        0.8620700817125236, 0.018037783688257644, 1e-5,                 0.1111111111111111,
   "Gender", "Unredacted", "Resume",      0.8456370510276761, 0.013580350079093103, 0.021544346900318822, 1,
   "Gender", "Unredacted", "Transcripts", 0.9010738588827437, 0.018275703471780546, 0.1,                  0.1111111111111111,
   "Gender", "Unredacted", "Both",        0.9064368268711409, 0.01191274385520321,  0.1,                  0.1111111111111111
  ) %>%
  mutate(features = factor(
    features,
    levels = c("Resume", "Transcripts", "Both")
  )) %>%
  ggplot(aes(x = features, y = mean, fill = status)) +
  geom_hline(yintercept = 0.9, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    aes(ymin = mean - 1.96 * std_err, ymax = mean + 1.96 * std_err),
    width = 0.25,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = "Features",
    y = "AUC",
    fill = "Status"
  ) +
  facet_wrap(vars(outcome)) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot2::ggsave(
  path_join(c("plots", "auc.pdf")),
  plot = p_auc,
  width = 6,
  height = 4
)
