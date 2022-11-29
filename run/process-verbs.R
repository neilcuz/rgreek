
suppressPackageStartupMessages(devtools::load_all())
message("Processing Verbs...")

verbs_to_do <- "~/OneDrive/Greek/greek_to_do.xlsx" |>
  read_excel(sheet = "verbs") |>
  clean_names() |>
  nest(tenses = past_simple:future_perfect) |>
  mutate(tenses = map(tenses, unlist))

verbs_table <- list(english_verb = verbs_to_do$english_verb,
     greek_verb = verbs_to_do$greek_verb,
     english_verb_tenses = verbs_to_do$tenses) |>
  pmap_dfr(possibly(get_verb_table, otherwise = NULL))

suppressWarnings(closeAllConnections())

# Split the verbs into present tense; past and future tenses simple and
# continuous; perfect tenses

verbs_table_present <- verbs_table |>
  filter(tense == "present") |>
  select(-tense)

verbs_table_past_future <- verbs_table |>
  filter(tense %in% c("past", "future", "past cont", "future cont")) |>
  select(-tense)

verbs_table_perfect <- verbs_table |>
  filter(str_detect(tense, "perfect")) |>
  select(-tense)

# Add code for error



# Output

filenames <- glue('{file.path(here::here(), "output")}/',
                  'verbs_{Sys.Date()}_{c("present", "past_future", "perfect", "error")}.csv')

write_csv(verbs_table_present, filenames[1], col_names = FALSE)
write_csv(verbs_table_past_future, filenames[2], col_names = FALSE)
write_csv(verbs_table_perfect, filenames[3], col_names = FALSE)
write_csv(verbs_table_error, filenames[4], col_names = FALSE)

write_csv(incomplete, filenames[5])

