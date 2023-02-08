
suppressPackageStartupMessages(devtools::load_all())
message("Processing Verbs...")

verbs_to_do_raw <- read_excel("~/OneDrive/Greek/greek_to_do.xlsx",
                              sheet = "verbs")

verbs_to_do <- verbs_to_do_raw |>
  clean_names() |>
  nest(tenses = past_simple:future_perfect) |>
  mutate(tenses = map(tenses, unlist))

suppressWarnings(
  verbs_table <- list(english_verb = verbs_to_do$english_verb,
                      greek_verb = verbs_to_do$greek_verb,
                      english_verb_tenses = verbs_to_do$tenses) |>
    pmap_dfr(possibly(get_verb_table, otherwise = NULL))
)


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

to_do <- verbs_to_do$english_verb
done <- verbs_table_present$english_verb

verbs_table_error <- tibble(english_verb = to_do[!(to_do %in% done)]) |>
  left_join(verbs_to_do_raw, by = "english_verb") |>
  distinct(.keep_all = TRUE)

# User message

summary_message <- glue("Complete: {nrow(verbs_table_present)}\n",
                        "Error: {nrow(verbs_table_error)}")

print(summary_message)
View(verbs_table_present)

# Output

filenames <- glue('{file.path(here::here(), "output")}/',
                  'verbs_{Sys.Date()}_{c("present", "past_future", "perfect", "error")}.csv')

write_csv(verbs_table_present, filenames[1], col_names = FALSE)
write_csv(verbs_table_past_future, filenames[2], col_names = FALSE)
write_csv(verbs_table_perfect, filenames[3], col_names = FALSE)
write_csv(verbs_table_error, filenames[4], col_names = FALSE)

