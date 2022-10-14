
# Process Nouns

# This script takes the nouns to do list from OneDrive and looks up their
# declension table on Greek wiktionary.

# ------------------------------------------------------------------------------

# Setup

suppressPackageStartupMessages(devtools::load_all())
message("Processing Nouns...")

# Nouns are updated on OneDrive excel sheet after lessons

nouns_to_do <- read_excel("~/OneDrive/Greek/greek_to_do.xlsx", sheet = "nouns")

## To Do - need to adapt the code to take account of nouns which have multiple
## genders

nouns_table <- map2(nouns_to_do$english_noun,
                    nouns_to_do$greek_noun,
                    safely(get_noun_table))

closeAllConnections()

# Split the nouns by gender because Anki cards are split by gender

nouns_table_m <- nouns_table |> filter(gender == "m") |> select(-gender)
nouns_table_f <- nouns_table |> filter(gender == "f") |> select(-gender)
nouns_table_n <- nouns_table |> filter(gender == "n") |> select(-gender)
nouns_table_mf <- nouns_table |> filter(gender == "mf") |> select(-gender)

# Let the user know how many of each - if none then no need to upload. Some may
# not have been successful

to_do <- nouns_to_do$english_noun
done <- nouns_table$english_noun

incomplete <- tibble(english_noun = to_do[!(to_do %in% done)]) |>
  left_join(nouns_to_do, by = "english_noun")

# User message

summary_message <- glue("Masculine: {nrow(nouns_table_m)}\n",
                        "Feminine: {nrow(nouns_table_f)}\n",
                        "Neutral: {nrow(nouns_table_n)}\n",
                        "Masculine/Feminine: {nrow(nouns_table_mf)}\n",
                        "Error: {nrow(incomplete)}")

print(summary_message)
View(nouns_table)

# Write files with date for upload to anki - col names not wanted for anki
# And write the incomplete ones too

filenames <- glue('{file.path(here::here(), "output")}/',
                  'nouns_{Sys.Date()}_{c("m", "f", "n", "mf", "error")}.csv')

write_csv(nouns_table_m, filenames[1], col_names = FALSE)
write_csv(nouns_table_f, filenames[2], col_names = FALSE)
write_csv(nouns_table_n, filenames[3], col_names = FALSE)
write_csv(nouns_table_mf, filenames[4], col_names = FALSE)

write_csv(incomplete, filenames[5])
