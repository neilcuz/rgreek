
# Process Nouns

# This script takes the nouns to do list from OneDrive and looks up their
# declension table on Greek wiktionary.

# ------------------------------------------------------------------------------

# Setup

suppressPackageStartupMessages(devtools::load_all())
message("Processing Nouns...")

# Nouns are updated on OneDrive excel sheet after lessons

nouns_to_do <- read_excel("~/OneDrive/Greek/greek_to_do.xlsx", sheet = "nouns")

# Loop through each noun and get the declension table

nouns_table <- vector("list", length = nrow(nouns_to_do))

## To Do - need to adapt the code to take account of nouns which have multiple
## genders

get_noun_table_safe <- safely(get_noun_table)

nouns_table <- map2(nouns_to_do$english_noun,
                    nouns_to_do$greek_noun,
                    get_noun_table_safe)

results <- map_dfr(nouns_table, "result")

results <- map(nouns_table, "error")


for (i in 1:nrow(nouns_to_do)) {

  english_noun <- unlist(nouns_to_do[i, "english_noun"])
  greek_noun <- unlist(nouns_to_do[i, "greek_noun"])

  nouns_table[[i]] <- safe_get_noun_table(english_noun, greek_noun)

  if (i %% 10 == 0) {

    print(i)

  }

  # Dont want to smash their servers

  Sys.sleep(0.25)

}

closeAllConnections()

nouns_table <- map_dfr(nouns_table, "result")

# Split the nouns by gender because Anki cards are split by gender

nouns_table_m <- nouns_table |> filter(gender == "m") |> select(-gender)
nouns_table_f <- nouns_table |> filter(gender == "f") |> select(-gender)
nouns_table_n <- nouns_table |> filter(gender == "n") |> select(-gender)

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
                        "Error: {nrow(incomplete)}")

print(summary_message)
View(nouns_table)

# Write files with date for upload to anki - col names not wanted for anki
# And write the incomplete ones too

filenames <- glue('{file.path(here::here(), "output")}/',
                  'nouns_{Sys.Date()}_{c("m", "f", "n", "error")}.csv')

write_csv(nouns_table_m, filenames[1], col_names = FALSE)
write_csv(nouns_table_f, filenames[2], col_names = FALSE)
write_csv(nouns_table_n, filenames[3], col_names = FALSE)

write_csv(incomplete, filenames[4])
